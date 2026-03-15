import Foundation
import Combine

@MainActor
final class GravityMonitor: ObservableObject {
    @Published var projects: [ProjectInfo] = []
    @Published var inboxItems: [InboxItemInfo] = []
    @Published var connected = false

    /// Tracked sessions from session.snapshot messages (keyed by sessionId)
    private var sessionMap: [String: SessionJSON] = [:]

    var activeCount: Int {
        projects.flatMap(\.sessions).filter { $0.status == "active" }.count
    }

    var hasResponding: Bool {
        projects.flatMap(\.sessions).contains { $0.claudeStatus == "responding" }
    }

    private var connection: SocketConnection?
    private var reconnectTask: Task<Void, Never>?

    private let socketPath: String

    init() {
        if let envPath = ProcessInfo.processInfo.environment["GRAVITY_TERMINAL_SOCK"] {
            socketPath = envPath
        } else {
            socketPath = NSString("~/.local/state/gravity-terminal.sock").expandingTildeInPath
        }
        startConnection()
    }

    deinit {
        reconnectTask?.cancel()
    }

    private func startConnection() {
        connection = SocketConnection(path: socketPath) { [weak self] line in
            Task { @MainActor in
                self?.handleMessage(line)
            }
        } onDisconnect: { [weak self] in
            Task { @MainActor in
                self?.handleDisconnect()
            }
        }
        connection?.connect()
    }

    private func handleDisconnect() {
        connected = false
        projects = []
        inboxItems = []
        sessionMap = [:]
        scheduleReconnect()
    }

    private func scheduleReconnect() {
        reconnectTask?.cancel()
        reconnectTask = Task { [weak self] in
            try? await Task.sleep(for: .seconds(3))
            guard !Task.isCancelled else { return }
            self?.startConnection()
        }
    }

    private func handleMessage(_ line: String) {
        guard let data = line.data(using: .utf8) else { return }

        // Parse envelope to get message type
        guard let envelope = try? JSONDecoder().decode(MessageEnvelope.self, from: data) else { return }

        if !connected {
            connected = true
            // Request overview on first connect
            sendRequest("""
            {"type":"request.overview"}
            """)
        }

        switch envelope.type {
        case "overview.snapshot":
            guard let msg = try? JSONDecoder().decode(OverviewSnapshot.self, from: data) else { return }
            applyOverview(msg)

        case "inbox.added":
            guard let msg = try? JSONDecoder().decode(InboxAdded.self, from: data) else { return }
            let item = msg.item
            if !inboxItems.contains(where: { $0.id == item.id }) {
                inboxItems.append(InboxItemInfo(
                    id: item.id, type: item.type, sessionId: item.sessionId,
                    project: item.project, label: item.label, summary: item.summary
                ))
            }

        case "inbox.removed":
            guard let msg = try? JSONDecoder().decode(InboxRemoved.self, from: data) else { return }
            inboxItems.removeAll { $0.id == msg.itemId }

        case "inbox.snapshot":
            guard let msg = try? JSONDecoder().decode(InboxSnapshot.self, from: data) else { return }
            inboxItems = msg.items.map {
                InboxItemInfo(id: $0.id, type: $0.type, sessionId: $0.sessionId,
                              project: $0.project, label: $0.label, summary: $0.summary)
            }

        case "session.snapshot":
            guard let msg = try? JSONDecoder().decode(SessionSnapshot.self, from: data) else { return }
            sessionMap[msg.sessionId] = msg.session
            rebuildFromSessionMap()

        case "session.removed":
            guard let msg = try? JSONDecoder().decode(SessionRemoved.self, from: data) else { return }
            sessionMap.removeValue(forKey: msg.sessionId)
            rebuildFromSessionMap()

        case "session.update":
            guard let msg = try? JSONDecoder().decode(SessionUpdate.self, from: data) else { return }
            applySessionPatches(sessionId: msg.sessionId, patches: msg.patches)

        default:
            break
        }
    }

    private func applyOverview(_ msg: OverviewSnapshot) {
        projects = msg.projects.map { p in
            ProjectInfo(
                id: p.project,
                name: p.project,
                sessions: p.sessions.map { s in
                    SessionInfo(
                        id: s.sessionId, slug: s.slug, status: s.status,
                        claudeStatus: s.claudeStatus, toolCount: s.toolCount,
                        lastEventTime: s.lastEventTime
                    )
                }
            )
        }
    }

    private func rebuildFromSessionMap() {
        // Group sessions by project
        var grouped: [String: [SessionJSON]] = [:]
        for session in sessionMap.values {
            grouped[session.project, default: []].append(session)
        }
        projects = grouped.sorted(by: { $0.key < $1.key }).map { (name, sessions) in
            ProjectInfo(
                id: name,
                name: name,
                sessions: sessions.map { s in
                    SessionInfo(
                        id: s.sessionId, slug: s.slug, status: s.status,
                        claudeStatus: s.claudeStatus, toolCount: s.totalToolCount,
                        lastEventTime: s.lastEventTime
                    )
                }
            )
        }
    }

    private func applySessionPatches(sessionId: String, patches: [PatchJSON]) {
        for patch in patches {
            switch patch.op {
            case "set_status":
                if let status = patch.status {
                    updateSession(sessionId) { s in
                        s.status = status
                    }
                }
            case "set_claude_status":
                if let cs = patch.claudeStatus {
                    updateSession(sessionId) { s in
                        s.claudeStatus = cs
                    }
                }
            default:
                break // Only status/claudeStatus matter for menu bar display
            }
        }
    }

    /// Mutate a session in-place within the projects array
    private func updateSession(_ sessionId: String, _ mutate: (inout SessionInfo) -> Void) {
        for pi in projects.indices {
            for si in projects[pi].sessions.indices {
                if projects[pi].sessions[si].id == sessionId {
                    mutate(&projects[pi].sessions[si])
                    return
                }
            }
        }
    }

    private func sendRequest(_ json: String) {
        connection?.send(json + "\n")
    }
}
