import Foundation
import Combine

/// Connects to gravity-server's terminal socket, receives NDJSON broadcasts,
/// and publishes state for the SwiftUI menu bar.
class GravityMonitor: ObservableObject {
    @Published var connected = false
    @Published var projects: [ProjectInfo] = []
    @Published var inboxItems: [InboxInfo] = []

    var activeCount: Int {
        projects.flatMap(\.sessions).filter { $0.status == "active" }.count
    }

    var hasResponding: Bool {
        projects.flatMap(\.sessions).contains { $0.claudeStatus == "responding" }
    }

    /// One dot per active session: .green (idle), .yellow (responding), .orange (waiting/inbox)
    var sessionDots: [DotStatus] {
        let waitingIds = Set(inboxItems.map(\.sessionId))
        return projects.flatMap(\.sessions)
            .filter { $0.status == "active" }
            .map { session -> DotStatus in
                if waitingIds.contains(session.id) { return .waiting }
                if session.claudeStatus == "responding" { return .responding }
                return .idle
            }
            .sorted { $0.priority > $1.priority }
    }

    private var socketFD: Int32 = -1
    private var readSource: DispatchSourceRead?
    private var reconnectTimer: Timer?
    private var buffer = Data()

    private let socketPath: String

    init() {
        if let envPath = ProcessInfo.processInfo.environment["GRAVITY_TERMINAL_SOCK"] {
            socketPath = envPath
        } else {
            socketPath = NSString(string: "~/.local/state/gravity-terminal.sock").expandingTildeInPath
        }
        connect()
    }

    deinit {
        disconnect()
    }

    // MARK: - Connection

    func reconnect() {
        disconnect()
        connect()
    }

    private func connect() {
        socketFD = Darwin.socket(AF_UNIX, SOCK_STREAM, 0)
        guard socketFD >= 0 else {
            scheduleReconnect()
            return
        }

        var addr = sockaddr_un()
        addr.sun_family = sa_family_t(AF_UNIX)

        let pathBytes = socketPath.utf8CString
        guard pathBytes.count <= MemoryLayout.size(ofValue: addr.sun_path) else {
            NSLog("gravity-menubar: socket path too long")
            Darwin.close(socketFD)
            socketFD = -1
            return
        }

        withUnsafeMutablePointer(to: &addr.sun_path) { sunPathPtr in
            sunPathPtr.withMemoryRebound(to: CChar.self, capacity: pathBytes.count) { dest in
                pathBytes.withUnsafeBufferPointer { src in
                    _ = memcpy(dest, src.baseAddress!, pathBytes.count)
                }
            }
        }

        let addrLen = socklen_t(MemoryLayout<sockaddr_un>.size)
        let result = withUnsafePointer(to: &addr) { addrPtr in
            addrPtr.withMemoryRebound(to: sockaddr.self, capacity: 1) { sockaddrPtr in
                Darwin.connect(socketFD, sockaddrPtr, addrLen)
            }
        }

        guard result == 0 else {
            Darwin.close(socketFD)
            socketFD = -1
            scheduleReconnect()
            return
        }

        DispatchQueue.main.async {
            self.connected = true
        }

        startReading()
        sendRequest(TerminalRequest(type: "request.overview"))
        sendRequest(TerminalRequest(type: "request.resync"))
    }

    private func disconnect() {
        reconnectTimer?.invalidate()
        reconnectTimer = nil
        readSource?.cancel()
        readSource = nil
        if socketFD >= 0 {
            Darwin.close(socketFD)
            socketFD = -1
        }
        DispatchQueue.main.async {
            self.connected = false
            self.projects = []
            self.inboxItems = []
        }
    }

    private func scheduleReconnect() {
        DispatchQueue.main.async {
            self.connected = false
            self.reconnectTimer?.invalidate()
            self.reconnectTimer = Timer.scheduledTimer(withTimeInterval: 5.0, repeats: false) { [weak self] _ in
                self?.connect()
            }
        }
    }

    // MARK: - Reading

    private func startReading() {
        let source = DispatchSource.makeReadSource(fileDescriptor: socketFD, queue: .global(qos: .utility))
        source.setEventHandler { [weak self] in
            self?.readAvailableData()
        }
        source.setCancelHandler { [weak self] in
            guard let self = self else { return }
            if self.socketFD >= 0 {
                Darwin.close(self.socketFD)
                self.socketFD = -1
            }
        }
        readSource = source
        source.resume()
    }

    private func readAvailableData() {
        var buf = [UInt8](repeating: 0, count: 8192)
        let n = Darwin.read(socketFD, &buf, buf.count)
        if n <= 0 {
            // Connection closed or error
            readSource?.cancel()
            readSource = nil
            scheduleReconnect()
            return
        }

        buffer.append(contentsOf: buf[0..<n])
        processBuffer()
    }

    private func processBuffer() {
        // Split on newlines (NDJSON)
        while let newlineIndex = buffer.firstIndex(of: UInt8(ascii: "\n")) {
            let lineData = buffer[buffer.startIndex..<newlineIndex]
            buffer = Data(buffer[buffer.index(after: newlineIndex)...])

            guard !lineData.isEmpty else { continue }

            do {
                let msg = try JSONDecoder().decode(ServerMessage.self, from: lineData)
                DispatchQueue.main.async {
                    self.handleMessage(msg)
                }
            } catch {
                NSLog("gravity-menubar: JSON decode error: \(error)")
            }
        }
    }

    // MARK: - Message Handling

    private func handleMessage(_ msg: ServerMessage) {
        switch msg.type {
        case "overview.snapshot":
            guard let jsonProjects = msg.projects else { return }
            projects = jsonProjects.map { p in
                ProjectInfo(
                    id: p.project,
                    name: p.project,
                    sessions: p.sessions.map { s in
                        SessionInfo(
                            id: s.sessionId,
                            slug: s.slug,
                            status: s.status,
                            claudeStatus: s.claudeStatus,
                            toolCount: s.toolCount,
                            lastEventTime: s.lastEventTime
                        )
                    }
                )
            }

        case "inbox.added":
            guard let item = msg.item else { return }
            inboxItems.removeAll { $0.id == item.id }
            inboxItems.append(InboxInfo(
                id: item.id,
                type: item.type,
                sessionId: item.sessionId,
                project: item.project,
                label: item.label,
                summary: item.summary
            ))

        case "inbox.removed":
            guard let itemId = msg.itemId else { return }
            inboxItems.removeAll { $0.id == itemId }

        case "inbox.snapshot":
            guard let items = msg.items else { return }
            inboxItems = items.map { item in
                InboxInfo(
                    id: item.id,
                    type: item.type,
                    sessionId: item.sessionId,
                    project: item.project,
                    label: item.label,
                    summary: item.summary
                )
            }

        case "session.update":
            // For status changes, request a fresh overview
            if let patches = msg.patches {
                let hasStatusChange = patches.contains { p in
                    p.op == "set_status" || p.op == "set_claude_status"
                }
                if hasStatusChange {
                    sendRequest(TerminalRequest(type: "request.overview"))
                }
            }

        case "session.removed":
            sendRequest(TerminalRequest(type: "request.overview"))

        default:
            break
        }
    }

    // MARK: - Writing

    private func sendRequest(_ request: TerminalRequest) {
        guard socketFD >= 0 else { return }
        do {
            var data = try JSONEncoder().encode(request)
            data.append(UInt8(ascii: "\n"))
            data.withUnsafeBytes { ptr in
                _ = Darwin.write(socketFD, ptr.baseAddress!, ptr.count)
            }
        } catch {
            NSLog("gravity-menubar: encode error: \(error)")
        }
    }
}
