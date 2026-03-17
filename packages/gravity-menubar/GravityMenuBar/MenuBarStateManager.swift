import Foundation

/// Pure state machine for menu bar icon and session state.
/// No I/O — all side effects expressed as `pendingRequests`.
public class MenuBarStateManager {
    // MARK: - Published state (read by UI via GravityMonitor)

    public private(set) var connected = false
    public private(set) var projects: [ProjectInfo] = []
    public private(set) var inboxItems: [InboxInfo] = []
    public private(set) var iconState: MenuBarIconState = .disconnected

    // MARK: - Internal state (visible for testing via @testable import)

    /// Last-known claudeStatus per session for transition detection
    public var previousStatuses: [String: String] = [:]
    public var justFinished = false
    public var hasResponding = false

    /// Timestamp when justFinished was last set to true
    public var justFinishedAt: Date?

    /// Side-effect queue: requests to send to gravity-server
    public var pendingRequests: [TerminalRequest] = []

    /// Callback fired after any state mutation (for GravityMonitor to sync @Published)
    public var onStateChange: (() -> Void)?

    public init() {}

    // MARK: - Icon state priority

    public func updateIconState() {
        let newState: MenuBarIconState
        if !connected { newState = .disconnected }
        else if justFinished { newState = .justFinished }
        else if !inboxItems.isEmpty { newState = .attention }
        else if hasResponding { newState = .responding }
        else { newState = .neutral }
        if iconState != newState {
            iconState = newState
        }
    }

    // MARK: - Connection lifecycle

    public func setConnected(_ value: Bool) {
        connected = value
        updateIconState()
        onStateChange?()
    }

    public func resetOnDisconnect() {
        connected = false
        projects = []
        inboxItems = []
        justFinished = false
        justFinishedAt = nil
        hasResponding = false
        previousStatuses = [:]
        updateIconState()
        onStateChange?()
    }

    // MARK: - Message handling

    public func handleMessage(_ msg: ServerMessage) {
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
            // Seed previousStatuses from snapshot (no transition on initial load)
            var activeSessionIds = Set<String>()
            for p in jsonProjects {
                for s in p.sessions where s.status == "active" {
                    previousStatuses[s.sessionId] = s.claudeStatus
                    activeSessionIds.insert(s.sessionId)
                }
            }
            // Clean stale entries for sessions no longer in snapshot
            for key in previousStatuses.keys where !activeSessionIds.contains(key) {
                previousStatuses.removeValue(forKey: key)
            }
            // Bug fix #1: update hasResponding from snapshot
            hasResponding = previousStatuses.values.contains("responding")
            updateIconState()

        case "inbox.added":
            guard let item = msg.item else { return }
            // Skip informational "idle" items — not actionable
            guard item.type != "idle" else { return }
            inboxItems.removeAll { $0.id == item.id }
            inboxItems.append(InboxInfo(
                id: item.id,
                type: item.type,
                sessionId: item.sessionId,
                project: item.project,
                label: item.label,
                summary: item.summary
            ))
            updateIconState()

        case "inbox.removed":
            guard let itemId = msg.itemId else { return }
            inboxItems.removeAll { $0.id == itemId }
            updateIconState()

        case "inbox.snapshot":
            guard let items = msg.items else { return }
            // Filter out informational "idle" items — not actionable
            inboxItems = items.filter { $0.type != "idle" }.map { item in
                InboxInfo(
                    id: item.id,
                    type: item.type,
                    sessionId: item.sessionId,
                    project: item.project,
                    label: item.label,
                    summary: item.summary
                )
            }
            updateIconState()

        case "session.update":
            if let sessionId = msg.sessionId, let patches = msg.patches {
                var hasStatusChange = false
                for patch in patches {
                    if patch.op == "set_claude_status", let newStatus = patch.claudeStatus {
                        let oldStatus = previousStatuses[sessionId]
                        previousStatuses[sessionId] = newStatus
                        if oldStatus == "responding" && newStatus == "idle" {
                            justFinished = true
                            justFinishedAt = Date()
                        } else if newStatus == "responding" {
                            justFinished = false
                            justFinishedAt = nil
                        }
                        hasResponding = previousStatuses.values.contains("responding")
                        hasStatusChange = true
                    } else if patch.op == "set_status" {
                        hasStatusChange = true
                    }
                }
                if hasStatusChange {
                    updateIconState()
                    pendingRequests.append(TerminalRequest(type: "request.overview"))
                }
            }

        case "session.removed":
            if let sessionId = msg.sessionId {
                previousStatuses.removeValue(forKey: sessionId)
                // Bug fix #3: recompute hasResponding after removal
                hasResponding = previousStatuses.values.contains("responding")
                updateIconState()
            }
            pendingRequests.append(TerminalRequest(type: "request.overview"))

        default:
            break
        }
        onStateChange?()
    }

    // MARK: - Timeout support

    /// Clear justFinished if it's been set for longer than the given interval.
    /// Call this from a timer to prevent indefinite green flash.
    public func clearJustFinishedIfStale(after interval: TimeInterval = 3.0) {
        guard justFinished, let setAt = justFinishedAt else { return }
        if Date().timeIntervalSince(setAt) >= interval {
            justFinished = false
            justFinishedAt = nil
            updateIconState()
            onStateChange?()
        }
    }
}
