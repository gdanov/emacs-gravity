import Foundation

/// Pure state machine for menu bar icon and session state.
/// No I/O — all side effects expressed as `pendingRequests`.
public class MenuBarStateManager {
    // MARK: - Published state (read by UI via GravityMonitor)

    public private(set) var connected = false
    public private(set) var projects: [ProjectInfo] = []
    public private(set) var inboxItems: [InboxInfo] = []
    public private(set) var iconState: MenuBarIconState = .disconnected
    public private(set) var noticeText: String? = nil

    // MARK: - Internal state (visible for testing via @testable import)

    /// Last-known claudeStatus per session for transition detection
    public var previousStatuses: [String: String] = [:]
    public var justFinished = false
    public var hasResponding = false

    /// Side-effect queue: requests to send to gravity-server
    public var pendingRequests: [TerminalRequest] = []

    /// Callback fired after any state mutation (for GravityMonitor to sync @Published)
    public var onStateChange: (() -> Void)?

    public init() {}

    // MARK: - Icon state priority

    public func updateIconState() {
        let newState: MenuBarIconState
        if !connected { newState = .disconnected }
        else if !inboxItems.isEmpty || noticeText != nil { newState = .attention }
        else if justFinished { newState = .justFinished }
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
            // Clear notice when sessions start appearing
            if jsonProjects.contains(where: { !$0.sessions.isEmpty }) {
                noticeText = nil
            }
            projects = jsonProjects.map { p in
                ProjectInfo(
                    id: p.project,
                    name: p.project,
                    sessions: p.sessions.map { s in
                        SessionInfo(
                            id: s.sessionId,
                            slug: s.slug,
                            serverDisplayName: s.displayName,
                            status: s.status,
                            claudeStatus: s.claudeStatus,
                            toolCount: s.toolCount,
                            lastEventTime: s.lastEventTime,
                            latestMessage: s.latestMessage,
                            latestUserPrompt: s.latestUserPrompt
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
                var addToolCount = 0
                var sessionFound = true

                for patch in patches {
                    switch patch.op {
                    case "set_claude_status":
                        if let newStatus = patch.claudeStatus {
                            let oldStatus = previousStatuses[sessionId]
                            previousStatuses[sessionId] = newStatus
                            if oldStatus == "responding" && newStatus == "idle" {
                                justFinished = true
                            } else if newStatus == "responding" {
                                justFinished = false
                            }
                            hasResponding = previousStatuses.values.contains("responding")
                            hasStatusChange = true
                            // Apply to projects model
                            if !mutateSession(sessionId, { $0.claudeStatus = newStatus }) {
                                sessionFound = false
                            }
                        }
                    case "set_status":
                        if let newStatus = patch.status {
                            hasStatusChange = true
                            if !mutateSession(sessionId, { $0.status = newStatus }) {
                                sessionFound = false
                            }
                        } else {
                            hasStatusChange = true
                        }
                    case "add_tool":
                        addToolCount += 1
                    case "set_meta":
                        if let slug = patch.slug {
                            if !mutateSession(sessionId, { $0.slug = slug }) {
                                sessionFound = false
                            }
                        }
                        if let dn = patch.displayName {
                            _ = mutateSession(sessionId, { $0.serverDisplayName = dn })
                        }
                    case "set_turn_stop":
                        if let stopText = patch.stopText {
                            _ = mutateSession(sessionId, { $0.latestMessage = stopText })
                        }
                    case "set_streaming_text":
                        // text may be nil (clearing streaming), which is fine
                        _ = mutateSession(sessionId, { $0.latestMessage = patch.text })
                    case "add_prompt":
                        if let promptText = patch.prompt?.text {
                            _ = mutateSession(sessionId, { $0.latestUserPrompt = promptText })
                        }
                    default:
                        break
                    }
                }

                // Increment tool count in projects model
                if addToolCount > 0 {
                    if !mutateSession(sessionId, { $0.toolCount += addToolCount }) {
                        sessionFound = false
                    }
                }

                // Update lastEventTime for any patch activity
                if !patches.isEmpty {
                    let now = Date().timeIntervalSince1970
                    if !mutateSession(sessionId, { $0.lastEventTime = now }) {
                        sessionFound = false
                    }
                }

                // If session not found in projects, request a full overview (new session race)
                if !sessionFound {
                    pendingRequests.append(TerminalRequest(type: "request.overview"))
                }

                if hasStatusChange {
                    updateIconState()
                    // Also request overview as consistency fallback for status changes
                    pendingRequests.append(TerminalRequest(type: "request.overview"))
                } else if addToolCount > 0 {
                    // Tool count changes update the display but don't need overview
                    updateIconState()
                }
            }

        case "session.snapshot":
            // A new session appeared — request overview to get it into projects
            pendingRequests.append(TerminalRequest(type: "request.overview"))

        case "session.removed":
            if let sessionId = msg.sessionId {
                previousStatuses.removeValue(forKey: sessionId)
                // Bug fix #3: recompute hasResponding after removal
                hasResponding = previousStatuses.values.contains("responding")
                updateIconState()
            }
            pendingRequests.append(TerminalRequest(type: "request.overview"))

        case "notice":
            noticeText = msg.text
            updateIconState()

        default:
            break
        }
        onStateChange?()
    }

    // MARK: - Session mutation helper

    /// Find a session by ID across all projects and mutate it in place.
    /// Returns true if the session was found and mutated.
    @discardableResult
    public func mutateSession(_ sessionId: String, _ body: (inout SessionInfo) -> Void) -> Bool {
        for pi in projects.indices {
            for si in projects[pi].sessions.indices {
                if projects[pi].sessions[si].id == sessionId {
                    body(&projects[pi].sessions[si])
                    return true
                }
            }
        }
        return false
    }

    /// Inbox items for a specific session.
    public func inboxItems(for sessionId: String) -> [InboxInfo] {
        inboxItems.filter { $0.sessionId == sessionId }
    }

}
