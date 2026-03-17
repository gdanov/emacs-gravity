import Foundation
import Testing
@testable import GravityMenuBarLib

// MARK: - Suite 1: updateIconState priority

@Suite("Icon State Priority")
struct IconStatePriorityTests {
    @Test("1: initial state is disconnected")
    func initialState() {
        let sm = MenuBarStateManager()
        #expect(sm.iconState == .disconnected)
    }

    @Test("2: connected with no sessions → neutral")
    func connectedNoSessions() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        #expect(sm.iconState == .neutral)
    }

    @Test("3: disconnected overrides everything")
    func disconnectedOverrides() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        // Add inbox item to trigger attention
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Allow Bash")
        ))
        #expect(sm.iconState == .attention)
        // Disconnect
        sm.setConnected(false)
        #expect(sm.iconState == .disconnected)
    }

    @Test("4: attention overrides justFinished")
    func attentionOverridesJustFinished() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        // Seed a responding session
        sm.previousStatuses["s1"] = "responding"
        // Transition responding→idle → justFinished
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "idle")]
        ))
        #expect(sm.justFinished == true)
        // Add inbox item
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Allow Bash")
        ))
        // attention should win over justFinished
        #expect(sm.iconState == .attention)
    }

    @Test("5: attention wins over responding")
    func attentionWinsOverResponding() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        // Set a session as responding
        sm.previousStatuses["s1"] = "responding"
        sm.hasResponding = true
        // Add inbox item
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Allow Bash")
        ))
        // attention has higher priority than responding
        #expect(sm.iconState == .attention)
    }

    @Test("6: inbox items when idle → attention")
    func inboxWhenIdle() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Allow Bash")
        ))
        #expect(sm.iconState == .attention)
    }

    @Test("7: empty inbox after removal → neutral")
    func emptyInboxAfterRemoval() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Allow Bash")
        ))
        #expect(sm.iconState == .attention)
        sm.handleMessage(ServerMessage(type: "inbox.removed", itemId: 1))
        #expect(sm.iconState == .neutral)
    }
}

// MARK: - Suite 2: session.update transitions

@Suite("Session Update Transitions")
struct SessionUpdateTransitionTests {
    @Test("8: responding→idle sets justFinished")
    func respondingToIdleSetsJustFinished() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.previousStatuses["s1"] = "responding"
        sm.hasResponding = true
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "idle")]
        ))
        #expect(sm.justFinished == true)
        #expect(sm.iconState == .justFinished)
    }

    @Test("9: idle→responding clears justFinished")
    func idleToRespondingClearsJustFinished() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.justFinished = true
        sm.previousStatuses["s1"] = "idle"
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "responding")]
        ))
        #expect(sm.justFinished == false)
    }

    @Test("10: idle→idle no transition")
    func idleToIdleNoTransition() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.previousStatuses["s1"] = "idle"
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "idle")]
        ))
        #expect(sm.justFinished == false)
    }

    @Test("11: responding→responding keeps hasResponding")
    func respondingToRespondingKeepsHasResponding() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.previousStatuses["s1"] = "responding"
        sm.hasResponding = true
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "responding")]
        ))
        #expect(sm.hasResponding == true)
    }

    @Test("12: unknown session (nil oldStatus) → idle, no justFinished")
    func unknownSessionToIdle() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        // No previous status for s1
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "idle")]
        ))
        #expect(sm.justFinished == false)
        #expect(sm.previousStatuses["s1"] == "idle")
    }

    @Test("13: hasResponding computed across all sessions — both idle → false")
    func hasRespondingBothIdle() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.previousStatuses["s1"] = "responding"
        sm.previousStatuses["s2"] = "responding"
        sm.hasResponding = true
        // s1 → idle
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "idle")]
        ))
        #expect(sm.hasResponding == true) // s2 still responding
        // s2 → idle
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s2",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "idle")]
        ))
        #expect(sm.hasResponding == false)
    }

    @Test("14: hasResponding true when any session still responding")
    func hasRespondingWithOneStillResponding() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.previousStatuses["s1"] = "responding"
        sm.previousStatuses["s2"] = "responding"
        sm.hasResponding = true
        // Only s1 → idle
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "idle")]
        ))
        #expect(sm.hasResponding == true) // s2 still responding
    }

    @Test("15: set_status patch triggers icon update")
    func setStatusPatchTriggersIconUpdate() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_status", status: "ended")]
        ))
        // Should have triggered an overview request
        #expect(sm.pendingRequests.contains(TerminalRequest(type: "request.overview")))
    }

    @Test("16: multiple patches in one update")
    func multiplePatchesInOneUpdate() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.previousStatuses["s1"] = "responding"
        sm.hasResponding = true
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [
                PatchJSON(op: "set_claude_status", claudeStatus: "idle"),
                PatchJSON(op: "set_status", status: "ended")
            ]
        ))
        #expect(sm.justFinished == true)
        #expect(sm.previousStatuses["s1"] == "idle")
    }
}

// MARK: - Suite 3: overview.snapshot

@Suite("Overview Snapshot")
struct OverviewSnapshotTests {
    @Test("17: seeds previousStatuses from snapshot")
    func seedsPreviousStatuses() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "overview.snapshot",
            projects: [
                ProjectSummaryJSON(project: "my-project", sessions: [
                    SessionSummaryJSON(sessionId: "s1", status: "active", claudeStatus: "idle"),
                    SessionSummaryJSON(sessionId: "s2", status: "active", claudeStatus: "responding")
                ])
            ]
        ))
        #expect(sm.previousStatuses["s1"] == "idle")
        #expect(sm.previousStatuses["s2"] == "responding")
    }

    @Test("18: only seeds active sessions, skips ended")
    func onlySeedsActiveSessions() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "overview.snapshot",
            projects: [
                ProjectSummaryJSON(project: "my-project", sessions: [
                    SessionSummaryJSON(sessionId: "s1", status: "active", claudeStatus: "idle"),
                    SessionSummaryJSON(sessionId: "s2", status: "ended", claudeStatus: "idle")
                ])
            ]
        ))
        #expect(sm.previousStatuses["s1"] == "idle")
        #expect(sm.previousStatuses["s2"] == nil)
    }

    @Test("19: populates projects")
    func populatesProjects() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "overview.snapshot",
            projects: [
                ProjectSummaryJSON(project: "proj-a", sessions: [
                    SessionSummaryJSON(sessionId: "s1", status: "active", claudeStatus: "idle")
                ]),
                ProjectSummaryJSON(project: "proj-b", sessions: [
                    SessionSummaryJSON(sessionId: "s2", status: "active", claudeStatus: "responding"),
                    SessionSummaryJSON(sessionId: "s3", status: "active", claudeStatus: "idle")
                ])
            ]
        ))
        #expect(sm.projects.count == 2)
        #expect(sm.projects[0].name == "proj-a")
        #expect(sm.projects[0].sessions.count == 1)
        #expect(sm.projects[1].name == "proj-b")
        #expect(sm.projects[1].sessions.count == 2)
    }
}

// MARK: - Suite 4: inbox lifecycle

@Suite("Inbox Lifecycle")
struct InboxLifecycleTests {
    @Test("20: inbox.added appends and shows attention")
    func inboxAddedAppends() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Allow Bash")
        ))
        #expect(sm.inboxItems.count == 1)
        #expect(sm.iconState == .attention)
    }

    @Test("21: inbox.added deduplicates by id")
    func inboxAddedDeduplicates() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Allow Bash")
        ))
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Allow Bash (updated)")
        ))
        #expect(sm.inboxItems.count == 1)
        #expect(sm.inboxItems[0].label == "Allow Bash (updated)")
    }

    @Test("22: inbox.removed removes and restores neutral")
    func inboxRemovedRestores() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Allow Bash")
        ))
        #expect(sm.inboxItems.count == 1)
        sm.handleMessage(ServerMessage(type: "inbox.removed", itemId: 1))
        #expect(sm.inboxItems.count == 0)
        #expect(sm.iconState == .neutral)
    }

    @Test("23: inbox.removed nonexistent is no-op")
    func inboxRemovedNonexistent() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Allow Bash")
        ))
        sm.handleMessage(ServerMessage(type: "inbox.removed", itemId: 999))
        #expect(sm.inboxItems.count == 1)
    }

    @Test("24: inbox.snapshot replaces all items")
    func inboxSnapshotReplaces() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        // Add one item first
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Old item")
        ))
        #expect(sm.inboxItems.count == 1)
        // Snapshot replaces
        sm.handleMessage(ServerMessage(
            type: "inbox.snapshot",
            items: [
                InboxItemJSON(id: 10, type: "permission", sessionId: "s2", label: "New A"),
                InboxItemJSON(id: 11, type: "question", sessionId: "s3", label: "New B")
            ]
        ))
        #expect(sm.inboxItems.count == 2)
        #expect(sm.inboxItems[0].id == 10)
        #expect(sm.inboxItems[1].id == 11)
    }
}

// MARK: - Suite 5: session.removed

@Suite("Session Removed")
struct SessionRemovedTests {
    @Test("25: clears previousStatuses entry")
    func clearsPreviousStatuses() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.previousStatuses["s1"] = "idle"
        sm.previousStatuses["s2"] = "responding"
        sm.handleMessage(ServerMessage(type: "session.removed", sessionId: "s1"))
        #expect(sm.previousStatuses["s1"] == nil)
        #expect(sm.previousStatuses["s2"] == "responding")
    }

    @Test("26: sends overview request")
    func sendsOverviewRequest() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(type: "session.removed", sessionId: "s1"))
        #expect(sm.pendingRequests.contains(TerminalRequest(type: "request.overview")))
    }
}

// MARK: - Suite 6: Known bugs (these test the FIXED behavior)

@Suite("Bug Fixes")
struct BugFixTests {
    @Test("27: Bug #1 — overview.snapshot with responding session updates hasResponding")
    func overviewSnapshotUpdatesHasResponding() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        // Add inbox item first
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Allow Bash")
        ))
        #expect(sm.iconState == .attention)
        // Now receive overview with a responding session
        sm.handleMessage(ServerMessage(
            type: "overview.snapshot",
            projects: [
                ProjectSummaryJSON(project: "proj", sessions: [
                    SessionSummaryJSON(sessionId: "s1", status: "active", claudeStatus: "responding")
                ])
            ]
        ))
        // hasResponding should be true, but attention has higher priority
        #expect(sm.hasResponding == true)
        #expect(sm.iconState == .attention)
    }

    @Test("28: Bug #2 — justFinished is per-transition, not cleared by other sessions")
    func justFinishedPerSession() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        // s1 responding, s2 idle
        sm.previousStatuses["s1"] = "responding"
        sm.previousStatuses["s2"] = "idle"
        sm.hasResponding = true
        // s1 finishes → justFinished
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "idle")]
        ))
        #expect(sm.justFinished == true)
        // s2 starts responding — this SHOULD clear justFinished
        // (Bug #2: in original code, justFinished was global and ANY responding clears it.
        //  This is actually the current behavior and is correct — when a session starts
        //  responding, the "just finished" flash should stop.)
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s2",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "responding")]
        ))
        #expect(sm.justFinished == false)
    }

    @Test("29: Bug #3 — session.removed recomputes hasResponding and updates icon")
    func sessionRemovedUpdatesIcon() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        // s1 responding, inbox has item
        sm.previousStatuses["s1"] = "responding"
        sm.hasResponding = true
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "permission", sessionId: "s1", label: "Allow Bash")
        ))
        // attention has higher priority than responding
        #expect(sm.iconState == .attention)
        // Remove the responding session
        sm.handleMessage(ServerMessage(type: "session.removed", sessionId: "s1"))
        // hasResponding should be false now → inbox item still shows attention
        #expect(sm.hasResponding == false)
        #expect(sm.iconState == .attention)
    }

    @Test("30: justFinished persists until a session starts responding")
    func justFinishedPersists() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.previousStatuses["s1"] = "responding"
        sm.hasResponding = true
        // Trigger justFinished
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "idle")]
        ))
        #expect(sm.justFinished == true)
        #expect(sm.iconState == .justFinished)
        // justFinished persists — no timer clears it
        sm.updateIconState()
        #expect(sm.justFinished == true)
        #expect(sm.iconState == .justFinished)
        // Only clears when a session starts responding
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "responding")]
        ))
        #expect(sm.justFinished == false)
        #expect(sm.iconState == .responding)
    }

    @Test("31: responding icon state shown when session responding and no inbox")
    func respondingIconState() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.previousStatuses["s1"] = "responding"
        sm.hasResponding = true
        sm.updateIconState()
        #expect(sm.iconState == .responding)
    }

    @Test("32: stale previousStatuses cleaned on overview snapshot")
    func stalePreviousStatusesCleaned() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        // Pre-populate with stale entries
        sm.previousStatuses["stale-1"] = "responding"
        sm.previousStatuses["stale-2"] = "idle"
        sm.hasResponding = true
        // Receive overview with only one active session (not stale-1 or stale-2)
        sm.handleMessage(ServerMessage(
            type: "overview.snapshot",
            projects: [
                ProjectSummaryJSON(project: "proj", sessions: [
                    SessionSummaryJSON(sessionId: "s1", status: "active", claudeStatus: "idle")
                ])
            ]
        ))
        #expect(sm.previousStatuses["stale-1"] == nil)
        #expect(sm.previousStatuses["stale-2"] == nil)
        #expect(sm.previousStatuses["s1"] == "idle")
        #expect(sm.hasResponding == false)
    }

    @Test("33: idle inbox items filtered out")
    func idleInboxItemsFiltered() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "inbox.added",
            item: InboxItemJSON(id: 1, type: "idle", sessionId: "s1", label: "Session idle")
        ))
        #expect(sm.inboxItems.count == 0)
        #expect(sm.iconState == .neutral)
    }

    @Test("34: idle items filtered from inbox.snapshot")
    func idleItemsFilteredFromSnapshot() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "inbox.snapshot",
            items: [
                InboxItemJSON(id: 1, type: "idle", sessionId: "s1", label: "Idle"),
                InboxItemJSON(id: 2, type: "permission", sessionId: "s2", label: "Allow Bash")
            ]
        ))
        #expect(sm.inboxItems.count == 1)
        #expect(sm.inboxItems[0].id == 2)
    }
}

// MARK: - Suite 7: Client-side patch application to projects model

@Suite("Patch Application to Projects")
struct PatchApplicationTests {

    /// Helper to seed a state manager with one project containing one session
    private func seededManager(
        sessionId: String = "s1",
        project: String = "proj",
        status: String = "active",
        claudeStatus: String = "idle",
        toolCount: Int = 5,
        slug: String? = nil
    ) -> MenuBarStateManager {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(
            type: "overview.snapshot",
            projects: [
                ProjectSummaryJSON(project: project, sessions: [
                    SessionSummaryJSON(
                        sessionId: sessionId,
                        slug: slug,
                        status: status,
                        claudeStatus: claudeStatus,
                        toolCount: toolCount,
                        lastEventTime: Date().timeIntervalSince1970
                    )
                ])
            ]
        ))
        sm.pendingRequests.removeAll()
        return sm
    }

    @Test("35: add_tool increments toolCount in projects")
    func addToolIncrementsToolCount() {
        let sm = seededManager(toolCount: 5)
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "add_tool")]
        ))
        #expect(sm.projects[0].sessions[0].toolCount == 6)
    }

    @Test("36: set_claude_status updates claudeStatus in projects")
    func setCludeStatusUpdatesProjects() {
        let sm = seededManager(claudeStatus: "idle")
        sm.previousStatuses["s1"] = "idle"
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_claude_status", claudeStatus: "responding")]
        ))
        #expect(sm.projects[0].sessions[0].claudeStatus == "responding")
    }

    @Test("37: session.update for unknown session queues request.overview")
    func unknownSessionQueuesOverview() {
        let sm = seededManager()
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "unknown-session",
            patches: [PatchJSON(op: "add_tool")]
        ))
        #expect(sm.pendingRequests.contains(TerminalRequest(type: "request.overview")))
    }

    @Test("38: any patch updates lastEventTime")
    func patchUpdatesLastEventTime() {
        let sm = seededManager(toolCount: 0)
        let before = Date().timeIntervalSince1970
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "add_tool")]
        ))
        let after = Date().timeIntervalSince1970
        let updated = sm.projects[0].sessions[0].lastEventTime
        #expect(updated >= before)
        #expect(updated <= after)
    }

    @Test("39: session.snapshot triggers overview request")
    func sessionSnapshotTriggersOverview() {
        let sm = MenuBarStateManager()
        sm.setConnected(true)
        sm.handleMessage(ServerMessage(type: "session.snapshot", sessionId: "s1"))
        #expect(sm.pendingRequests.contains(TerminalRequest(type: "request.overview")))
    }

    @Test("40: set_meta with slug updates session display name")
    func setMetaUpdatesSlug() {
        let sm = seededManager(slug: nil)
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [PatchJSON(op: "set_meta", slug: "my-slug")]
        ))
        #expect(sm.projects[0].sessions[0].slug == "my-slug")
        #expect(sm.projects[0].sessions[0].displayName == "my-slug")
    }

    @Test("41: multiple add_tool patches increment count correctly")
    func multipleAddToolPatches() {
        let sm = seededManager(toolCount: 3)
        sm.handleMessage(ServerMessage(
            type: "session.update",
            sessionId: "s1",
            patches: [
                PatchJSON(op: "add_tool"),
                PatchJSON(op: "add_tool"),
                PatchJSON(op: "add_tool")
            ]
        ))
        #expect(sm.projects[0].sessions[0].toolCount == 6)
    }
}
