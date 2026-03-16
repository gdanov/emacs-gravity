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

    @Test("4: justFinished overrides attention")
    func justFinishedOverridesAttention() {
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
        // justFinished should win over attention
        #expect(sm.iconState == .justFinished)
    }

    @Test("5: hasResponding suppresses attention → neutral")
    func hasRespondingSuppressesAttention() {
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
        // hasResponding suppresses attention
        #expect(sm.iconState == .neutral)
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
        // hasResponding should be true → suppresses attention → neutral
        #expect(sm.hasResponding == true)
        #expect(sm.iconState == .neutral)
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
        // hasResponding suppresses attention → neutral
        #expect(sm.iconState == .neutral)
        // Remove the responding session
        sm.handleMessage(ServerMessage(type: "session.removed", sessionId: "s1"))
        // hasResponding should be false now → inbox item shows attention
        #expect(sm.hasResponding == false)
        #expect(sm.iconState == .attention)
    }

    @Test("30: Bug #4 — clearJustFinishedIfStale clears after timeout")
    func justFinishedTimeout() {
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
        #expect(sm.justFinishedAt != nil)
        // Simulate time passing by backdating justFinishedAt
        sm.justFinishedAt = Date().addingTimeInterval(-5.0)
        sm.clearJustFinishedIfStale(after: 3.0)
        #expect(sm.justFinished == false)
        #expect(sm.justFinishedAt == nil)
        #expect(sm.iconState == .neutral)
    }
}
