import SwiftUI

// MARK: - Display Models

struct ProjectInfo: Identifiable {
    let id: String
    let name: String
    var sessions: [SessionInfo]
}

struct SessionInfo: Identifiable {
    let id: String
    let slug: String?
    var status: String        // "active" | "ended"
    var claudeStatus: String  // "idle" | "responding"
    let toolCount: Int
    let lastEventTime: Double

    var displayName: String {
        slug ?? String(id.prefix(8))
    }

    var statusColor: Color {
        if status == "ended" { return .gray }
        if claudeStatus == "responding" { return .yellow }
        return .green
    }

    var statusLabel: String {
        if status == "ended" { return "ended" }
        if claudeStatus == "responding" { return "responding" }
        let elapsed = Date().timeIntervalSince1970 - lastEventTime
        if elapsed > 3600 {
            return "idle \(Int(elapsed / 3600))h"
        } else if elapsed > 60 {
            return "idle \(Int(elapsed / 60))m"
        }
        return "idle"
    }
}

struct InboxItemInfo: Identifiable {
    let id: Int
    let type: String
    let sessionId: String
    let project: String?
    let label: String
    let summary: String
}

// MARK: - JSON Protocol Types

struct OverviewSnapshot: Decodable {
    let type: String
    let projects: [ProjectSummaryJSON]
}

struct ProjectSummaryJSON: Decodable {
    let project: String
    let sessions: [SessionSummaryJSON]
}

struct SessionSummaryJSON: Decodable {
    let sessionId: String
    let slug: String?
    let status: String
    let claudeStatus: String
    let toolCount: Int
    let lastEventTime: Double
}

struct InboxAdded: Decodable {
    let type: String
    let item: InboxItemJSON
}

struct InboxRemoved: Decodable {
    let type: String
    let itemId: Int
}

struct InboxSnapshot: Decodable {
    let type: String
    let items: [InboxItemJSON]
}

struct InboxItemJSON: Decodable {
    let id: Int
    let type: String
    let sessionId: String
    let project: String?
    let label: String
    let summary: String
}

struct SessionUpdate: Decodable {
    let type: String
    let sessionId: String
    let patches: [PatchJSON]
}

struct PatchJSON: Decodable {
    let op: String
    let status: String?
    let claudeStatus: String?
}

struct SessionSnapshot: Decodable {
    let type: String
    let sessionId: String
    let session: SessionJSON
}

struct SessionJSON: Decodable {
    let sessionId: String
    let project: String
    let status: String
    let claudeStatus: String
    let slug: String?
    let totalToolCount: Int
    let lastEventTime: Double
}

struct SessionRemoved: Decodable {
    let type: String
    let sessionId: String
}

/// Minimal envelope to read just the "type" field
struct MessageEnvelope: Decodable {
    let type: String
}
