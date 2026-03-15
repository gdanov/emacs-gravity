import SwiftUI

// MARK: - Menu Bar Icon State

enum MenuBarIconState: Equatable {
    case neutral       // connected, all idle or no sessions
    case justFinished  // a session just went responding→idle
    case attention     // inbox items present
    case disconnected  // not connected to server

    var systemImage: String {
        switch self {
        case .neutral:      return "bolt.fill"
        case .justFinished: return "bolt.fill"
        case .attention:    return "exclamationmark.bubble.fill"
        case .disconnected: return "bolt.slash.fill"
        }
    }

    var color: Color {
        switch self {
        case .neutral:      return .secondary
        case .justFinished: return .green
        case .attention:    return .orange
        case .disconnected: return .secondary
        }
    }
}

// MARK: - View Models

struct ProjectInfo: Identifiable {
    let id: String
    let name: String
    let sessions: [SessionInfo]
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

struct InboxInfo: Identifiable {
    let id: Int
    let type: String
    let sessionId: String
    let project: String?
    let label: String
    let summary: String
}

// MARK: - JSON Protocol Types (server → terminal)

/// Represents any message from gravity-server
struct ServerMessage: Decodable {
    let type: String

    // overview.snapshot
    let projects: [ProjectSummaryJSON]?

    // inbox.added
    let item: InboxItemJSON?

    // inbox.removed
    let itemId: Int?

    // inbox.snapshot
    let items: [InboxItemJSON]?

    // session.update
    let sessionId: String?
    let patches: [PatchJSON]?

    enum CodingKeys: String, CodingKey {
        case type, projects, item, itemId, items, sessionId, patches
    }
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

struct InboxItemJSON: Decodable {
    let id: Int
    let type: String
    let sessionId: String
    let project: String?
    let label: String
    let summary: String
}

/// Minimal patch decoding — we only care about status changes for the menu bar
struct PatchJSON: Decodable {
    let op: String
    let status: String?
    let claudeStatus: String?

    enum CodingKeys: String, CodingKey {
        case op, status, claudeStatus
    }
}

// MARK: - Terminal → Server request

struct TerminalRequest: Encodable {
    let type: String
}
