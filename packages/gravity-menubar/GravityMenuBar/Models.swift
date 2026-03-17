import SwiftUI

// MARK: - Menu Bar Icon State

public enum MenuBarIconState: Equatable {
    case neutral       // connected, all idle or no sessions
    case responding    // at least one session is responding
    case justFinished  // a session just went responding→idle
    case attention     // inbox items present
    case disconnected  // not connected to server

    public var systemImage: String {
        switch self {
        case .neutral:      return "bolt.fill"
        case .responding:   return "bolt.fill"
        case .justFinished: return "bolt.fill"
        case .attention:    return "message.fill"
        case .disconnected: return "bolt.slash.fill"
        }
    }

    public var color: Color {
        switch self {
        case .neutral:      return .white
        case .responding:   return .orange
        case .justFinished: return .green
        case .attention:    return .orange
        case .disconnected: return .secondary
        }
    }
}

// MARK: - View Models

public struct ProjectInfo: Identifiable {
    public let id: String
    public let name: String
    public var sessions: [SessionInfo]

    public init(id: String, name: String, sessions: [SessionInfo]) {
        self.id = id
        self.name = name
        self.sessions = sessions
    }
}

public struct SessionInfo: Identifiable {
    public let id: String
    public var slug: String?
    public var status: String        // "active" | "ended"
    public var claudeStatus: String  // "idle" | "responding"
    public var toolCount: Int
    public var lastEventTime: Double

    public init(id: String, slug: String?, status: String, claudeStatus: String, toolCount: Int, lastEventTime: Double) {
        self.id = id
        self.slug = slug
        self.status = status
        self.claudeStatus = claudeStatus
        self.toolCount = toolCount
        self.lastEventTime = lastEventTime
    }

    public var displayName: String {
        slug ?? String(id.prefix(8))
    }

    public var statusColor: Color {
        if status == "ended" { return .gray }
        if claudeStatus == "responding" { return .yellow }
        return .green
    }

    public var statusLabel: String {
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

public struct InboxInfo: Identifiable {
    public let id: Int
    public let type: String
    public let sessionId: String
    public let project: String?
    public let label: String
    public let summary: String

    public init(id: Int, type: String, sessionId: String, project: String?, label: String, summary: String) {
        self.id = id
        self.type = type
        self.sessionId = sessionId
        self.project = project
        self.label = label
        self.summary = summary
    }
}

// MARK: - JSON Protocol Types (server → terminal)

/// Represents any message from gravity-server
public struct ServerMessage: Decodable {
    public let type: String

    // overview.snapshot
    public let projects: [ProjectSummaryJSON]?

    // inbox.added
    public let item: InboxItemJSON?

    // inbox.removed
    public let itemId: Int?

    // inbox.snapshot
    public let items: [InboxItemJSON]?

    // session.update / session.removed
    public let sessionId: String?
    public let patches: [PatchJSON]?

    enum CodingKeys: String, CodingKey {
        case type, projects, item, itemId, items, sessionId, patches
    }

    /// Memberwise init for test factories
    public init(type: String, projects: [ProjectSummaryJSON]? = nil, item: InboxItemJSON? = nil, itemId: Int? = nil, items: [InboxItemJSON]? = nil, sessionId: String? = nil, patches: [PatchJSON]? = nil) {
        self.type = type
        self.projects = projects
        self.item = item
        self.itemId = itemId
        self.items = items
        self.sessionId = sessionId
        self.patches = patches
    }
}

public struct ProjectSummaryJSON: Decodable {
    public let project: String
    public let sessions: [SessionSummaryJSON]

    public init(project: String, sessions: [SessionSummaryJSON]) {
        self.project = project
        self.sessions = sessions
    }
}

public struct SessionSummaryJSON: Decodable {
    public let sessionId: String
    public let slug: String?
    public let status: String
    public let claudeStatus: String
    public let toolCount: Int
    public let lastEventTime: Double

    public init(sessionId: String, slug: String? = nil, status: String, claudeStatus: String, toolCount: Int = 0, lastEventTime: Double = 0) {
        self.sessionId = sessionId
        self.slug = slug
        self.status = status
        self.claudeStatus = claudeStatus
        self.toolCount = toolCount
        self.lastEventTime = lastEventTime
    }
}

public struct InboxItemJSON: Decodable {
    public let id: Int
    public let type: String
    public let sessionId: String
    public let project: String?
    public let label: String
    public let summary: String

    public init(id: Int, type: String, sessionId: String, project: String? = nil, label: String, summary: String = "") {
        self.id = id
        self.type = type
        self.sessionId = sessionId
        self.project = project
        self.label = label
        self.summary = summary
    }
}

/// Patch decoding — status changes, tool counts, and metadata for the menu bar
public struct PatchJSON: Decodable {
    public let op: String
    public let status: String?
    public let claudeStatus: String?
    public let slug: String?

    enum CodingKeys: String, CodingKey {
        case op, status, claudeStatus, slug
    }

    public init(op: String, status: String? = nil, claudeStatus: String? = nil, slug: String? = nil) {
        self.op = op
        self.status = status
        self.claudeStatus = claudeStatus
        self.slug = slug
    }
}

// MARK: - Terminal → Server request

public struct TerminalRequest: Encodable, Equatable {
    public let type: String

    public init(type: String) {
        self.type = type
    }
}
