import SwiftUI
import AppKit

// MARK: - Menu Bar Icon + Counts

public struct MenuBarLabel: View {
    @ObservedObject var monitor: GravityMonitor

    public init(monitor: GravityMonitor) {
        self.monitor = monitor
    }

    public var body: some View {
        Image(systemName: monitor.iconState.systemImage)
            .symbolRenderingMode(.palette)
            .foregroundStyle(monitor.iconState.color)
    }
}

// MARK: - Dropdown Panel

public struct MenuBarDropdown: View {
    @ObservedObject var monitor: GravityMonitor

    public init(monitor: GravityMonitor) {
        self.monitor = monitor
    }

    public var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            if monitor.projects.isEmpty && monitor.inboxItems.isEmpty {
                if monitor.connected {
                    Text("No active sessions")
                        .font(.system(size: 12))
                        .foregroundColor(.secondary)
                        .padding(.vertical, 8)
                } else {
                    Text("gravity-server offline")
                        .font(.system(size: 12))
                        .foregroundColor(.secondary)
                        .padding(.vertical, 8)
                }
            }

            ForEach(monitor.projects) { project in
                ProjectSection(
                    project: project,
                    inboxItems: monitor.inboxItems
                )
            }

            Divider()

            HStack {
                if monitor.connected {
                    Circle()
                        .fill(.green)
                        .frame(width: 6, height: 6)
                    Text("Connected")
                        .font(.system(size: 11))
                        .foregroundColor(.secondary)
                } else {
                    Circle()
                        .fill(.red)
                        .frame(width: 6, height: 6)
                    Text("Disconnected")
                        .font(.system(size: 11))
                        .foregroundColor(.secondary)
                }
                Spacer()
                Button("Quit") {
                    NSApplication.shared.terminate(nil)
                }
                .buttonStyle(.plain)
                .font(.system(size: 11))
                .foregroundColor(.secondary)
            }
            .padding(.top, 2)
        }
        .padding(10)
        .frame(width: 560)
        .font(.system(size: 12))
    }
}

struct ProjectSection: View {
    let project: ProjectInfo
    let inboxItems: [InboxInfo]

    var body: some View {
        VStack(alignment: .leading, spacing: 2) {
            Text(project.name)
                .font(.system(size: 12, weight: .bold))
                .foregroundColor(.primary)
                .padding(.top, 2)

            ForEach(project.sessions) { session in
                SessionSection(
                    session: session,
                    inboxItems: inboxItems.filter { $0.sessionId == session.id }
                )
                .padding(.leading, 12)
            }
        }
    }
}

struct SessionSection: View {
    let session: SessionInfo
    let inboxItems: [InboxInfo]

    var body: some View {
        VStack(alignment: .leading, spacing: 2) {
            // Session header row
            HStack(spacing: 4) {
                Circle()
                    .fill(session.statusColor)
                    .frame(width: 6, height: 6)
                Text(session.displayName)
                    .foregroundColor(.primary)
                    .lineLimit(1)
                Spacer()
                Text(session.statusLabel)
                    .foregroundColor(.secondary)
                    .font(.system(size: 11))
            }
            .padding(.vertical, 1)

            // Latest message (capped to 200 chars)
            if let msg = session.latestMessage, !msg.isEmpty {
                let trimmed = msg.count > 200 ? String(msg.prefix(200)) + "..." : msg
                Text(trimmed)
                    .font(.system(size: 11))
                    .foregroundColor(.secondary)
                    .lineLimit(3)
                    .padding(.leading, 10)
            }

            // Inbox items for this session
            if !inboxItems.isEmpty {
                ForEach(inboxItems) { item in
                    InboxRow(item: item)
                        .padding(.leading, 10)
                }
            }
        }
    }
}

struct InboxRow: View {
    let item: InboxInfo

    var body: some View {
        HStack(spacing: 4) {
            Image(systemName: "exclamationmark.triangle.fill")
                .font(.system(size: 10))
                .foregroundColor(.orange)
            Text(item.label)
                .foregroundColor(.primary)
                .lineLimit(1)
        }
        .padding(.vertical, 1)
    }
}
