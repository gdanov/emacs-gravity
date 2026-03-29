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

            ForEach(Array(project.sessions.enumerated()), id: \.element.id) { index, session in
                if index > 0 {
                    Divider()
                        .padding(.vertical, 4)
                }
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
        VStack(alignment: .leading, spacing: 4) {
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

            // User prompt bubble (right-aligned)
            if let prompt = session.latestUserPrompt, !prompt.isEmpty {
                let trimmed = prompt.count > 150 ? String(prompt.prefix(150)) + "..." : prompt
                MessageBubble(text: trimmed, isUser: true)
                    .padding(.leading, 10)
            }

            // Assistant message bubble (left-aligned)
            if let msg = session.latestMessage, !msg.isEmpty {
                let trimmed = msg.count > 200 ? String(msg.prefix(200)) + "..." : msg
                MessageBubble(text: trimmed, isUser: false)
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

// MARK: - Message Bubble

struct MessageBubble: View {
    let text: String
    let isUser: Bool

    var body: some View {
        HStack {
            if isUser { Spacer(minLength: 40) }

            Text(text)
                .font(.system(size: 11))
                .foregroundColor(isUser ? .white : .secondary)
                .lineLimit(3)
                .truncationMode(.tail)
                .fixedSize(horizontal: false, vertical: true)
                .padding(.horizontal, 8)
                .padding(.vertical, 4)
                .background(
                    RoundedRectangle(cornerRadius: 8)
                        .fill(isUser ? Color.accentColor.opacity(0.7) : Color.primary.opacity(0.08))
                )

            if !isUser { Spacer(minLength: 40) }
        }
    }
}

// MARK: - Inbox Row (card style)

struct InboxRow: View {
    let item: InboxInfo

    private var accentColor: Color {
        switch item.type {
        case "permission":  return .orange
        case "question":    return .blue
        case "plan-review": return .purple
        default:            return .orange
        }
    }

    private var iconName: String {
        switch item.type {
        case "permission":  return "lock.shield"
        case "question":    return "questionmark.circle.fill"
        case "plan-review": return "doc.text.magnifyingglass"
        default:            return "exclamationmark.triangle.fill"
        }
    }

    var body: some View {
        HStack(spacing: 6) {
            RoundedRectangle(cornerRadius: 1.5)
                .fill(accentColor)
                .frame(width: 3)

            Image(systemName: iconName)
                .font(.system(size: 10))
                .foregroundColor(accentColor)

            VStack(alignment: .leading, spacing: 1) {
                Text(item.label)
                    .foregroundColor(.primary)
                    .lineLimit(1)
                if !item.summary.isEmpty {
                    Text(item.summary)
                        .font(.system(size: 10))
                        .foregroundColor(.secondary)
                        .lineLimit(2)
                }
            }
        }
        .padding(.horizontal, 6)
        .padding(.vertical, 4)
        .background(
            RoundedRectangle(cornerRadius: 6)
                .fill(accentColor.opacity(0.06))
        )
    }
}
