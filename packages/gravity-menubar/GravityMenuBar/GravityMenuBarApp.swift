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
                ProjectSection(project: project)
            }

            if !monitor.inboxItems.isEmpty {
                Divider()
                ForEach(monitor.inboxItems) { item in
                    InboxRow(item: item)
                }
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

    var body: some View {
        VStack(alignment: .leading, spacing: 2) {
            Text(project.name)
                .font(.system(size: 12, weight: .bold))
                .foregroundColor(.primary)
                .padding(.top, 2)

            ForEach(project.sessions) { session in
                SessionRow(session: session)
                    .padding(.leading, 12)
            }
        }
    }
}

struct SessionRow: View {
    let session: SessionInfo

    var body: some View {
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
    }
}

struct InboxRow: View {
    let item: InboxInfo

    var body: some View {
        HStack(spacing: 4) {
            Text("\u{26A0}")
                .font(.system(size: 11))
            Text(item.label)
                .foregroundColor(.primary)
                .lineLimit(1)
            Spacer()
            Text(item.project ?? "")
                .foregroundColor(.secondary)
                .font(.system(size: 11))
        }
        .padding(.vertical, 1)
    }
}
