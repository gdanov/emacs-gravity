import SwiftUI
import AppKit

@main
struct GravityMenuBarApp: App {
    @StateObject private var monitor = GravityMonitor()

    init() {
        // Hide from Dock — menu bar only
        NSApplication.shared.setActivationPolicy(.accessory)
    }

    var body: some Scene {
        MenuBarExtra {
            MenuBarDropdown(monitor: monitor)
        } label: {
            MenuBarLabel(monitor: monitor)
        }
        .menuBarExtraStyle(.window)
    }
}

// MARK: - Menu Bar Icon + Counts

struct MenuBarLabel: View {
    @ObservedObject var monitor: GravityMonitor

    var body: some View {
        HStack(spacing: 4) {
            if monitor.connected {
                Circle()
                    .fill(monitor.hasResponding ? .yellow : .green)
                    .frame(width: 6, height: 6)
                Text("\(monitor.activeCount)")
                    .font(.system(size: 12, weight: .medium))
                if monitor.inboxItems.count > 0 {
                    Text("⚠\(monitor.inboxItems.count)")
                        .font(.system(size: 11))
                        .foregroundColor(.orange)
                }
            } else {
                Circle()
                    .fill(.gray)
                    .frame(width: 6, height: 6)
                Text("off")
                    .font(.system(size: 11))
                    .foregroundColor(.gray)
            }
        }
    }
}

// MARK: - Dropdown Panel

struct MenuBarDropdown: View {
    @ObservedObject var monitor: GravityMonitor

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            if monitor.projects.isEmpty && monitor.inboxItems.isEmpty {
                if monitor.connected {
                    Text("No active sessions")
                        .font(.system(size: 12, design: .monospaced))
                        .foregroundColor(.secondary)
                        .padding(.vertical, 8)
                } else {
                    Text("gravity-server offline")
                        .font(.system(size: 12, design: .monospaced))
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
        .frame(width: 280)
        .font(.system(size: 12, design: .monospaced))
    }
}

struct ProjectSection: View {
    let project: ProjectInfo

    var body: some View {
        VStack(alignment: .leading, spacing: 2) {
            Text(project.name)
                .font(.system(size: 12, weight: .bold, design: .monospaced))
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
                .foregroundColor(session.statusColor)
                .font(.system(size: 11, design: .monospaced))
        }
        .padding(.vertical, 1)
    }
}

struct InboxRow: View {
    let item: InboxInfo

    var body: some View {
        HStack(spacing: 4) {
            Text("⚠")
                .font(.system(size: 11))
            Text(item.label)
                .foregroundColor(.orange)
                .lineLimit(1)
            Spacer()
            Text(item.project ?? "")
                .foregroundColor(.secondary)
                .font(.system(size: 11, design: .monospaced))
        }
        .padding(.vertical, 1)
    }
}
