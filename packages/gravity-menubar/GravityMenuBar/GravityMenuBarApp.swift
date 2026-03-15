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
        .menuBarExtraStyle(.menu)
    }
}

// MARK: - Menu Bar Icon + Text

struct MenuBarLabel: View {
    @ObservedObject var monitor: GravityMonitor

    var body: some View {
        HStack(spacing: 4) {
            Image(systemName: monitor.connected ? "circle.fill" : "circle")
                .font(.system(size: 9))
            if monitor.connected {
                let active = monitor.activeSessionCount
                let attention = monitor.attentionCount
                if active > 0 {
                    Text("\(active)")
                }
                if attention > 0 {
                    Text("!").foregroundColor(.orange)
                }
            } else {
                Text("off")
            }
        }
    }
}

// MARK: - Dropdown Menu

struct MenuBarDropdown: View {
    @ObservedObject var monitor: GravityMonitor

    var body: some View {
        if !monitor.connected {
            Text("gravity-server offline")
                .foregroundColor(.secondary)
            Divider()
            Button("Reconnect") {
                monitor.reconnect()
            }
        } else if monitor.projects.isEmpty {
            Text("No sessions")
                .foregroundColor(.secondary)
        } else {
            ForEach(monitor.projects) { project in
                Section(project.name) {
                    ForEach(project.sessions) { session in
                        SessionRow(session: session)
                    }
                }
            }

            if !monitor.inboxItems.isEmpty {
                Divider()
                Section("Attention") {
                    ForEach(monitor.inboxItems) { item in
                        InboxRow(item: item)
                    }
                }
            }
        }

        Divider()
        Button("Quit") {
            NSApplication.shared.terminate(nil)
        }
        .keyboardShortcut("q")
    }
}

struct SessionRow: View {
    let session: SessionInfo

    var body: some View {
        HStack {
            Image(systemName: session.status == "active" ? "circle.fill" : "circle")
                .font(.system(size: 8))
                .foregroundColor(session.statusColor)
            Text(session.displayName)
            Spacer()
            Text(session.claudeStatusLabel)
                .foregroundColor(.secondary)
                .font(.caption)
        }
    }
}

struct InboxRow: View {
    let item: InboxInfo

    var body: some View {
        HStack {
            Image(systemName: "exclamationmark.triangle.fill")
                .font(.system(size: 10))
                .foregroundColor(.orange)
            Text(item.label)
            if let project = item.project {
                Text("(\(project))")
                    .foregroundColor(.secondary)
                    .font(.caption)
            }
        }
    }
}
