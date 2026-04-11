import SwiftUI
import AppKit
import GravityMenuBarLib

// Holds the App Nap opt-out token for the process lifetime.
private let appNapToken: NSObjectProtocol = ProcessInfo.processInfo.beginActivity(
    options: [.userInitiated, .idleSystemSleepDisabled],
    reason: "gravity-menubar socket heartbeat"
)

@main
struct GravityMenuBarApp: App {
    @StateObject private var monitor = GravityMonitor()

    init() {
        // Hide from Dock — menu bar only
        NSApplication.shared.setActivationPolicy(.accessory)
        // Defense-in-depth: ignore SIGPIPE process-wide
        signal(SIGPIPE, SIG_IGN)
        // Keep appNapToken referenced so ARC doesn't release it.
        _ = appNapToken
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
