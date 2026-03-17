import SwiftUI
import AppKit
import GravityMenuBarLib

@main
struct GravityMenuBarApp: App {
    @StateObject private var monitor = GravityMonitor()

    init() {
        // Hide from Dock — menu bar only
        NSApplication.shared.setActivationPolicy(.accessory)
        // Defense-in-depth: ignore SIGPIPE process-wide
        signal(SIGPIPE, SIG_IGN)
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
