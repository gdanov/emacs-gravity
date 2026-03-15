# gravity-menubar

macOS menu bar status item for emacs-gravity. Shows active session count and attention items from gravity-server.

## Requirements

- macOS 13 (Ventura) or later
- Swift 5.9+
- gravity-server running (provides data via `~/.local/state/gravity-terminal.sock`)

## Build & Run

```bash
cd packages/gravity-menubar
swift build
swift run
```

Or build a release binary:

```bash
swift build -c release
# Binary at .build/release/GravityMenuBar
```

## What it shows

**Menu bar:** One colored dot per active session — green (idle), yellow (responding), orange (waiting on user). Icon switches to `!` bubble when inbox items need attention. Shows `⚡̸` when server is offline.

**Dropdown menu:**
- Sessions grouped by project with status (idle/responding/ended)
- Inbox items needing attention (permissions, plan reviews, questions)
- Auto-reconnects when server restarts

## Configuration

Override socket path via environment variable:

```bash
GRAVITY_TERMINAL_SOCK=/path/to/socket swift run
```

## No Apple Developer Account needed

Build and run locally with just Xcode Command Line Tools (free). Developer account only required for distribution to others.
