# emacs-gravity: Terminal-to-Backend Communication Architecture

## Overview

emacs-gravity connects **two terminal frontends** (Emacs, macOS Menu Bar) to **three coding agent backends** (Claude Code, OpenCode, pi-agent) via a single stateful server.

## Component Diagram

```mermaid
flowchart TB
    subgraph Terminals["Terminals"]
        Emacs["Emacs\n(claude-gravity.el)"]
        Menubar["macOS Menu Bar\n(GravityMenuBar.app)"]
    end

    subgraph Bridge["Bridge Layer"]
        CC["Claude Code Bridge\n(bridge shim)"]
        OC["OpenCode Bridge\n(opencode-bridge.ts)"]
        PiB["pi Bridge\n(pi-session.ts)"]
    end

    subgraph Backend["gravity-server"]
        HS["Hook Socket\n(gravity-hooks.sock)"]
        TS["Terminal Socket\n(gravity-terminal.sock)"]
        PDriver["pi Driver\n(spawns pi-agent)"]
        StateStore[("Session Store")]
        Inbox[("Inbox")]
        TerminalSvc["Terminal Service"]
    end

    subgraph CodingAgents["Coding Agents"]
        ClaudeCode["Claude Code\n(external process)"]
        OpenCode["OpenCode\n(external process)"]
        PiAgent["pi-agent\n(external process)"]
    end

    %% Terminal → Server
    Emacs -->|pi.prompt, pi.start, action.*| TS
    Menubar -->|pi.prompt, pi.start, action.*| TS

    %% Bridge → Hook Socket
    CC -->|hook events\n(SessionStart, Stop, etc.)| HS
    OC -->|mapped events| HS
    PiB -->|mapped events| HS

    %% Coding agents → Bridge
    ClaudeCode -->|hook events| CC
    OpenCode -->|SSE events| OC
    PiAgent -->|agent events| PiB

    %% Hook Socket → Server handlers
    HS -->|parse & route| StateStore
    HS -->|bidirectional| Inbox
    HS -->|notify| TerminalSvc

    %% Terminal Socket → Terminals
    TS -->|session.update, overview.snapshot| Emacs
    TS -->|session.update, overview.snapshot| Menubar
    TS -->|inbox.added, pi.session| Emacs
    TS -->|inbox.added, pi.session| Menubar

    %% pi Driver ↔ pi Session
    PDriver -->|prompt, abort, set_model| PiAgent
    PiAgent -->|agent_end, tool_execution| PDriver

    %% Server → pi Driver
    TS -->|pi.prompt, pi.abort| PDriver
    PDriver -->|translated events| HS
```

## Socket Architecture

| Socket | Path | Protocol | Purpose |
|--------|------|----------|---------|
| **Hook Socket** | `gravity-hooks.sock` | JSON lines (newline-delimited) | Bridge shims send hook events from coding agents |
| **Terminal Socket** | `gravity-terminal.sock` | JSON lines (newline-delimited) | Terminals send commands; receive state patches |

### Hook Socket Messages (Bridge → Server)

| Event | Direction | Description |
|-------|-----------|-------------|
| `SessionStart` | → | New session began |
| `SessionEnd` | → | Session ended |
| `UserPromptSubmit` | → | User sent a prompt |
| `Stop` | → | Agent finished a turn |
| `PreToolUse` | → | Tool call starting |
| `PostToolUse` | → | Tool call completed |
| `PostToolUseFailure` | → | Tool call failed |
| `PermissionRequest` | ↔ | Needs user approval (bidirectional) |
| `AskUserQuestionIntercept` | ↔ | Needs user answer (bidirectional) |
| `SubagentStart` | → | Sub-agent spawned |
| `SubagentStop` | → | Sub-agent completed |
| `Notification` | → | Log/info message |

### Terminal Socket Messages (Terminal ↔ Server)

**Client → Server:**
| Message | Description |
|---------|-------------|
| `hello` | Announce capabilities (`["action.permission", "action.question", ...]`) |
| `pi.start` | Start a pi session |
| `pi.prompt` | Send text prompt to pi |
| `pi.abort` | Interrupt current generation |
| `pi.stop` | Kill pi process entirely |
| `pi.set-thinking` | Change thinking level |
| `pi.set-model` | Change model |
| `pi.resume` | Resume a session file |
| `pi.compact` | Compact context |
| `pi.new-session` | Start fresh session |
| `action.permission` | Respond to permission request |
| `action.question` | Respond to question |
| `action.plan-review` | Submit plan feedback |
| `poll` | Pull mode: request current state |
| `request.session` | Request session snapshot |
| `request.overview` | Request project overview |
| `request.resync` | Request full state resync |

**Server → Client:**
| Message | Description |
|---------|-------------|
| `session.update` | Incremental patches |
| `session.snapshot` | Full session state |
| `overview.snapshot` | Project summaries |
| `inbox.added` | New permission/question |
| `inbox.removed` | Inbox item resolved |
| `inbox.snapshot` | Full inbox state |
| `pi.session` | pi lifecycle event (`started`, `stopped`, `rejected`) |
| `notice` | Warning/info (e.g., hooks silence) |
| `session.removed` | Session purged |

## Communication Flows

### 1. External Agent (Claude Code / OpenCode) → Emacs

```
Claude Code (hook events)
    ↓
bridge shim (one-shot process)
    ↓ JSON lines
gravity-hooks.sock
    ↓
gravity-server (parse, store patches, update inbox)
    ↓
gravity-terminal.sock
    ↓ JSON lines
Emacs (render session buffer, update overview)
```

### 2. Emacs → pi (embedded agent)

```
Emacs (M-x claude-gravity-compose-prompt)
    ↓ { type: "pi.prompt", text: "..." }
gravity-terminal.sock
    ↓
gravity-server (route to pi driver)
    ↓
pi-driver (spawn pi-agent process)
    ↓ text prompt
pi-agent
    ↓ agent events (agent_start, tool_execution, message_update)
pi-session.ts
    ↓ translated hook events
gravity-hooks.sock
    ↓
gravity-server
    ↓
gravity-terminal.sock
    ↓
Emacs (stream delta → render buffer)
```

### 3. Permission Request (bidirectional)

```
Coding Agent
    ↓ PermissionRequest (needs_response=true)
Bridge shim
    ↓ write to hook socket
gravity-server (wait for capable terminal, timeout 10s)
    ↓ add to inbox, broadcast
gravity-terminal.sock
    ↓
Emacs (show permission buffer)
    ↓ user clicks "Allow" or "Deny"
action.permission { itemId, decision }
gravity-terminal.sock
    ↓
gravity-server
    ↓ write response to hook socket
Bridge shim
    ↓ HTTP response or return code
Coding Agent (continues or aborts)
```

### 4. pi Extension UI (dialog)

```
pi-agent (extension_ui_request: { method: "confirm" | "select", ... })
    ↓
pi-session.ts (onExtensionUIRequest callback)
    ↓
gravity-server (handlePiExtensionUIRequest)
    ↓ add to inbox with pi_ui metadata, pendingPiUIResponses map
gravity-terminal.sock
    ↓
Emacs (render permission/question with pi options)
    ↓ action.permission { itemId, decision }
gravity-terminal.sock
    ↓
gravity-server (route to pendingPiUIResponses)
    ↓ sendExtensionUIResponse
pi-session.ts
    ↓
pi-agent (receives response, continues)
```

## Session State Flow

```
┌─────────────────────────────────────────────────────┐
│                    gravity-server                     │
│                                                      │
│   Hook events     Session Store      Terminals       │
│   ──────────     ────────────      ──────────       │
│       │                │                ↑             │
│       │  runEvent()    │                │            │
│       ↓                ↓                │ broadcast   │
│   event-handler ──→ patches ──────→  patches       │
│                     (append)           (session.update)│
│                        │                             │
│                        ↓                             │
│                    Inbox                            │
│                    (permission, question items)     │
│                        │                             │
│              ┌────────┴────────┐                   │
│              ↑                  ↑                    │
│          inbox.added       inbox.respond             │
│              │                  │                    │
│              └────── action.* ──┘                   │
└─────────────────────────────────────────────────────┘
```

## Pull Mode vs Push Mode

| Mode | Trigger | Behavior |
|------|---------|----------|
| **Push** (default) | `GRAVITY_PUSH_MODE=true` | Server broadcasts patches on every event |
| **Pull** | `GRAVITY_PULL_MODE=true` (default) | Server stores patches, signals `changed`; client polls with `poll` |

Pull mode reduces chattiness — terminals request state on idle or after receiving a signal, avoiding per-event broadcasts.

## Backend Comparison

| Backend | Spawn Method | Protocol | UI Integration |
|---------|-------------|----------|---------------|
| **Claude Code** | External process | Hook events | Bridge shim (one-shot) |
| **OpenCode** | External process (mDNS discovery) | SSE + HTTP | opencode-bridge.ts |
| **pi-agent** | Spawned by gravity-server | Agent SDK events | pi-session.ts (embedded) |
| **Claude SDK daemon** | Node.js subprocess | Hook events | claude-gravity-daemon.el (ON HOLD) |

## File Reference

| Component | File |
|-----------|------|
| Emacs terminal client | `claude-gravity-client.el` |
| Menu bar app | `packages/gravity-menubar/` |
| gravity-server | `packages/gravity-server/src/gravity-server.ts` |
| pi driver | `packages/gravity-server/src/pi-driver/` |
| pi bridge | `packages/emacs-bridge/src/pi-session.ts` |
| OpenCode bridge | `packages/emacs-bridge/src/opencode-bridge.ts` |
| Shared types | `packages/shared/src/types.ts` |