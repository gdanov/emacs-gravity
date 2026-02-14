# OpenCode Integration Plan

Display OpenCode sessions in emacs-gravity alongside Claude Code sessions.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  OpenCode Instance 1 (port 52341)  ─┐                      │
│  OpenCode Instance 2 (port 49823)  ──┼──> opencode-bridge ──> Emacs
│  OpenCode Instance 3 (port 61234)  ─┘                      │
└─────────────────────────────────────────────────────────────┘
```

## Discovery: mDNS

- OpenCode instances publish via mDNS when started with `--mdns`
- Bridge scans for `opencode-*` services on `opencode.local`
- Requires user to start OpenCode with `--mdns` flag

## Component: opencode-bridge

**Location**: `emacs-bridge/src/opencode-bridge.ts`

**Dependencies**: `bonjour-service` (for mDNS)

### Event Mapping

| OpenCode Event | emacs-gravity Event | Data Available |
|----------------|---------------------|----------------|
| `session.created` | `SessionStart` | `id`, `title`, `directory`, `projectID`, `parentID`, `time.created`, `permission` |
| `session.deleted` | `SessionEnd` | `sessionID` |
| `session.status` (idle/busy/retry) | Status update | `sessionID`, `status.type`, `status.attempt`, `status.message` |
| `message.updated` (role: user) | `UserPromptSubmit` | Full `UserMessage` with `id`, `agent`, `model`, `tools`, `system` |
| `message.updated` (role: assistant) | Assistant message | Full `AssistantMessage` with `id`, `modelID`, `providerID`, `cost`, `tokens` |
| `message.part.delta` | Streaming content | `sessionID`, `messageID`, `partID`, `field`, `delta` |
| `message.part.updated` | Part update | Full `Part` object (text, reasoning, tool, etc.) |
| `permission.asked` | Permission request | `id`, `sessionID`, `permission`, `patterns`, `metadata`, `always`, `tool` |
| `question.asked` | Question | `id`, `sessionID`, `questions[]`, `tool` |
| `vcs.branch.updated` | Branch metadata | `branch` |
| `session.updated` | Session metadata | `title`, `slug`, `summary` |

## Session Data to Extract

### From `session.created` event

```typescript
{
  id: string,           // Session ID (e.g., "ses_abc123")
  title: string,        // Session title
  slug: string,         // Auto-generated slug
  directory: string,    // Project directory
  projectID: string,   // Project ID
  parentID?: string,   // Parent session if forked
  time: {
    created: number,  // Unix timestamp
    updated: number
  }
}
```

### From `message.updated` (assistant)

```typescript
{
  id: string,           // Message ID
  sessionID: string,
  role: "assistant",
  modelID: string,
  providerID: string,
  cost: number,        // Total cost in USD
  tokens: {
    input: number,
    output: number,
    reasoning: number,
    cache: { read: number, write: number }
  },
  finish?: string       // "stop", "length", "error", etc.
}
```

### From `/vcs` API

```typescript
{
  branch: string        // Current git branch
}
```

## Part Types to Handle

| Part Type | Description |
|-----------|-------------|
| `text` | Regular text content |
| `reasoning` | Model reasoning/thinking |
| `tool` | Tool use (calls + results) |
| `step-start` | Step beginning |
| `step-finish` | Step completion with cost/tokens |
| `agent` | Agent switching |
| `subtask` | Subtask prompts |
| `patch` | Patch/diff info |
| `file` | File attachments |
| `snapshot` | Snapshot data |
| `retry` | Retry attempts |
| `compaction` | Session compaction |

## Session Plist Fields (Emacs)

```elisp
(:session-id . "ses_abc123")
(:source . "opencode")           ; vs "claude-code"
(:instance-port . 52341)          ; Port of OpenCode instance
(:instance-dir . "/path/to/proj") ; Project directory
(:title . "Session title")
(:slug . "session-slug")
(:directory . "/path/to/project")
(:branch . "main")                ; From /vcs API
(:model-name . "claude-sonnet-4")
(:cost . 0.05)
(:token-usage . (input . 1000) (output . 500))
(:status . 'idle)                ; idle/busy/retry
```

## Files to Create/Modify

| File | Action |
|------|--------|
| `emacs-bridge/src/opencode-bridge.ts` | **Create** — OpenCode discovery + event bridge |
| `emacs-bridge/package.json` | Add `bonjour-service` dependency |
| `emacs-bridge/src/index.ts` | Add `--mode opencode` flag or separate entry |
| `claude-gravity-session.el` | Add `:source`, `:instance-port`, `:instance-dir` fields |
| `claude-gravity-events.el` | Source-aware event handling |
| `claude-gravity-ui.el` | Optional: distinct styling for OpenCode sessions |

## User Workflow

1. **Start OpenCode with mDNS**: `opencode --mdns`
2. **Start emacs-gravity** (bridge auto-discovers all OpenCode instances)
3. **Sessions appear** in emacs-gravity overview alongside Claude Code sessions

## Open Questions

1. **Should bridge be separate process or integrated?** — Recommend separate: `node opencode-bridge.js`
2. **How to handle OpenCode session lifecycle?** — Since sessions are in-memory, bridge must poll to detect stale sessions
3. **Tool call representation?** — OpenCode has `tool` parts with states (pending/running/completed/error) vs Claude Code's separate PreToolUse/PostToolUse events. Map to similar structure?
4. **Permission/Question handling?** — OpenCode sends `permission.asked` and `question.asked` events. Map 1:1 to emacs-gravity inbox format?
