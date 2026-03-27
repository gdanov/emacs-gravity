# Event Handling End-to-End — Mermaid Diagrams

Generated from code analysis of gravity-server's event handling pipeline.

---

## Diagram 1: Event Handling Overview

The fire-and-forget path: Claude Code fires a hook, the bridge enriches the data, forwards it to the server's hook socket, the event handler mutates session state and produces patches, and those patches are broadcast to all connected terminals.

```mermaid
flowchart LR
    CC["Claude Code<br/>(11 hook events)"]
    BS["Bridge Shim<br/>(one-shot Node.js)"]
    EN["Enrichment<br/>(transcript parsing,<br/>agent attribution)"]
    HS["Hook Socket<br/>(gravity-hooks.sock)"]
    HM["handleHookMessage"]
    SC["Stale Inbox<br/>Cleanup"]
    EH["handleEvent<br/>(event-handler.ts)"]
    SS["Session State<br/>(SessionStore)"]
    IM["InboxManager"]
    PA["Patches[]"]
    TB["terminals.broadcast"]
    OV["overview.snapshot"]
    TS["Terminal Socket<br/>(gravity-terminal.sock)"]
    EM["Emacs Client"]
    MB["Menu Bar"]

    CC -->|"stdin JSON"| BS
    BS --> EN
    EN -->|"enrichedData"| BS
    BS -->|"NDJSON"| HS
    HS --> HM
    HM --> SC
    SC -->|"remove stale<br/>inbox items"| IM
    HM --> EH
    EH -->|"mutations"| SS
    EH -->|"inbox.add<br/>(bidirectional)"| IM
    EH --> PA
    PA --> TB
    HM --> OV
    TB --> TS
    OV --> TS
    TS --> EM
    TS --> MB
```

---

## Diagram 2: Bidirectional Flow (PermissionRequest / AskUserQuestion)

The bidirectional path where the bridge keeps its connection open while the user responds in Emacs. The `InboxManager` holds the `PendingResponse` mapping the inbox item to the bridge's hook socket. When a terminal responds, the server writes back to the bridge, which writes to stdout for Claude Code.

```mermaid
sequenceDiagram
    participant CC as Claude Code
    participant BS as Bridge Shim
    participant HS as Hook Socket
    participant EH as handleEvent
    participant IM as InboxManager
    participant TS as Terminal Socket
    participant EM as Emacs Client

    CC->>BS: stdin (PermissionRequest)
    BS->>BS: enrichment (metadata, tool attribution)
    BS->>HS: hookMsg {needs_response: true}
    Note over BS: Bridge keeps socket open

    HS->>EH: handleEvent("PermissionRequest", ...)
    EH->>IM: inbox.add("permission", ..., hookSocket)
    Note over IM: PendingResponse created<br/>maps itemId → hookSocket
    EH-->>HS: patches (set_claude_status: idle)

    HS->>TS: broadcast(session.update, patches)
    HS->>TS: broadcast(inbox.added, item)
    TS->>EM: inbox.added

    Note over EM: User reviews in<br/>action buffer

    EM->>TS: action.permission {itemId, decision}
    TS->>IM: inbox.respond(itemId, response)
    IM->>HS: hookSocket.write(response)
    IM->>IM: remove pending + item
    TS->>EM: inbox.removed

    HS-->>BS: response JSON
    BS->>CC: stdout (hookSpecificOutput)
```

---

## Diagram 3: Event Handler Switch — Per-Event State Mutations

What each event type does to session state. Every event first updates meta and self-heals ended sessions, then dispatches to event-specific logic.

```mermaid
flowchart TB
    EH["handleEvent(eventName, ...)"]
    META["Update meta on every event<br/>(pid, slug, displayName, branch)<br/>+ self-heal ended→active"]

    EH --> META
    META --> SW{eventName}

    SW -->|SessionStart| SS_START["ensureSession / resetSession<br/>updateMeta, set modelName"]
    SW -->|SessionEnd| SS_END["sessionEnd<br/>(status=ended, claudeStatus=idle)"]
    SW -->|UserPromptSubmit| UPS["stripSystemXml<br/>dedup check (500ms)<br/>addPrompt, set displayName<br/>setClaudeStatus(responding)"]
    SW -->|Stop| STOP["setClaudeStatus(idle)<br/>finalizeLastPrompt(stop_text/thinking)<br/>setTokenUsage, finalizeTurnTokens<br/>inbox.add(idle)"]
    SW -->|PreToolUse| PRE["addTool(running)<br/>setPermissionMode<br/>setClaudeStatus(responding)<br/>trackFile, trackTask<br/>AskUserQuestion→addPrompt(question)"]
    SW -->|PostToolUse| POST["completeTool(done)<br/>trackFile, trackTask<br/>AskUserQuestion→updatePromptAnswer<br/>ExitPlanMode→setPlan+addPrompt(phase-boundary)<br/>updateTurnTokens"]
    SW -->|PostToolUseFailure| FAIL["completeTool(error)<br/>trackFile, trackTask"]
    SW -->|SubagentStart| AGS["addAgent(running)"]
    SW -->|SubagentStop| AGE["completeAgent<br/>(stop_text, stop_thinking, duration)"]
    SW -->|PermissionRequest| PERM["setClaudeStatus(idle)<br/>inbox.add(permission/plan-review, hookSocket)"]
    SW -->|AskUserQuestionIntercept| AUQ["setClaudeStatus(idle)<br/>inbox.add(question, hookSocket)"]
    SW -->|Notification| NOP["no-op"]
```

---

## Source Files

| Entity | File |
|--------|------|
| Bridge shim entry | `packages/emacs-bridge/src/index.ts` |
| Enrichment functions | `packages/emacs-bridge/src/enrich.ts` |
| Hook/terminal socket setup | `packages/gravity-server/src/gravity-server.ts` |
| `handleHookMessage` | `packages/gravity-server/src/gravity-server.ts:93` |
| `handleEvent` (event handler switch) | `packages/gravity-server/src/handlers/event-handler.ts:135` |
| `handleTerminalMessage` | `packages/gravity-server/src/gravity-server.ts:264` |
| `InboxManager` | `packages/gravity-server/src/state/inbox.ts:14` |
| `TerminalServer` | `packages/gravity-server/src/protocol/terminal-server.ts` |
| Session mutations | `packages/gravity-server/src/state/session.ts` |
| `SessionStore` | `packages/gravity-server/src/state/session-store.ts` |
