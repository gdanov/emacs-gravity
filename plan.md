We are building emacs UI for Claude Code that has features similar to google's antigravity and cursor. The aim is to be able to prompt cloude and trace all it's activity in our tool, so we don't need to be looking at the command-line tool output. Our mission is to present the full claude code conversation in structured, intuitive and navigatable way.

# features

## working memory overview

we show the current plan, tasks, todos, etc. in nice way so that user has convenient overview and can navigate them.

## commenting on working memory

we allow user to open the current plan and comment/annotate it similarly to how antigravity allows commenting plan, todos and files.

## claude code plugin

we will produce a claude code plug-in (`emacs-bridge`) that hooks into lifecycle stages (e.g. `PostToolUse`) and pushes changes to Emacs via a Unix socket. Emacs will listen on this socket and update its UI.

## chatting with and "driving" claude code

we don't want to implement full-blown claude code IDE, as we already use two pretty good ones (claude-code and claude-code-ide). We may integrate with them later

## previous art, i

### google antigravity

The IDE that made me go "a-ha". Good ideas (probably many taken from Cursor), bad implementation.

### plannotator

great claude code plug-in, I'm shamelessly copying it here.

# current state

## features

- Plugin hooks for 10 lifecycle events: PreToolUse, PostToolUse, Stop, UserPromptSubmit, SubagentStart, SubagentStop, SessionStart, SessionEnd, Notification, PermissionRequest
- Multi-session support with per-session buffers, grouped by project in overview
- Track session state (idle, responding, ended) with PID-based liveness detection
- Dead session detection: PID checks, staleness timeout (>5min), cleanup commands (D/d/R/X)
- Magit-section based overview and detail views with transient menus
- File tracking (read/edit/write operations per file)
- Agent/subagent tracking with live status, duration, transcript parsing (task, model, tool count via RET)
- Task tracking (TaskCreate/TaskUpdate/TaskList) with status indicators
- User prompt history display with elapsed time per turn
- Plan detection (via ExitPlanMode tool) and display in side buffer (markdown mode)
- Claude status indicator (idle/responding) per session with idle time in overview
- AskUserQuestion tool handled like prompt (with answer tracking)
- Tool usage grouped by prompt (turn-based)
- Tool permission signatures displayed (e.g. Bash(command), Edit(/path))
- Allow-pattern management: suggest patterns, copy to kill ring (A), write to settings.local.json (a)
- Comment system (overlay-based, mock/non-persistent)
- Debounced per-session rendering (0.1s timer)
- ExitPlanMode inline preview: collapsible 8-line plan preview in session buffer, allowedPrompts display, plan file path with F keybinding to open
- Assistant monologue & extended thinking display: extracted from Claude Code transcript, deduplicated across parallel tool calls, rendered in gray italic with ┊ margin indicator
- Response cycles: tools grouped into collapsible sections by assistant text boundaries; completed non-last cycles auto-collapse
- Trailing assistant text/thinking captured on Stop events, displayed after tools/agents/tasks per turn
- Visual section dividers (─ Title ──), turn separators (╌╌╌), margin indicators (┊)
- Running tool/agent background highlighting (subtle gold tint via `claude-gravity-running-bg` face)
- Inline agent annotations on Task tools ("→ AgentType (short-id) duration")
- Bidirectional PermissionRequest: plan review buffer with approve/deny/feedback flow (matcher: ExitPlanMode, 96h timeout)
- Plan review inline comments (`C-c ;`): orange wave-underline overlays with `« comment »` after-string
- Plan review `@claude` marker scanning: detects `@claude:` annotations in edited plan text
- Plan review smart approve (`C-c C-c`): if no feedback detected, sends clean allow. If edits, inline comments (`C-c ;`), or `@claude` markers are present, automatically sends deny with structured feedback instead — because the PermissionRequest allow channel cannot carry a payload, deny-with-message is the only way to deliver feedback back to Claude
- Plan review explicit deny (`C-c C-k`): always denies, prompts for optional general comment, bundles all feedback (diff + comments + markers + comment)
- Plan review diff view (`C-c C-d`): unified diff between original and edited plan
- Plan review transient menu (`C-c ?`): command palette for all plan review actions

# tech stack

we like emacs magit package and want to use similar UI paradigms and libraries. We are open to use org-mode, but not very keen.

# ideas, pending features to discuss

## open

* restore session state from json files when emacs is restarted and claude planning or other activity is already in progress
- transient keys that focus session state buffer on the latest turn - all previous are collapsed
- show Edit tools diff (ideally color coded) — `old_string`/`new_string` are in `tool_input` but discarded by bridge. Render inline diff with `diff-mode` faces
- show tools that asked for permissions (partially done: pattern generation + settings integration works, but no explicit "permission requested" indicator in UI)
- `AskUserQuestion` tool is treated as new prompt in the prompts section, but not as a new turn in the Tool Usage section. Let's fix that and also experiment with having the tool usage be sub-section of the prompt
- error/failure visualization: failed tools show as "done" with no distinction. Add error status, red face/✗ icon, inline error excerpt, `n`/`p` keybindings to jump between errors
- tool timing: record timestamps on PreToolUse/PostToolUse, display duration per tool (e.g. "Read 0.3s", "Bash 12.4s")
- navigation & search: `]`/`[` next/prev turn, `}`/`{` next/prev tool, `S-TAB` collapse/expand all, `G` jump to latest, depth-level toggles (`1`/`2`/`3`)
- permission mode display: `permission_mode` field is sent by Claude Code but ignored. Show in session header (plan/acceptEdits/default), show mode transitions in timeline
- show the claude status bar info in the session status
- bidirectional communication (Emacs → Claude Code): partially done — PermissionRequest (plan approve/deny with feedback) works. Still missing: answering AskUserQuestion, sending prompts from Emacs
- session persistence: sessions lost on Emacs restart. Serialize state to file, offer restore on startup
- incremental buffer updates: currently full `erase-buffer` + redraw on every event. Track changed sections and re-render only those, or use magit-section in-place updates
- notification system: all non-reset notifications silently discarded. Display as timestamped entries, distinguish severity, surface status bar updates (tokens, cost, model)
- session source/resume tracking: `SessionStart` has `source` field (e.g. "resume") that's unused. Display fresh vs resumed, track lineage
- tool usage statistics: aggregate view — total tools by type, total duration, file touch frequency
- auto-parse agent transcripts: on SubagentStop, auto-parse and show key stats (tool count, duration, task) without requiring RET
- show more (what?) details and state for agents (currently: type, status, short ID, duration, transcript parsing with task/model/tool count)

## hook & interaction gap analysis (Feb 2025)

Claude Code supports 12 hook events. We handle 10. Analysis of missing hooks, unused data fields, and bridge-level gaps.

### missing hook events (2 unregistered)

- **PostToolUseFailure** — fires when a tool execution fails. Fields: `tool_name`, `tool_use_id`, `tool_input`, `error`, `is_interrupt`. Enables: error/failure visualization (distinguishing failed tools from successful), user-interrupt detection. Directly unblocks the "error/failure visualization" open item.
- **PreCompact** — fires before context compaction (manual or auto). Fields: `trigger` (manual/auto), `custom_instructions`. Enables: tracking compaction events, warning user before compaction, preserving pre-compact state for session persistence.

**Note:** PermissionRequest was implemented — see current state features above. Bidirectional approve/deny with inline feedback flow is working.

### unused data fields (available but discarded)

**High value — directly enable open items:**
- **Token usage** (`message.usage.input_tokens`, `output_tokens`, `cache_creation_input_tokens`, `cache_read_input_tokens`) — available in transcript JSONL per API call, not extracted by bridge. Enables cost/usage display in status bar and per-turn stats. Unblocks "show claude status bar info" and "notification system" (cost/model surface).
- **Edit diffs** (`old_string`/`new_string` from Edit `tool_input`) — already arrive in tool_input, bridge forwards them, but elisp discards. Just needs UI rendering with `diff-mode` faces. Directly unblocks "show Edit tools diff" open item.
- **SessionStart `source`** field — values: "startup", "resume", "clear", "compact". Currently ignored. Directly unblocks "session source/resume tracking" open item.
- **SessionEnd `reason`** field — values: "clear", "logout", "prompt_input_exit", "bypass_permissions_disabled", "other". Currently ignored. Improves session lifecycle visibility.
- **`stop_reason`** from transcript — whether Claude stopped normally (`end_turn`) or hit token limit (`max_tokens`). Not extracted. Useful for "why did Claude stop?" visibility.
- **Task dependencies** (`blockedBy`, `blocks`, `owner`, `metadata`) from TaskCreate/TaskUpdate/TaskList — parsed by bridge but not stored in elisp task tracking. Enables dependency graph in task view.

**Medium value — improve existing features:**
- **Notification types** beyond reset/clear — `permission_prompt`, `idle_prompt`, `auth_success`, `elicitation_dialog` all silently dropped. Only reset/clear regex checked. Full notification content not stored.
- **`permission_mode`** — sent on every event, stored on session but not prominently displayed. Values: "default", "plan", "acceptEdits", "dontAsk", "bypassPermissions". Unblocks "permission mode display" open item.
- **`model`** on SessionStart — which model is active. Could display in session header.
- **`git_branch`** from transcript metadata — show branch context per session.
- **`is_interrupt`** on PostToolUseFailure — distinguish user cancellation from actual errors.

**Lower value — metadata enrichment:**
- `version` — Claude Code version
- `slug` — session naming
- `parentUuid`/`isSidechain` — message threading
- `thinkingMetadata.maxThinkingTokens` — thinking budget visibility

### bridge-level gaps

- Bridge only enriches **PreToolUse** and **Stop** from transcript. Could also enrich PostToolUse/PostToolUseFailure with error context.
- Transcript entry types `progress`, `file-history-snapshot`, `summary` are discarded — progress events could feed a progress indicator.
- No token extraction: transcript JSONL has `message.usage` per API call — bridge should aggregate and forward per-turn totals.
- Edit `tool_input` with `old_string`/`new_string` passes through bridge but elisp doesn't render diffs.

### implementation priority (recommended order)

1. Register **PostToolUseFailure** hook + handle in elisp → error visualization
2. Extract **token usage** in bridge from transcript → cost/stats display
3. Render **Edit diffs** from existing `tool_input` data → inline diffs
4. Use **SessionStart `source`** and **SessionEnd `reason`** → lifecycle visibility
5. Register **PreCompact** hook → compaction tracking
6. Expand **Notification handling** → surface notification types/content
7. Add **tool timing** → capture timestamps in elisp Pre/PostToolUse handlers

## done

- ExitPlanMode inline preview + allowedPrompts + file path
- Assistant monologue, thinking, and trailing text display
- Visual-line-mode + insert-wrapped helper for all long content
- Response cycle auto-collapse for completed cycles
- Depth-aware indentation (top sections 0sp, sub-headings 2sp, items 4sp, body 6sp, continuation 8sp)
- Debounced per-session rendering (0.1s idle timer)
- PermissionRequest bidirectional flow with plan review feedback (inline comments, @claude markers, auto-deny, diff view)
