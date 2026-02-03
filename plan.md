We are building emacs UI for Claude Code that has features similar to google's antigravity and cursor. The aim is to be able to prompt cloude and trace all it's activity in our tool, so we don't need to be looking at the command-line tool output

# features

## working memory overview

we show the current plan, tasks, todos, etc. in nice way so that user has convenient overview and can navigate them.

## commenting on working memory

we allow user to open the current plan and comment/annotate it similarly to how antigravity allows commenting plan, todos and files.

## claude code plugin

we will produce a claude code plug-in (`emacs-bridge`) that hooks into lifecycle stages (e.g. `PostToolUse`) and pushes changes to Emacs via a Unix socket. Emacs will listen on this socket and update its UI.

## chatting with and "driving" claude code

we don't want to implement full-blown claude code IDE, as we already use two pretty good ones (claude-code and claude-code-ide). We may integrate with them later

# current state

## features

- Plugin hooks for 8 lifecycle events: PreToolUse, PostToolUse, Stop, UserPromptSubmit, SubagentStart, SubagentStop, SessionStart, SessionEnd
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

# tech stack

we like emacs magit package and want to use similar UI paradigms and libraries. We are open to use org-mode, but not very keen.

# ideas, pending features to discuss

## open

- show Edit tools diff (ideally color coded) — `old_string`/`new_string` are in `tool_input` but discarded by bridge. Render inline diff with `diff-mode` faces
- show tools that asked for permissions (partially done: pattern generation + settings integration works, but no explicit "permission requested" indicator in UI)
- `AskUserQuestion` tool is treated as new prompt in the prompts section, but not as a new turn in the Tool Usage section. Let's fix that and also experiment with having the tool usage be sub-section of the prompt
- error/failure visualization: failed tools show as "done" with no distinction. Add error status, red face/✗ icon, inline error excerpt, `n`/`p` keybindings to jump between errors
- tool timing: record timestamps on PreToolUse/PostToolUse, display duration per tool (e.g. "Read 0.3s", "Bash 12.4s")
- navigation & search: `]`/`[` next/prev turn, `}`/`{` next/prev tool, `S-TAB` collapse/expand all, `G` jump to latest, depth-level toggles (`1`/`2`/`3`)
- permission mode display: `permission_mode` field is sent by Claude Code but ignored. Show in session header (plan/acceptEdits/default), show mode transitions in timeline
- show the claude status bar info in the session status
- bidirectional communication (Emacs → Claude Code): currently one-way only. Enable answering AskUserQuestion, approving plans, sending prompts from Emacs
- session persistence: sessions lost on Emacs restart. Serialize state to file, offer restore on startup
- incremental buffer updates: currently full `erase-buffer` + redraw on every event. Track changed sections and re-render only those, or use magit-section in-place updates
- notification system: all non-reset notifications silently discarded. Display as timestamped entries, distinguish severity, surface status bar updates (tokens, cost, model)
- session source/resume tracking: `SessionStart` has `source` field (e.g. "resume") that's unused. Display fresh vs resumed, track lineage
- tool usage statistics: aggregate view — total tools by type, total duration, file touch frequency
- auto-parse agent transcripts: on SubagentStop, auto-parse and show key stats (tool count, duration, task) without requiring RET
- show more (what?) details and state for agents (currently: type, status, short ID, duration, transcript parsing with task/model/tool count)

## done

- ~~(done: inline preview + allowedPrompts + file path)~~
  - ~~`ExitPlanMode` tool does not show the suggested plan. I want it to show the suggested plan. see the plannotator plugin for example~~
- ~~(done: assistant monologue, thinking, and trailing text display)~~
  - ~~show replies in conversation, for example when I ask claude in planning mode to analyze something, it answers and we're still in planning mode (and there's no explicit plan)~~
- ~~(done: visual-line-mode + insert-wrapped helper for all long content: prompts, commands, paths, questions, answers, tool signatures, stderr, plan preview, permissions)~~
  - ~~wrap text such as the full prompts~~
- ~~(done: response cycle auto-collapse for completed cycles)~~
  - ~~tools/agents history is automatically expanded when there are no running tools/agents, not nice~~
- ~~(done: depth-aware indentation — top sections 0sp, sub-headings (Tools/Agents/Tasks/History) 2sp, items 4sp, body/labels 6sp, continuation/output 8sp; `insert-label` helper with optional indent arg; `indent-body`/`indent-continuation` constants for top-level contexts)~~
  - ~~improve left padding of the different section content, so that it matches their level~~
- ~~(done: debounced per-session rendering with 0.1s idle timer)~~
  - ~~session status buffer should automatically update~~
