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

# roadmap

All open features, bugs, and tasks are tracked in beads. Run `bd ready` for available work, `bd list --status=open` for the full backlog (21 issues across P1–P4).
