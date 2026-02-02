We are building emacs UI for Claude Code that has some features similar to google's antigravity and cursor.

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
- Agent/subagent tracking with live status
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

# tech stack

we like emacs magit package and want to use similar UI paradigms and libraries. We are open to use org-mode, but not very keen.

# ideas, pending features to discuss

- show tools that asked for permissions (partially done: pattern generation + settings integration works, but no explicit "permission requested" indicator in UI)
- show more (what?) details and state for agents (currently only: type, status, short ID)
- show Edit tools diff (ideally color coded) — not started
- (done: inline preview + allowedPrompts + file path) 
  - `ExitPlanMode` tool does not show the suggested plan. I want it to show the suggested plan. see the plannotator plugin for example
- `AskUserQuestion` tool is treated as new prompt in the prompts section, but not as a new turn in the Tool Usage section. Let's fix that and also experiment with having the tool usage be sub-section of the prompt 
- show replies in conversation, for example when I ask claude in planning mode to analyze somthing, it answers and we're still in planning mode (and there's no explicit plan)
