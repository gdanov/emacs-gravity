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

- Plugin hooks for 6 lifecycle events: PreToolUse, PostToolUse, Stop, UserPromptSubmit, SubagentStart, SubagentStop
- Multi-session support with per-session buffers, grouped by project in overview
- Magit-section based overview and detail views with transient menus
- File tracking (read/edit/write operations per file)
- Agent/subagent tracking with live status
- User prompt history display
- Plan detection (via ExitPlanMode tool) and display
- Claude status indicator (idle/responding) per session

# tech stack

we like emacs magit package and want to use similar UI paradigms and libraries. We are open to use org-mode, but not very keen.
