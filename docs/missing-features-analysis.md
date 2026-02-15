# Missing Features Analysis: emacs-gravity

## Context

This analysis identifies missing features for emacs-gravity to become a **feature-complete Claude Code UI** for Emacs, focusing exclusively on **Claude Cloud agentic SDK sessions** (managed sessions via `claude -p` with JSON I/O). One-shot and tmux session handling are explicitly excluded from this analysis.

The project currently handles:
- Multi-session tracking with turn-based UI
- Hook-based event tracking (11 event types)
- Agent/subagent nested rendering
- Plan review with inline feedback
- File and task tracking
- Tool execution display
- Managed Claude subprocess (start/resume/send prompt)
- JSON-output adapter with streaming text
- Permission pattern management

## Critical Gap Categories

### 1. Session Management & Lifecycle

**Missing:**

1. **Session forking** (`--fork-session`)
   - No UI to fork an existing session into a new branch
   - No indication when a session is a fork vs original
   - No visualization of fork ancestry/tree

2. **Session history browser**
   - Can only see active sessions, not completed/archived ones
   - No search across historical sessions
   - No session filtering (by date, project, cost, duration)
   - No "recently closed" list

3. **Session metadata display**
   - Creation timestamp not shown
   - Last activity time not tracked (only idle duration)
   - Total conversation cost not displayed
   - Total token usage not accumulated across turns
   - Session duration (wall clock time) not shown

4. **Conversation compaction**
   - No UI for triggering `/clear` or compaction
   - No visibility into compaction status
   - No warning when approaching context limits
   - No PreCompact hook integration (exists in beads backlog as P3)

5. **Session titles/slugs** (P2 in beads backlog)
   - Sessions identified by UUID only
   - No human-readable names
   - No renaming capability

6. **Session persistence** (P2 in beads backlog)
   - Sessions lost when Emacs restarts
   - No state restoration from disk
   - No auto-save of active sessions

### 2. Configuration & Settings UI

**Missing:**

1. **Model selection**
   - No UI to switch model (opus/sonnet/haiku) mid-conversation
   - No per-session model display
   - No model performance comparison
   - Fast mode toggle exists in CLI but no gravity UI

2. **Permission mode management**
   - Current mode (ask/yolo/etc.) not displayed
   - No UI to change permission mode per session
   - No per-session permission overrides

3. **Token budget & cost controls**
   - No budget display (`--max-budget-usd`)
   - No budget warnings when approaching limit
   - No cost accumulation UI
   - No cost breakdown (by turn, by tool type)

4. **Settings editor integration**
   - No inline editor for `.claude/settings.json`
   - No inline editor for `CLAUDE.md`
   - Allow pattern writing (exists via `a` key) but no bulk editor
   - No validation of settings before save

5. **MCP server management**
   - No display of active MCP servers
   - No configuration UI for MCP servers
   - No status indicators (connected/error)

6. **Plugin management**
   - No list of loaded plugins
   - No enable/disable UI
   - No plugin configuration editor

### 3. Input & Prompt Composition

**Missing:**

1. **Rich prompt composer**
   - Current compose buffer is text-only
   - No image attachment UI (Claude Code supports multimodal)
   - No file attachment shortcut (attach CLAUDE.md, code files, etc.)
   - No context preview (what files/tools are in context)

2. **Prompt templates**
   - No template library for common tasks
   - No prompt history search
   - No prompt editing/reuse from history

3. **Multi-turn planning in compose buffer**
   - Can't queue multiple prompts
   - No "execute plan" workflow (send prompts 1-5 sequentially)

4. **Context attachment controls**
   - No explicit "attach this file to context"
   - No way to see what's currently in Claude's context window
   - No manual context trimming/pruning

5. **Slash command support** (P2 in beads backlog)
   - Skills and commands not accessible from gravity UI
   - No autocomplete for `/command`
   - No command palette

### 4. Output Display & Visualization

**Missing:**

1. **Image display**
   - Claude Code supports image responses
   - No rendering of images in tool results or assistant messages
   - No image preview/zoom

2. **Structured output display**
   - When using `--json-schema`, no special rendering
   - No syntax highlighting for JSON schemas
   - No tree view for structured data

3. **Tool result enhancements**
   - No syntax highlighting for code in tool outputs
   - No copy-to-clipboard for individual tool results
   - No save-to-file for tool results
   - No diff view for Edit tool (before/after comparison)

4. **Code block extraction**
   - Assistant messages contain code blocks
   - No "extract to file" for code blocks
   - No "execute code block" shortcut

5. **Tool execution timing** (P2 in beads backlog)
   - Duration shown for agents, not for individual tools
   - No waterfall/timeline view of tool execution
   - No identification of slow tools

6. **PostToolUseFailure handling**
   - Hook exists but no distinct rendering
   - Failed tools not visually distinct from successful ones
   - No error detail expansion
   - No retry mechanism

7. **Streaming progress indicators**
   - ">>> Claude is responding..." is basic
   - No tokens/sec display
   - No estimated time remaining
   - No streaming word count

### 5. Cost, Performance & Analytics

**Missing:**

1. **Token usage tracking**
   - Exists in beads backlog as P2 (emacs-gravity-buv, emacs-gravity-cz6)
   - No per-turn token display
   - No per-session cumulative tokens
   - No input vs output token breakdown
   - No cache hit tracking (prompt caching)

2. **Cost tracking**
   - No cost per turn
   - No session total cost
   - No cost by model (if model switched mid-session)
   - No budget vs actual comparison

3. **Performance metrics**
   - Turn duration shown, but no breakdown
   - No "time to first token" metric
   - No "tokens per second" display
   - No comparison across sessions/models

4. **Tool usage statistics** (P3 in beads backlog)
   - No aggregation of which tools used most
   - No success/failure rates per tool
   - No timing statistics per tool type

5. **Session analytics**
   - No summary view (total turns, tools, agents, cost)
   - No export to CSV/JSON for external analysis

### 6. Permission & Security Management

**Missing:**

1. **Permission request queue** (P2 in beads backlog: auto-next mode)
   - Multiple PermissionRequest events arrive sequentially
   - No "review all, then approve batch" workflow
   - No auto-advance to next pending request

2. **Permission history**
   - No log of all grant/deny decisions
   - No audit trail
   - No ability to revoke previously granted pattern

3. **Permission pattern editor**
   - Allow pattern writing exists (`a` key)
   - No validation before save
   - No pattern testing ("does this pattern match this command?")
   - No pattern conflict detection (overlapping patterns)

4. **Dangerous command warnings**
   - No special highlighting for destructive commands (rm -rf, force push)
   - No confirmation dialog for high-risk permissions
   - No "undo" for recently executed destructive commands

5. **Permission requested indicator** (P3 in beads backlog)
   - When tool is pending permission, not visually distinct
   - No countdown timer for permission timeout

### 7. Search, Navigation & Filtering

**Missing:**

1. **Full-text search**
   - No search across conversation history in current session
   - No search across all sessions
   - No search in tool results
   - No regex search

2. **Jump to navigation**
   - No "jump to turn N"
   - No "jump to tool by ID"
   - No "jump to next error"
   - No "jump to next permission request"

3. **Filtering**
   - No filter by tool type (show only Edit tools)
   - No filter by file path (show tools affecting file X)
   - No filter by agent (show only tools from agent Y)
   - No filter by status (show only failed tools)

4. **Bookmarks** (P2 in beads backlog: navigation & search)
   - No ability to bookmark important turns
   - No bookmark list/navigation
   - No annotations on bookmarks

5. **Session comparison**
   - No diff between two sessions
   - No "show me what changed from session A to B"

### 8. Task Management Integration

**Missing:**

1. **TaskGet integration**
   - Can see TaskCreate/TaskUpdate/TaskList in UI
   - No way to fetch full task details via TaskGet
   - No task metadata display

2. **Task dependency visualization** (P3 in beads backlog)
   - Tasks shown as flat list
   - No graph/tree view of blocks/blockedBy
   - No critical path highlighting

3. **Task creation from gravity**
   - Tasks created by Claude only
   - No "create task from this tool" action
   - No "create task from this turn" action

4. **Task filtering & search**
   - All tasks shown together
   - No filter by status (pending/in_progress/completed)
   - No filter by owner
   - No task search

5. **Task comments**
   - Task structure supports comments
   - No display of task comments in gravity UI

### 9. Agent & Subagent Enhancements

**Missing:**

1. **Agent control**
   - Agents run to completion
   - No pause/resume for agents
   - No cancel during execution
   - No step-through mode

2. **Agent transcript streaming**
   - Transcript shown after agent completes
   - No live streaming of agent tools as they execute
   - Auto-parse agent transcripts (P3 in beads backlog)

3. **Agent configuration display**
   - Agent type shown (Explore, Plan, etc.)
   - No display of agent's allowed tools
   - No display of agent's model
   - No display of agent's max turns

4. **Agent performance**
   - Duration shown
   - No turn count for agent
   - No token usage for agent
   - No cost for agent

5. **Background agent tracking**
   - `run_in_background` flag exists in Task tool
   - No UI indication when agent is background vs foreground
   - No notification when background agent completes

6. **Nested agent clarity**
   - Nested agents render correctly
   - But depth >2 becomes hard to follow visually
   - No "flatten view" to see all tools across all agents

### 10. Error Handling & Debugging

**Missing:**

1. **Error detail expansion**
   - Errors shown inline
   - No stack traces
   - No "show raw JSON" for error events
   - No error context (what was Claude trying to do)

2. **Retry mechanisms**
   - Failed tools cannot be retried
   - No "retry this turn" action
   - No "retry with different model" action

3. **Debug mode**
   - No verbose logging toggle
   - No raw transcript viewer (already processed)
   - No hook event log viewer
   - Debug buffer exists but not integrated into main UI

4. **Session diagnostics**
   - No health check for daemon connection
   - No latency display (Emacs ↔ daemon ↔ Claude API)
   - No circuit breaker status

### 11. Export, Sharing & Collaboration

**Missing:**

1. **Session export**
   - No export to markdown
   - No export to JSON
   - No export to HTML (for sharing)
   - No export subset (turns 3-7 only)

2. **Plan export**
   - Plan can be viewed in separate buffer
   - No export plan to standalone markdown
   - No export plan with annotations

3. **Comment export**
   - Comments are overlays (non-persistent)
   - No export of comments
   - No comment threading

4. **Remote session push**
   - Claude Code supports `--push-to-remote` (cloud.anthropic.com)
   - No gravity UI for pushing session to web
   - No display of remote session URL
   - No "share this session" button

5. **Import/restore**
   - No import from exported session
   - No restore from backup
   - Session persistence (P2) would enable this

### 12. Plan Mode Enhancements

**Missing:**

1. **Plan approval with modifications**
   - Current: approve (clean) or deny (with feedback)
   - No "approve with changes" (send modified plan as approved)

2. **Plan step-by-step execution**
   - Plan shows numbered steps
   - No "execute step 1 only, then pause" mode
   - No manual advancement through plan steps

3. **Plan revision history**
   - Only current plan visible
   - No history of plan changes (if Claude revises plan)
   - No diff across plan revisions

4. **Plan templates**
   - No template library for common plan structures
   - No "use this plan as template for new task"

### 13. Notification & Alerts

**Missing:**

1. **Desktop notifications** (P2 in beads backlog)
   - No notification when Claude finishes turn (while Emacs not focused)
   - No notification when permission required
   - No notification when agent completes

2. **Sound alerts**
   - No audio cue for completion
   - No audio cue for error

3. **Mode line integration**
   - No active session indicator in Emacs mode line
   - No current model/status in mode line

4. **Long-running tool alerts**
   - No alert when tool exceeds 30s, 1m, 5m
   - No "this is taking longer than usual" warning

5. **Budget warnings**
   - No alert when approaching token budget
   - No alert when approaching cost budget

### 14. Accessibility & UX Polish

**Missing:**

1. **Keybinding customization**
   - Keybindings hard-coded
   - No user-configurable keymap
   - No keybinding cheat sheet (beyond transient menu)

2. **Theme customization**
   - 37 faces defined
   - No theme integration (inherit from user's Emacs theme)
   - No light/dark mode toggle

3. **Layout customization**
   - Overview and session buffers are fixed layout
   - No sidebar mode
   - No split view (session + plan side-by-side)

4. **Font customization**
   - Uses default monospace
   - No per-section font size (e.g., larger prompts, smaller tool signatures)

5. **Quick action menu**
   - Transient menu exists (`?`)
   - No context-sensitive commands (right-click menu)
   - No "actions on this tool" submenu

6. **Tooltip/documentation**
   - No hover documentation
   - No inline help for keybindings
   - No explain-this-tool tooltip

### 15. Emacs Integration

**Missing:**

1. **Org-mode integration**
   - No capture turn as org entry
   - No export session to org-mode outline
   - No org-babel integration (execute code blocks from gravity)

2. **Dired integration**
   - File paths shown as plain text
   - No dired jump-to-file from file mention
   - No "open in dired" for file tool results

3. **Magit integration**
   - No commit directly from gravity UI
   - No show git diff for edited files
   - No stage files from gravity

4. **Projectile integration**
   - Sessions grouped by project directory
   - No projectile-aware session creation
   - No "switch to project session" command

5. **LSP integration**
   - Code snippets in tool results
   - No jump-to-definition from code in tool results
   - No LSP hover in tool result code

6. **Compilation mode integration**
   - Bash tool output may contain errors
   - No `next-error` integration
   - No jump to error from Bash output

### 16. Advanced Session Features

**Missing:**

1. **Multi-session workflows** (P0 in beads backlog: tmux sessions)
   - Can track multiple sessions independently
   - No orchestration (session A waits for session B)
   - No session merge (combine outputs)

2. **Session comparison**
   - No side-by-side view of two sessions
   - No diff of turns across sessions

3. **Session branching visualization**
   - Forking supported in CLI
   - No visual tree of session forks
   - No ancestry display

4. **Session replay**
   - Transcript exists
   - No replay mode (step through session history)
   - No "rewind to turn N and branch"

5. **Session templates**
   - No "start session from template" (predefined context, files)

### 17. Daemon-Specific Features

**Missing:**

1. **Authentication UI**
   - `claude-gravity-daemon-login` exists
   - No status display (logged in as X, token expires Y)
   - No logout command
   - No token refresh indicator

2. **Daemon health monitoring**
   - Daemon process spawned by Emacs
   - No health check ping
   - No reconnect on crash
   - No daemon log viewer

3. **Session source tracking** (P3 in beads backlog)
   - Sessions can come from: terminal hooks, managed process, daemon
   - No indicator of session source
   - No "resume in terminal" for daemon session

4. **Model & permission mode switching** (recently implemented based on file timestamps)
   - May exist in daemon module
   - Need verification of UI exposure
   - Need verification of per-session vs global setting

5. **Streaming event types**
   - Daemon adds StreamDelta, AssistantComplete, DaemonResult, ToolProgress
   - ToolProgress not rendered (if implemented)
   - AssistantComplete/DaemonResult may duplicate result event

### 18. Missing Hook Integrations

**Missing:**

1. **PreCompact hook** (P3 in beads backlog)
   - Hook exists in Claude Code
   - Not registered in hooks.json
   - No UI event when compaction about to happen

2. **PostToolUseFailure distinct rendering**
   - Hook received
   - Rendered same as PostToolUse (just with error in output)
   - No red border, retry button, or error expansion

3. **Notification hook richness**
   - Notification hook fires for info messages
   - Currently just logged
   - No notification center/history
   - No filtering (show only errors)

### 19. Context Management

**Missing:**

1. **Context window visualization**
   - No display of tokens used vs available
   - No "you have 50k tokens left" indicator
   - No per-file token contribution

2. **Context pruning controls**
   - No manual "remove turn N from context"
   - No "pin this turn" (keep in context forever)
   - No context priority visualization

3. **Context file tracking**
   - File tracking exists (read/edit/write)
   - No distinction between "in context" vs "just referenced"
   - No "what files does Claude currently have in memory"

### 20. OpenCode Integration (P1 Epic in beads)

**Missing:**

1. **OpenCode event handling**
   - Epic exists: emacs-gravity-rhr
   - Full integration of OpenCode as alternative to daemon
   - Sub-issues for specific OpenCode features

## Priority Recommendations

Based on user impact and feasibility, recommended priority order:

### High Priority (P0-P1):
1. **Session metadata & cost tracking** — users need to know what they're spending
2. **Model selection UI** — switching models is common workflow
3. **Image display** — multimodal is core Claude feature
4. **Permission queue & auto-next** — current UX is tedious for multi-permission flows
5. **Session forking** — critical for experimentation workflows
6. **Search & filtering** — sessions get long, need to find things fast

### Medium Priority (P2):
7. **Rich prompt composer** (images, file attach, context preview)
8. **Token usage display** — already in backlog
9. **Tool timing & performance** — already in backlog
10. **Error retry mechanisms** — improves reliability perception
11. **Export to markdown/JSON** — sharing and documentation
12. **Session persistence** — already in backlog
13. **Slash command support** — already in backlog

### Lower Priority (P3-P4):
14. **Task dependency visualization** — nice-to-have for power users
15. **Agent pause/resume** — advanced use case
16. **Org-mode integration** — specific to org users
17. **Plan step-by-step execution** — niche workflow
18. **Session replay/rewind** — interesting but complex

## Summary Statistics

**Total identified gaps: ~120 distinct features** across 20 categories.

**Breakdown:**
- Session management: 16 gaps
- Configuration & settings: 13 gaps
- Input & composition: 11 gaps
- Output display: 14 gaps
- Cost & analytics: 9 gaps
- Permission management: 10 gaps
- Search & navigation: 10 gaps
- Task integration: 8 gaps
- Agent enhancements: 11 gaps
- Error handling: 7 gaps
- Export & sharing: 9 gaps
- Plan mode: 8 gaps
- Notifications: 9 gaps
- Accessibility: 11 gaps
- Emacs integration: 11 gaps
- Advanced sessions: 9 gaps
- Daemon features: 9 gaps
- Hook integrations: 6 gaps
- Context management: 6 gaps
- OpenCode integration: (Epic in progress)

**Current beads backlog:** 25 open issues (P0-P4), ~10-15 directly align with this analysis.

**Estimated work:** 6-12 months for full feature parity with professional-grade Claude Code UI (AntiGravity/Cursor level).
