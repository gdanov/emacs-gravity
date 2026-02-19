# UI Specification: emacs-gravity

Text-based sketches of every UI state, serving as the reference for the product.

---

## 1. Overview Buffer: `*Structured Claude Sessions*`

### All sessions collapsed (default)

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Structured Claude Sessions  ◆ 4 sessions
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

▶ emacs-gravity (2)
▶ my-webapp (1)
▶ dotfiles (1)
```

### Project expanded

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Structured Claude Sessions  ◆ 4 sessions
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

▼ emacs-gravity (2)
  ● fix-rendering   responding      [12 tools]
  ● add-hooks       idle 3m         [5 tools]
▶ my-webapp (1)
▶ dotfiles (1)
```

### Legend

```
●  = active session (yellow)
○  = ended session (gray)

Status:
  "responding"  (yellow)  — Claude is generating output
  "idle"        (green)   — waiting for user
  "idle 3m"     (green)   — idle with elapsed time (>1m)
  "idle 1h"     (green)   — idle with elapsed time (>1h)
```

### Capabilities Section (in Overview Buffer)

Appears below each project's sessions, showing all skills, agents, commands, and MCP servers available for the project (from plugins, project-local, and global scopes).

#### All capabilities collapsed (default)

```
▶ Capabilities (12 total)
```

#### Capabilities expanded, categories collapsed

```
▼ Capabilities (12 total)
  ▶ Plugins (2)
  ▶ Standalone Skills (3)
  ▶ Standalone Agents (1)
  ▶ Standalone Commands (2)
  ▶ MCP Servers (1)
```

#### Plugins expanded with nested capabilities

```
▼ Capabilities (12 total)
  ▼ Plugins (2)
    ▼ obsidian  (user) — 3 items
      ▼ Skills (2)
        ▶ S obsidian-markdown  (obsidian)
        ▶ S obsidian-bases  (obsidian)
      ▼ Commands (1)
        ▶ / obsidian-cli  (obsidian)
    ▶ beads  (user) — 5 items
  ▶ Standalone Skills (3)
```

#### Capability entry expanded (showing detail)

```
        ▼ S obsidian-markdown  (obsidian)
            Create and edit Obsidian Flavored Markdown with wikilinks, embeds, callouts...
            File: ~/.claude/plugins/obsidian/skills/obsidian-markdown/SKILL.md
```

#### Standalone categories expanded

```
▼ Capabilities (12 total)
  ▶ Plugins (2)
  ▼ Standalone Skills (3)
    ▶ S custom-skill  (project)
    ▶ S another-skill  (global)
    ▶ S test-skill  (project)
  ▼ Standalone Agents (2)
    ▶ A Explore  (built-in)
    ▶ A custom-agent  (project)
  ▼ Standalone Commands (2)
    ▶ / custom-cmd  (project)
  ▶ MCP Servers (1)
```

#### Legend

```
Prefixes:
  S    = Skill
  A    = Agent
  /    = Command (slash command)
  M    = MCP Server

Scope labels (in parentheses):
  (global)         = from ~/.claude/
  (project)        = from .claude/ in project
  (plugin-name)    = from installed plugin
  (built-in)       = for built-in agents (Explore, Plan, Bash, etc.)
  (user)           = user-scoped plugin

Plugin format:     PluginName  (scope) — N items
Category counts:   CategoryName (N)
Top-level count:   Capabilities (N total)
Description:       Truncated to 80 characters, shown when expanded
File path:         Shown when expanded (if applicable)
```

#### Interactions

| Key | Action |
|-----|--------|
| `TAB` | Toggle section expansion |
| `RET` | Navigate to capability file (if file-path exists) |
| Collapse behavior | Completed (non-running) sections auto-collapse when not focused |

#### Colors

```
Section headings (Plugins, Standalone Skills, etc.):
  Face: claude-gravity-section-heading (bold white)

Capability names:
  Skills/Agents: claude-gravity-tool-name (light green)
  Commands/MCP:  claude-gravity-tool-signature (gray italic)

Prefixes, scopes, descriptions, file paths:
  Face: claude-gravity-detail-label (gray)
```

---
## 2. Session Detail Buffer: `*Structured Claude Session <slug>*`

### Minimal session — one turn, one tool

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Structured Claude Session  ◆ 1 tool
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

── Turns (1) ────────────────────────────────────────────

❯ Fix the bug in the login handler
  [0t 0a 1 tools]  12s

  ▎ 1 tool
    [x] Fix authentication check
      Read(/src/auth/login.ts)
```

### Rich session — plan, multiple turns, agents, tasks

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Structured Claude Session  ◆ 18 tools
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

── Plan ─────────────────────────────────────────────────
  1. Explore the current rendering code
  2. Fix the section heading alignment
  3. Add word-wrapping to long tool descriptions
  4. Update tests
  ...
  Permissions: Bash(npm run build), Edit(src/**)
  File: /Users/me/.claude/plans/calm-river.md  (F to open)
  P to view full plan

── Turns (3) ────────────────────────────────────────────

  Pre-prompt activity  [0t 0a 2 tools]
    ▎ 2 tools
      [x] Read codebase structure
        Glob(**/*.el)
      [x] Read main file
        Read(/Users/me/project/claude-gravity.el)

╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
❯ Refactor the rendering pipeline to support nested
  sections properly
  [2t 1a 8 tools]  2m34s

  ▎ 3 tools                                   [collapsed]

╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
  ▎ 5 tools

  ▎ Thinking...                                  (purple)
    I need to restructure the section insertion
    functions to handle arbitrary nesting...

  ▎ I'll update the rendering functions now.     (orange)

    [x] Explore current rendering code
      Task(Explore · explore rendering)                4s
    [/] Update insert functions                    [gold bg]
      Edit(/Users/me/project/claude-gravity.el)

  Tasks (1/2)
    [x] Explore current rendering code
    [/] Update insert functions  Updating insert functions

╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
? Which approach do you prefer for nesting?
  [0t 0a 1 tools]  → Option A: recursive  45s

  ▎ 1 tool
    [x] Which approach do you prefer?
      AskUserQuestion
      Answer: Option A: recursive
```

---

## 3. Tool Items — All States

### Running tool (single-line, no description)

```
    [/] Read  /src/components/Button.tsx            [gold bg]
```

### Done tool (single-line, no description)

```
    [x] Glob  **/*.test.ts
```

### Done tool (two-line, with description)

```
    [x] Fix authentication check in login handler
      Edit(/src/auth/login.ts)
```

### Running tool (two-line, with description)

```
    [/] Install project dependencies                [gold bg]
      Bash(npm install)
```

### Task/Agent tool (two-line, with agent info)

```
    [x] Search for error handling patterns
      Task(Explore · find error handlers)  → Explore (a8f2)  3.1s
```

### Tool expanded (showing detail + output)

```
    [x] Fix authentication check in login handler
      Edit(/src/auth/login.ts)

      File: /src/auth/login.ts
      Output (3 lines):
        - if (user.isAuthenticated) {
        + if (user.isAuthenticated && !user.isExpired) {
             return next();
```

### Tool expanded — Bash with stderr

```
    [x] Run test suite
      Bash(npm test)

      Command: npm test
      Output (24 lines):
        PASS src/auth.test.ts
        PASS src/login.test.ts
          ✓ validates credentials (12ms)
          ✓ rejects expired tokens (8ms)
        ...
      Stderr:
        npm warn deprecated package@1.0
```

### Tool expanded — AskUserQuestion

```
    [x] Which testing framework to use?
      AskUserQuestion

      Question: Which testing framework should we use
               for the new API endpoints?
      Answer: Use vitest, it's already configured
```

---

## 4. Response Cycles (Tool Grouping)

Tools are grouped into "response cycles" — each cycle is one assistant
message plus its tool calls. A `▎ N tools` heading groups them.
The `▎` block character is colored to match the content type:
purple for thinking, orange for assistant text, gray for structural headings.

### Single cycle (expanded, current)

```
  ▎ 2 tools
    [x] Read project structure
      Glob(**/*.ts)
    [x] Read main module
      Read(/src/index.ts)
```

### Multiple cycles — earlier ones auto-collapsed

```
  ▎ 3 tools                                   [collapsed]

  ▎ 2 tools

  ▎ Thinking...
    Now I need to update the imports...

    [x] Update imports
      Edit(/src/index.ts)
    [/] Update tests                               [gold bg]
      Edit(/src/index.test.ts)
```

### Cycle with preceding assistant text

```
  ▎ Thinking...                                    (purple)
    The error is in the token validation. The expiry
    check is missing from the conditional...

  ▎ I'll fix the authentication check now.         (orange)

  ▎ 1 tool
    [x] Fix authentication check
      Edit(/src/auth/login.ts)
```

---

## 5. Turns

### Turn structure

Each turn starts with a dashed separator and a prompt line:

```
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
❯  User prompt text here                          (cyan)
   [Nt Ma P tools]  elapsed
```

### Prompt types

```
❯  Normal user prompt                             (cyan)
?  Question from Claude to user                   (magenta)
→  Phase boundary (plan approval)                 (gray)
```

### Turn heading counts

```
   [2t 1a 8 tools]  2m34s
    │   │  │          └─ elapsed time since prompt
    │   │  └─ total tool calls
    │   └─ M agents (subagents launched)
    └─ N tasks (TaskCreate calls)
```

### Pre-prompt activity (turn 0)

Work that happens before any user prompt:

```
  Pre-prompt activity  [0t 0a 2 tools]
    ▎ 2 tools
      [x] Read codebase
        Glob(**/*.el)
      [x] Read config
        Read(/.claude/settings.json)
```

### Turn with trailing assistant text

After all tools complete, the final assistant message:

```
  ▎ 1 tool
    [x] Run tests
      Bash(npm test)

  ▎ All tests pass. The fix correctly handles       (orange)
    expired tokens in the authentication flow.
```

---

## 6. Tasks Section (within a turn)

```
  Tasks (1/3)
    [x] Explore error handling patterns
    [/] Fix token validation  Fixing token validation
    [ ] Update integration tests
```

### Legend

```
[x]  completed  (green)
[/]  in_progress (yellow)  — activeForm text shown in gray italic
[ ]  pending    (gray)
```

Sort order: in_progress → pending → completed

---

## 7. Agents (within a turn)

Agents with live tool data render as **sub-branches** — their tools appear
nested inside the Task tool item using the same response-cycle format as
root tools. This provides full visibility into agent thinking and tool use.

### Agent sub-branch — completed (collapsed by default)

```
    [x] Search for error handling patterns
      Task(Explore · find error handlers)  → Explore (a8f2)  3.1s
```

When expanded, the agent's own tool history is shown:

```
    [x] Search for error handling patterns
      Task(Explore · find error handlers)  → Explore (a8f2)  3.1s

      ▎ 3 tools
        [x] Glob  **/*.ts
        [x] Read  /src/error-handler.ts
        [x] Grep  catch.*Error
```

### Agent sub-branch — running (expanded by default)

```
    [/] Analyze test coverage                      [gold bg]
      Task(Explore · check coverage)  → Explore (b3d1)

      ▎ Thinking...                                (purple)
        I need to check the test directory...

      ▎ 2 tools
        [x] Glob  **/*.test.ts
        [/] Read  /src/tests/auth.test.ts          [gold bg]
```

### Agent sub-branch — waiting for tools

```
    [/] Explore codebase structure                 [gold bg]
      Task(Explore · explore codebase)  → Explore (c4e2)
      Agent running...
```

### Nested agents (agent spawns sub-agent)

```
    [x] Research authentication patterns
      Task(general-purpose · research auth)  → general-purpose (d5f3)  45.2s

      ▎ 2 tools
        [x] Glob  **/*.ts
        [x] Explore auth implementation
          Task(Explore · find auth)  → Explore (e6g4)  12.1s

          ▎ 3 tools
            [x] Grep  authenticate
            [x] Read  /src/auth/index.ts
            [x] Read  /src/auth/middleware.ts
```

### Agent without tool data (legacy/flat view)

When no live tool data is available (agent completed before bridge
tracking was active), the flat metadata view is shown:

```
    [x] Search for error handling patterns
      Task(Explore · find error handlers)  → Explore (a8f2)  3.1s

      Agent task: find error handlers
      Model: sonnet
      Agent tools: 12
      Transcript: /tmp/agent-a8f2.json
```

---

## 8. Section Headings

All major sections use the embedded-title divider pattern:

```
── Plan ─────────────────────────────────────────────────
── Turns (3) ────────────────────────────────────────────
── Files (7) ────────────────────────────────────────────
── Allow Patterns (2) ──────────────────────────────────
```

The `──` extends to fill the window width.

---

## 9. Color Reference

```
Content:
  cyan        — user prompts (❯)
  magenta     — questions (?), answers
  orange      — assistant monologue text (▎ ...) — orange ▎ + orange text
  purple      — assistant thinking (▎ ...) — purple ▎ + purple text
  light green — tool description text (#88cc88)
  red         — stderr output

Margin indicators (▎):
  The ▎ block character color matches the content type:
  purple ▎    — thinking sections
  orange ▎    — assistant text
  gray ▎      — tool cycle headings and structural labels

Status:
  green       — done [x], idle, completed tasks
  yellow      — running [/], responding, in-progress tasks
  gray        — ended ○, pending [ ], labels, signatures

UI chrome:
  white bold  — section headings, header title
  gray        — dividers (━ ── ╌), detail labels, tool signatures
  gray italic — tool signatures, activeForm text
  gold bg     — running tool/agent background highlight

Agent demarcation:
  teal bg     — agent sub-branch background (#0a1a2a dark / #f0f5fa light)
  darker bg   — nested agent background (#0f2030 dark / #e8f0f5 light)
  (agents use same ▎ with content-type colors on teal background)
```

---

## 10. Plan Review Buffer: `*Claude Plan Review: <slug>*`

Opened when a `PermissionRequest` (matcher: `ExitPlanMode`) arrives. The buffer
shows the plan content as editable markdown. A minor mode
(`claude-gravity-plan-review-mode`) provides review-specific keybindings.

### Clean plan (no feedback)

```
*Claude Plan Review: calm-river*          PlanReview[C-c C-c:approve | C-c C-k:deny | c:comment]

## Implementation Plan

1. Explore the current rendering code
2. Fix the section heading alignment
3. Add word-wrapping to long tool descriptions
4. Update tests
```

### Plan with inline comment (`c`)

```
1. Explore the current rendering code
2. Fix the section heading alignment          « this should also fix indentation »
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        (orange wave underline on line)
3. Add word-wrapping to long tool descriptions
```

The comment overlay uses:
- Orange wave underline on the full line (`(:underline (:style wave :color "orange"))`)
- After-string: `  « comment text »` in orange italic

### Plan with `@claude` marker

```
3. Add word-wrapping to long tool descriptions
   @claude: also handle tool signatures, not just descriptions
4. Update tests
```

Markers matching `@claude:` (case-insensitive) are scanned on approve/deny.

### Actions

| Key | Action | Behavior |
|-----|--------|----------|
| `C-c C-c` | Approve | If no feedback: sends `allow`. If any edits, comments, or `@claude` markers exist: **auto-converts to deny** with structured feedback message |
| `C-c C-k` | Deny | Prompts for general comment, collects all feedback (comments, markers, diff, general comment), sends `deny` |
| `C-c C-d` | Diff | Shows unified diff between original and current buffer in `*Claude Plan Diff*` |
| `c` | Comment | Prompts for text, adds inline comment overlay on current line |

### Feedback message format (sent on deny / auto-deny)

```
# Plan Feedback

## Inline comments
- Line 5 (near "Fix the section heading"): this should also fix indentation

## @claude markers
- Line 8 (near "Add word-wrapping"): also handle tool signatures

## Changes requested
--- original
+++ edited
@@ -3,1 +3,2 @@
-3. Add word-wrapping to long tool descriptions
+3. Add word-wrapping to long tool descriptions and signatures

## General comment
Please also consider edge cases with very long file paths.
```

### Buffer lifecycle

- Buffer killed without decision → auto-sends deny
- After approve/deny, buffer is killed and process handle released

---

## 11. Keyboard / Transient Menu (`?`)

```
Actions
  c  Comment
  g  Refresh
  t  Tail
  P  Show Plan
  F  Open plan file
  T  Parse agent transcript
  V  Open agent transcript file

Permissions
  A  Copy allow pattern
  a  Add to settings

Sessions
  D  Remove ended sessions
  R  Reset all status to idle
  X  Detect dead sessions
  d  Delete session

Navigation
  RET  Visit or toggle
  TAB  Toggle section
```

### Plan Review Mode (`claude-gravity-plan-review-mode-map`)

Active in `*Claude Plan Review*` buffers:

```
C-c C-c  Approve plan (auto-denies if feedback detected)
C-c C-k  Deny plan with feedback
C-c C-d  Show diff vs original
c        Add inline comment on current line
m        Maximize window
```

