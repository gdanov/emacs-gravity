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

  ┊ 1 tool
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
    ┊ 2 tools
      [x] Read codebase structure
        Glob(**/*.el)
      [x] Read main file
        Read(/Users/me/project/claude-gravity.el)

╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
❯ Refactor the rendering pipeline to support nested
  sections properly
  [2t 1a 8 tools]  2m34s

  ┊ 3 tools                                   [collapsed]

╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
  ┊ 5 tools

  ┊ Thinking...                                  (purple)
    I need to restructure the section insertion
    functions to handle arbitrary nesting...

  ┊ I'll update the rendering functions now.     (orange)

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

  ┊ 1 tool
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
message plus its tool calls. A `┊ N tools` heading groups them.

### Single cycle (expanded, current)

```
  ┊ 2 tools
    [x] Read project structure
      Glob(**/*.ts)
    [x] Read main module
      Read(/src/index.ts)
```

### Multiple cycles — earlier ones auto-collapsed

```
  ┊ 3 tools                                   [collapsed]

  ┊ 2 tools

  ┊ Thinking...
    Now I need to update the imports...

    [x] Update imports
      Edit(/src/index.ts)
    [/] Update tests                               [gold bg]
      Edit(/src/index.test.ts)
```

### Cycle with preceding assistant text

```
  ┊ Thinking...                                    (purple)
    The error is in the token validation. The expiry
    check is missing from the conditional...

  ┊ I'll fix the authentication check now.         (orange)

  ┊ 1 tool
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
    ┊ 2 tools
      [x] Read codebase
        Glob(**/*.el)
      [x] Read config
        Read(/.claude/settings.json)
```

### Turn with trailing assistant text

After all tools complete, the final assistant message:

```
  ┊ 1 tool
    [x] Run tests
      Bash(npm test)

  ┊ All tests pass. The fix correctly handles       (orange)
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

Agents appear as tool items with extra metadata when expanded:

```
    [x] Search for error handling patterns
      Task(Explore · find error handlers)  → Explore (a8f2)  3.1s

      Agent task: find error handlers
      Model: sonnet
      Agent tools: 12
      Transcript: /tmp/agent-a8f2.json
```

Running agent:

```
    [/] Analyze test coverage                      [gold bg]
      Task(Explore · check coverage)  → Explore (b3d1)
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
  orange      — assistant monologue text (┊ ...)
  purple      — assistant thinking (┊ ...)
  light green — tool description text (#88cc88)
  red         — stderr output

Status:
  green       — done [x], idle, completed tasks
  yellow      — running [/], responding, in-progress tasks
  gray        — ended ○, pending [ ], labels, signatures

UI chrome:
  white bold  — section headings, header title
  gray        — dividers (━ ── ╌), detail labels, tool signatures
  gray italic — tool signatures, activeForm text
  gold bg     — running tool/agent background highlight
```

---

## 10. Keyboard / Transient Menu (`?`)

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
