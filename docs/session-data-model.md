# Session Data Model

Reference documentation for how session transcript state is stored in-memory in Emacs.

## Session Plist

Stored in `claude-gravity--sessions` hash table (session-id → plist).
Created in `claude-gravity-session.el:83` (`--ensure-session`).

```
Session (plist)
├── :session-id      string     — UUID from Claude Code
├── :cwd             string     — project working directory
├── :project         string     — basename of cwd
├── :slug            string|nil — human-readable name
├── :status          symbol     — active | ended
├── :claude-status   symbol     — idle | responding
├── :pid             integer|nil
├── :start-time      time
├── :last-event-time time
├── :current-turn    integer    — monotonic, 0-based
├── :permission-mode string|nil
├── :buffer          buffer|nil — session detail buffer
│
│  Turn tree (hierarchical — mirrors screen layout):
├── :turns           tlist      — tlist of turn-node alists
│
│  Indexes (O(1) lookup):
├── :tool-index      hash-table — tool_use_id → tool alist (pointer)
├── :agent-index     hash-table — agent_id → agent alist (pointer)
│
│  Other collections:
├── :files           hash-table — filepath → { ops, last-touched }
├── :tasks           hash-table — taskId → task alist
│
├── :plan            plist|nil  — { :content, :file-path, :allowed-prompts }
├── :token-usage     alist|nil  — { input_tokens, output_tokens, cache_* }
├── :allow-patterns  list|nil
└── :notifications   list
```

## Turn Tree

The primary data structure. Mirrors the screen layout: Session → Turns → Cycles → Tools.
Hooks write directly to the tree at insertion time; the renderer iterates it without grouping or dedup.

Defined in `claude-gravity-state.el:276`.

```
:turns (tlist)
└── turn-node (alist)
    ├── turn-number    integer    — 0-based, monotonic
    ├── prompt         alist|nil  — user prompt for this turn (see below)
    ├── cycles         tlist      — response cycles (see below)
    ├── agents         tlist      — agents spawned in this turn
    ├── tasks          list       — task alists for this turn
    ├── tool-count     integer    — root tools in this turn (pre-computed)
    ├── agent-count    integer    — agents in this turn (pre-computed)
    ├── frozen         bool|nil   — turn is complete (no more tools)
    ├── stop_text      string|nil — trailing assistant text (set by Stop)
    └── stop_thinking  string|nil — trailing thinking (set by Stop)
```

Turn 0 is always pre-allocated for "pre-prompt activity" (tools that run before any user prompt).

### Cycle Node

A response cycle groups tools under a single assistant text/thinking block. Cycle boundaries are detected at insertion time in `claude-gravity--tree-add-tool` — the renderer just iterates.

```
cycle-node (alist)
├── thinking   string|nil  — assistant thinking for this cycle
├── text       string|nil  — assistant monologue for this cycle
└── tools      tlist       — tool alists in this cycle
```

New cycle is created when:
- No current cycle exists (first tool in turn)
- New `assistant_text` differs from current cycle's text
- New `assistant_thinking` differs from current cycle's thinking

## Prompt Alist

Stored as the `prompt` key of a turn-node. Created by UserPromptSubmit, AskUserQuestion, ExitPlanMode in `claude-gravity-events.el`.

```
├── text            string     — user prompt (XML-stripped)
├── submitted       time       — when sent
├── elapsed         float|nil  — seconds to Stop (set by model-finalize-last-prompt)
│
│  Question prompts only:
├── type            symbol     — 'question | 'phase-boundary | nil
├── tool_use_id     string     — links to AskUserQuestion tool
└── answer          string|nil
```

## Tool Alist

In a cycle's `:tools` tlist. Created by PreToolUse, completed by PostToolUse.

```
├── tool_use_id       string     — unique ID
├── name              string     — "Read", "Edit", "Bash", "Task", etc.
├── input             alist      — tool-specific params
├── status            string     — "running" | "done" | "error"
├── result            any|nil    — response (set on PostToolUse)
├── timestamp         time
├── turn              integer
├── permission_mode   string|nil
│
│  Transcript context (bridge-extracted):
├── assistant_text     string|nil — text BEFORE this tool
├── assistant_thinking string|nil — thinking BEFORE this tool
├── post_text          string|nil — text AFTER this tool
├── post_thinking      string|nil — thinking AFTER this tool
│
├── parent_agent_id   string|nil — owning agent (nil = root)
├── ambiguous         bool|nil
├── candidate-agents  list|nil
│
│  Agent link (set by --link-agent-to-task-tool for Task tools):
└── agent             alist|nil  — pointer to agent alist (bidirectional)
```

## Agent Alist

In a turn-node's `:agents` tlist. Created by SubagentStart, completed by SubagentStop.

Agents have their own `:cycles` tlist (same structure as turn-node cycles), enabling nested tool rendering.

```
├── agent_id          string     — unique ID
├── type              string     — "Explore", "general-purpose", "Plan", etc.
├── status            string     — "running" | "done"
├── timestamp         time
├── turn              integer
├── duration          float|nil  — seconds (set on SubagentStop)
├── cycles            tlist      — agent's own response cycles (same as turn cycles)
├── tool-count        integer    — number of tools in this agent
├── transcript_path   string|nil
├── stop_text         string|nil — agent summary text
├── stop_thinking     string|nil — agent final thinking
│
│  Bidirectional link:
├── task-tool         alist|nil  — pointer to spawning Task tool
│
│  Set by RET → parse transcript:
├── transcript_parsed      bool|nil
├── transcript_prompt      string|nil
├── transcript_model       string|nil
└── transcript_tool_count  integer|nil
```

## tlist (O(1) Append List)

Defined in `claude-gravity-core.el:158`. A cons cell `(items-head . tail-pointer)` enabling O(1) append.

```elisp
(claude-gravity--tlist-new)          → empty tlist
(claude-gravity--tlist-append tl x)  → append item, O(1)
(claude-gravity--tlist-items tl)     → list of items
(claude-gravity--tlist-last-item tl) → last appended item
```

## Full Session Diagram

Diagram showing the tree structure with indexes:

```
claude-gravity--sessions (hash-table)
│
└── "session-uuid" ───────────────────────────────────────────────
    │
    │  Session Plist
    │  :status=active  :claude-status=responding  :current-turn=2
    │
    ├── :turns (tlist) ───────────────────────────────────────────
    │   │
    │   ├─ turn-node [turn-number=0]  "Pre-prompt activity"
    │   │   ├── prompt: nil
    │   │   ├── cycles (tlist)
    │   │   │   └─ cycle-0
    │   │   │       ├── thinking: nil
    │   │   │       ├── text: nil
    │   │   │       └── tools (tlist)
    │   │   │           ├── {id:"tu_001" name:"Glob"  status:"done"}
    │   │   │           └── {id:"tu_002" name:"Read"  status:"done"}
    │   │   ├── agents (tlist): []
    │   │   ├── tasks: []
    │   │   └── tool-count: 2
    │   │
    │   ├─ turn-node [turn-number=1]
    │   │   ├── prompt: {text:"Fix the auth bug" elapsed:45.2}
    │   │   ├── cycles (tlist)
    │   │   │   ├─ cycle-0
    │   │   │   │   ├── thinking: nil
    │   │   │   │   ├── text: "Let me find the files..."
    │   │   │   │   └── tools (tlist)
    │   │   │   │       ├── {id:"tu_003" name:"Glob" status:"done"}
    │   │   │   │       └── {id:"tu_004" name:"Read" status:"done"}
    │   │   │   └─ cycle-1
    │   │   │       ├── thinking: nil
    │   │   │       ├── text: "I see the issue..."
    │   │   │       └── tools (tlist)
    │   │   │           ├── {id:"tu_005" name:"Edit" status:"done"}
    │   │   │           └── {id:"tu_006" name:"Bash" status:"done"
    │   │   │                 post_text:"Tests pass."}
    │   │   ├── agents (tlist)
    │   │   │   └── {agent_id:"ag_01" type:"Explore" status:"done"
    │   │   │         duration:3.1 task-tool:→tu_007
    │   │   │         cycles (tlist)
    │   │   │           └─ cycle-0
    │   │   │               └── tools: [{name:"Glob"} {name:"Read"}]}
    │   │   ├── tasks: [{subject:"Fix auth" status:"completed"}]
    │   │   ├── stop_text: "All tests pass now."
    │   │   └── tool-count: 4  agent-count: 1
    │   │
    │   └─ turn-node [turn-number=2]
    │       ├── prompt: {text:"Also update the docs"}
    │       ├── cycles (tlist)
    │       │   └─ cycle-0
    │       │       ├── text: "Now for the docs..."
    │       │       └── tools (tlist)
    │       │           ├── {id:"tu_008" name:"Read" status:"done"}
    │       │           └── {id:"tu_009" name:"Edit" status:"running"}
    │       └── tool-count: 2
    │
    ├── :tool-index (hash-table) ─────────────────────────────────
    │   "tu_001" → ● (pointer to tool in turn-0/cycle-0)
    │   "tu_003" → ● (pointer to tool in turn-1/cycle-0)
    │   "tu_005" → ● (pointer to tool in turn-1/cycle-1)
    │   ...
    │
    ├── :agent-index (hash-table) ────────────────────────────────
    │   "ag_01" → ● (pointer to agent in turn-1)
    │
    ├── :files (hash-table)
    │   "/src/auth.ts" → {ops: ("read" "edit"), last-touched: T}
    │
    └── :tasks, :plan, :token-usage, :allow-patterns, :notifications ...
```

## Rendering Mapping

The tree mirrors the screen. The renderer (`claude-gravity-render.el`) iterates without grouping:

```
TREE STRUCTURE                          RENDERED UI
─────────────                           ──────────
:turns tlist                            ── Turns (N) ──────
  turn-node[0]                            Pre-prompt activity  [0t 0a 2 tools]
    cycle-0                                 ┊ 2 tools
      tool, tool                              [x] Glob  [x] Read

  turn-node[1]                          ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
    prompt                              ❯ Fix the auth bug  [1t 1a 4 tools]  45.2s
    cycle-0                               ┊ Let me find the files...
      tool, tool                            ┊ 2 tools  [x] Glob  [x] Read
    cycle-1                               ┊ I see the issue...
      tool, tool                            ┊ 2 tools  [x] Edit  [x] Bash
    agents                                  [x] Explore (ag_01)  3.1s
      agent.cycles                            ┊ 2 tools  [x] Glob  [x] Read
    tasks                                 Tasks (1/1)  [x] Fix auth
    stop_text                             ┊ All tests pass now.
```

## Key Design Points

1. **Insertion-time grouping**: Cycle boundaries are detected when tools are added (`--tree-add-tool`), not at render time. This eliminates dedup logic from the renderer.

2. **O(1) lookups**: `:tool-index` and `:agent-index` hash tables store pointers to the same alist objects in the tree. Updates via `PostToolUse`/`SubagentStop` use hash lookup, not tree traversal.

3. **Bidirectional agent↔tool links**: `--link-agent-to-task-tool` sets `agent` key on the Task tool and `task-tool` key on the agent alist, enabling the renderer to show agent branches nested under their spawning Task tool.

4. **Pre-computed counts**: `tool-count` and `agent-count` on turn-nodes are incremented at insertion time, avoiding tree walks for header display.
