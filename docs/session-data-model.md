# Session Data Model

Reference documentation for how session transcript state is stored in-memory in Emacs.

## Session Plist

Stored in `claude-gravity--sessions` hash table (session-id → plist).
Created in `claude-gravity-session.el:92` (`--ensure-session`).

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
├── :state           alist      — { tools: list-of-tool-alists }
├── :prompts         list       — prompt alists (one per turn)
├── :agents          list       — agent alists
├── :tasks           hash-table — taskId → task alist
├── :files           hash-table — filepath → { ops, last-touched }
├── :tool-index      hash-table — tool_use_id → tool alist (O(1))
│
├── :plan            plist|nil  — { :content, :file-path, :allowed-prompts }
├── :token-usage     alist|nil  — { input_tokens, output_tokens, cache_* }
├── :allow-patterns  list|nil
└── :notifications   list
```

## Prompt Alist

One per turn in `:prompts`. Created by UserPromptSubmit, AskUserQuestion, ExitPlanMode in `claude-gravity-events.el`.

```
├── text            string     — user prompt (XML-stripped)
├── submitted       time       — when sent
├── elapsed         float|nil  — seconds to Stop (set by model-finalize-last-prompt)
├── stop_text       string|nil — trailing assistant text (set by Stop handler)
├── stop_thinking   string|nil — trailing thinking (set by Stop handler)
│
│  Question prompts only:
├── type            symbol     — 'question | 'phase-boundary | nil
├── tool_use_id     string     — links to AskUserQuestion tool
└── answer          string|nil
```

## Tool Alist

In `:state.tools` (root) or agent's `agent-tools`. Created by PreToolUse, completed by PostToolUse.

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
└── candidate-agents  list|nil
```

## Agent Alist

In `:agents`. Created by SubagentStart, completed by SubagentStop.

```
├── agent_id          string     — unique ID
├── type              string     — "Explore", "general-purpose", "Plan", etc.
├── status            string     — "running" | "done"
├── timestamp         time
├── turn              integer
├── duration          float|nil  — seconds (set on SubagentStop)
├── agent-tools       list       — nested tool alists (same shape as root)
├── transcript_path   string|nil
├── stop_text         string|nil — agent summary text
├── stop_thinking     string|nil — agent final thinking
│
│  Set by RET → parse transcript:
├── transcript_parsed      bool|nil
├── transcript_prompt      string|nil
├── transcript_model       string|nil
└── transcript_tool_count  integer|nil
```

## Turn Grouping

Everything is tied together by `turn` number:

```
Turn N:
  prompts[N]           — the user prompt + stop_text/elapsed
  state.tools[turn=N]  — root tools for this turn
  agents[turn=N]       — agents spawned in this turn
    └── agent-tools    — agent's nested tools
  tasks[turn=N]        — tasks created/updated in this turn
```

The renderer groups by turn, iterating prompts 0..current-turn and collecting tools/agents/tasks with matching turn number.

## Full Session Diagram (Real Data)

Diagram of session `33e00903` (4 turns, 119 tools, 1 agent) showing the hash table structure, nested collections, and pointer-based tool index:

```
claude-gravity--sessions (hash-table)
│
├── "33e00903-..." ─────────────────────────────────────────────────────────
│   │
│   │  Session Plist
│   │  :status=ended  :claude-status=responding  :current-turn=4
│   │
│   ├── :prompts ─── list of 4 alists ──────────────────────────────────────
│   │   │
│   │   ├─[0] turn 1: "emacs mcp is connected..."
│   │   │      submitted=T  elapsed=656s  stop_text="Done. Here's..."
│   │   │
│   │   ├─[1] turn 2: "good job. let's test..."
│   │   │      submitted=T  elapsed=nil   stop_text=nil     ← Stop lost
│   │   │
│   │   ├─[2] turn 3: "check if we're using async..."
│   │   │      submitted=T  elapsed=nil   stop_text=nil     ← Stop lost
│   │   │
│   │   └─[3] turn 4: "let's do 2. Fix O(n^2)..."
│   │          submitted=T  elapsed=nil   stop_text=nil     ← Stop lost
│   │
│   ├── :state ─── alist ──────────────────────────────────────────────────
│   │   │
│   │   └── tools ─── list of 119 tool alists
│   │       │
│   │       │  turn 0: 34 tools (pre-prompt)
│   │       │  ┌─────────────────────────────────────────────────────┐
│   │       ├──│ {tool_use_id, name="Glob", turn=0, status="done",  │
│   │       │  │  assistant_text=nil, post_text=nil}                 │
│   │       ├──│ {tool_use_id, name="Read", turn=0, status="done",  │
│   │       │  │  assistant_text="Let me read...", post_text=nil}    │
│   │       │  │ ...32 more                                         │
│   │       │  └─────────────────────────────────────────────────────┘
│   │       │
│   │       │  turn 1: 42 tools
│   │       │  ┌─────────────────────────────────────────────────────┐
│   │       ├──│ {name="Edit", turn=1, status="done",               │
│   │       │  │  assistant_text="Now I'll fix...",                  │
│   │       │  │  post_text="The edit was successful."}              │
│   │       │  │ ...41 more                                         │
│   │       │  └─────────────────────────────────────────────────────┘
│   │       │
│   │       │  turn 2: 28 tools    turn 3: 10 tools    turn 4: 5 tools
│   │       └──...
│   │
│   ├── :agents ─── list of 1 alist ───────────────────────────────────────
│   │   │
│   │   └─[0] {agent_id="...", type="Explore", status="done",
│   │          turn=1, duration=3.2,
│   │          stop_text="Based on my search...",
│   │          agent-tools: [] }              ← nested tools (same shape)
│   │
│   ├── :tool-index ─── hash-table (119 entries) ─────────────────────────
│   │   │
│   │   │  "toolu_014HGB..." ──→ ─┐
│   │   │  "toolu_01ABC..." ──→ ──┤  same alist objects as in
│   │   │  "toolu_01XYZ..." ──→ ──┤  :state.tools and agent-tools
│   │   │  ...                    │  (pointer, not copy)
│   │   └─────────────────────────┘
│   │
│   ├── :files ─── hash-table (6 entries) ─────────────────────────────────
│   │   │  "/path/to/file.el" → {ops: ("read" "edit"), last-touched: T}
│   │   └── ...
│   │
│   ├── :tasks ─── hash-table (0 entries) ─────────────────────────────────
│   │
│   └── :plan, :token-usage, :allow-patterns, :notifications ...
│
├── "e4dd8db7-..." ─── (current session, same structure) ──────────────────
│
└── "test-liv..." ──── (test session) ─────────────────────────────────────
```

## Key Relationships

Turn number is the join key connecting all collections:

```
                    ┌──────────────────────────────────────┐
                    │          TURN NUMBER                  │
                    │    (the join key for everything)      │
                    └──────┬──────┬──────┬──────┬──────────┘
                           │      │      │      │
                    prompts[N]    │      │      │
                           │   tools     │      │
                           │  [turn=N]   │      │
                           │      │   agents    │
                           │      │  [turn=N]   │
                           │      │      │    tasks
                           │      │      │   [turn=N]
                           │      │      │
                           │      │      └─── agent-tools[]
                           │      │              │
                           │      └──────────────┘
                           │             │
                           │      :tool-index{tid} ──→ (same objects)
                           │
              stop_text ◄──┘
              elapsed   ◄──┘  (set by Stop handler on last prompt)
```

The `tool-index` hash is a secondary index — it stores pointers to the same alist objects that live in `state.tools` and `agent-tools`, enabling O(1) lookup by `tool_use_id` without duplicating data.

## ASCII Visualization: Turn Grouping Example

A real session with 2 turns, showing how data maps to the rendered UI:

```
SESSION abc-123
├── :current-turn 1
├── :prompts
│   ├── [0] (text: "Fix the auth bug"  submitted: T0  elapsed: 45.2)
│   │       (stop_text: "All tests pass now."  stop_thinking: nil)
│   └── [1] (text: "Also update the docs"  submitted: T1  elapsed: nil)
│
├── :state.tools
│   ├── tool{turn:0 name:"Glob"   id:"tu_001" status:"done"  assistant_text:"Let me find the files..."}
│   ├── tool{turn:0 name:"Read"   id:"tu_002" status:"done"  assistant_text:nil}
│   ├── tool{turn:0 name:"Edit"   id:"tu_003" status:"done"  assistant_text:"I see the issue..."}
│   ├── tool{turn:0 name:"Bash"   id:"tu_004" status:"done"  post_text:"Tests pass."}
│   ├── tool{turn:1 name:"Read"   id:"tu_005" status:"done"  assistant_text:"Now for the docs..."}
│   └── tool{turn:1 name:"Edit"   id:"tu_006" status:"running"}
│
├── :agents
│   └── agent{turn:0 id:"ag_01" type:"Explore" status:"done" duration:3.1}
│       └── agent-tools
│           ├── tool{name:"Glob" id:"tu_010" status:"done"}
│           └── tool{name:"Read" id:"tu_011" status:"done"}
│
└── :tasks
    └── "task_1" → {subject:"Fix auth" status:"completed" turn:0}

RENDERS AS:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
── Turns (2) ───────────────────────────────

╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
❯ Fix the auth bug
  [1t 1a 4 tools]  45.2s

  ┊ Let me find the files...                    ← tu_001.assistant_text
  ┊ 2 tools                                     ← tu_001 + tu_002 (grouped)
    [x] Glob  **/*.ts
    [x] Read  /src/auth.ts

  ┊ I see the issue...                          ← tu_003.assistant_text
  ┊ 2 tools                                     ← tu_003 + tu_004
    [x] Edit  /src/auth.ts
    [x] Bash  npm test
      Tests pass.                                ← tu_004.post_text

    [x] Explore auth patterns                   ← agent ag_01
      Task(Explore)  → Explore (ag_01)  3.1s

  Tasks (1/1)
    [x] Fix auth

  ┊ All tests pass now.                         ← prompts[0].stop_text

╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
❯ Also update the docs
  [0t 0a 2 tools]

  ┊ Now for the docs...                         ← tu_005.assistant_text
  ┊ 2 tools
    [x] Read  /docs/auth.md
    [/] Edit  /docs/auth.md                      ← tu_006 (running)
```
