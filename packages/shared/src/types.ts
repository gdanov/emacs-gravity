// types.ts — Shared types for the gravity ecosystem
//
// Three layers:
// 1. HookData — raw hook events flowing from Claude Code through the bridge
// 2. View Model — stateful session tree (port of claude-gravity-state.el)
// 3. Protocol — messages between server and terminals

// ── Hook Data (bridge layer) ─────────────────────────────────────────

/** Token usage counters extracted from transcript. */
export interface TokenUsage {
  input_tokens: number;
  output_tokens: number;
  cache_read_input_tokens: number;
  cache_creation_input_tokens: number;
}

/**
 * Hook event data flowing through the bridge.
 *
 * Fields are grouped into two categories:
 * - **Hook input**: populated by Claude Code before the bridge runs
 * - **Enrichment output**: populated by the bridge before sending to Emacs
 *
 * All fields are optional because different event types carry different subsets.
 */
export interface HookData {
  // --- Hook input (from Claude Code) ---
  session_id?: string;
  cwd?: string;
  transcript_path?: string;
  agent_id?: string;
  tool_name?: string;
  tool_use_id?: string;
  tool_input?: {
    command?: string;
    model?: string;
    [key: string]: unknown;
  };

  // --- Enrichment output (added by bridge) ---

  // Session metadata
  temp_id?: string | null;
  tmux_session?: string;
  effort_level?: string;
  slug?: string | null;
  branch?: string | null;
  /** Driver that produced the event: "claude-code", "pi", "opencode", … */
  source?: string;

  // Agent tracking
  agent_transcript_path?: string;
  agent_tool_ids?: string[];
  agent_stop_text?: string;
  agent_stop_thinking?: string;

  // Tool attribution
  parent_agent_id?: string;
  candidate_agent_ids?: string[];

  // Content extraction (PreToolUse)
  assistant_text?: string;
  assistant_thinking?: string;
  model?: string;
  requested_model?: string;

  // Content extraction (PostToolUse)
  post_tool_text?: string;
  post_tool_thinking?: string;

  // Content extraction (Stop)
  stop_text?: string;
  stop_thinking?: string;
  token_usage?: TokenUsage;

  // Pass-through for unknown fields from Claude Code
  [key: string]: unknown;
}

/** All hook event types supported by the bridge.
 *
 * The first group mirrors Claude Code's hook vocabulary (delivered via the
 * emacs-bridge shim). The trailing `TurnOpen` / `TurnClose` / `ToolPartial`
 * are internal boundary events emitted by the pi-driver translator — they
 * don't correspond to any Claude Code hook. They exist so pi can drive
 * turn boundaries and stream partial tool output independently of CC's
 * coarser event vocabulary.
 */
export type HookEventName =
  | "SessionStart"
  | "SessionEnd"
  | "PreToolUse"
  | "PostToolUse"
  | "PostToolUseFailure"
  | "SubagentStart"
  | "SubagentStop"
  | "UserPromptSubmit"
  | "Stop"
  | "Notification"
  | "PermissionRequest"
  | "AskUserQuestionIntercept"
  | "TurnOpen"
  | "TurnClose"
  | "ToolPartial"
  | "Compaction";

// ── View Model (session state tree) ──────────────────────────────────
//
// Direct port of claude-gravity-state.el / claude-gravity-session.el.
// Bidirectional links (Tool↔Agent) use ID references instead of
// object pointers.

export interface Session {
  sessionId: string;
  cwd: string;
  project: string;
  status: "active" | "ended";
  claudeStatus: "idle" | "responding";
  slug: string | null;
  displayName: string | null;
  branch: string | null;
  pid: number | null;
  modelName: string | null;
  tmuxSession: string | null;
  source: string | null;  // "claude-code", "pi", "opencode", etc.
  startTime: number;
  lastEventTime: number;
  tokenUsage: TokenUsage | null;
  /** Cumulative cost for the session in USD. Sourced from pi's
   * `get_session_stats.cost`. Null when not reported (e.g. Claude Code
   * sessions don't surface a cost field). */
  cost: number | null;
  /** Pi only: on-disk path of pi's session file (under
   * `--session-dir`). Used to resume the session via `switch_session`
   * or `--session <path>`. Null for non-pi sessions. */
  piSessionFile: string | null;
  /** Context-window utilization. Sourced from pi's
   * `get_session_stats.contextUsage` (tokens used / window size / percent).
   * Null when not reported. */
  contextUsage: { tokens: number | null; contextWindow: number; percent: number | null } | null;
  plan: Plan | null;
  streamingText: string | null;
  permissionMode: string | null;

  // Turn tree
  turns: TurnNode[];
  currentTurn: number;

  // Indexes for O(1) lookup
  toolIndex: Record<string, ToolLocation>;
  agentIndex: Record<string, AgentLocation>;

  // Collections
  tasks: Record<string, Task>;
  files: Record<string, FileEntry>;

  /**
   * Chronological list of compactions that occurred during this session.
   * Each entry is appended by `compaction_end`; never mutated thereafter.
   * `marker.turnNumber` is the turn that was current when compaction
   * completed — terminals can group markers by turn to render an inline
   * banner without affecting turn-boundary semantics.
   */
  compactions: CompactionMarker[];

  totalToolCount: number;

  /**
   * Pi only: snapshot of `get_commands` for the current pi process —
   * extension commands, prompt templates, and skills the user can invoke
   * via `/<name>`. Null means "not yet fetched / unknown"; an explicit
   * empty array means "fetched, none available". Refreshed when the pi
   * process starts and on `pi.refresh-commands` from a terminal.
   */
  piCommands: PiCommandDescriptor[] | null;

  /**
   * Pi only: snapshot of `get_available_models` for the current pi process —
   * the models the user can switch to via `set_model`. Null means "not yet
   * fetched / unknown"; an explicit empty array means "fetched, none
   * available". Refreshed when the pi process starts and on
   * `pi.refresh-models` from a terminal.
   */
  piModels: PiModel[] | null;
}

/**
 * One entry from pi's `get_commands` RPC. Mirrors the shape pi emits in
 * `response.data.commands[]`. See pi `docs/rpc.md` `get_commands`.
 */
export interface PiCommandDescriptor {
  /** Command name without the leading slash. Skills carry the `skill:` prefix. */
  name: string;
  /** Human-readable description (optional for extension commands). */
  description?: string;
  /** Origin of the command. */
  source: "extension" | "prompt" | "skill";
  /** Where it was loaded from. Absent for extension commands. */
  location?: "user" | "project" | "path";
  /** Absolute path of the backing file (optional). */
  path?: string;
}

/**
 * One entry from pi's `get_available_models` RPC, normalized to the subset
 * the model picker needs. Pi's full `Model` carries more fields (api,
 * baseUrl, reasoning, input[], maxTokens, cost{}); only these matter to
 * gravity. `id` IS the `modelId` accepted by `set_model`. See pi
 * `docs/rpc.md` `get_available_models`.
 */
export interface PiModel {
  /** Model id — the value passed back as `modelId` to `set_model`. */
  id: string;
  /** Human-readable model name (optional). */
  name?: string;
  /** Provider key (anthropic / openai / google / …). */
  provider: string;
  /** Context window size in tokens (optional; used only for the label). */
  contextWindow?: number;
}

/**
 * One pi compaction event recorded on the session. Sourced from pi's
 * `compaction_end` event. Pi summarizes older conversation history and
 * discards it from the model's context window to free space; gravity
 * records the marker so users can see "context was compacted here".
 */
export interface CompactionMarker {
  /**
   * Pi-side reason: `"manual"` (user-triggered via /compact),
   * `"threshold"` (pi hit a context threshold mid-stream), or
   * `"overflow"` (pi hit context overflow during a tool loop).
   */
  reason: string;
  /**
   * Turn number that was current when `compaction_end` fired. -1 if no
   * user turn has been opened yet (compaction during turn 0).
   */
  turnNumber: number;
  /** Unix-ms timestamp when the marker was recorded. */
  timestamp: number;
  /** Tokens in pi's context immediately before compaction. null if unreported. */
  tokensBefore: number | null;
  /** Pi's summary of the discarded history. null if unreported or aborted. */
  summary: string | null;
  /** True if pi aborted the compaction (no actual context savings). */
  aborted: boolean;
}

/** Pointer to a tool's location in the turn tree. */
export interface ToolLocation {
  turnNumber: number;
  stepIndex: number;
  toolIndex: number;
  agentId: string | null;
}

/** Pointer to an agent's location in the turn tree. */
export interface AgentLocation {
  turnNumber: number;
  agentIndex: number;
}

export interface TurnNode {
  turnNumber: number;
  prompt: PromptEntry | null;
  steps: StepNode[];
  agents: Agent[];
  tasks: Task[];
  toolCount: number;
  agentCount: number;
  frozen: boolean;
  stopText: string | null;
  stopThinking: string | null;
  tokenIn: number | null;
  tokenOut: number | null;
  /**
   * Why the model stopped on this turn. Sourced from the trailing
   * AssistantMessage's `stopReason` in pi's `agent_end.messages[]`. One of
   * `"stop"` (model produced no further tool calls), `"length"` (budget
   * exhausted), `"toolUse"` (stopped to wait for tool result), `"error"`,
   * `"aborted"`, or null (Claude Code path — CC's Stop hook doesn't carry
   * a stop reason).
   */
  stopReason: string | null;
  /**
   * Net file changes made during this turn. One entry per file edited
   * (Edit/Write/MultiEdit/NotebookEdit), each carrying a single
   * consolidated baseline→final diff regardless of how many times the
   * file was edited. Populated incrementally via `update_turn_file`
   * patches; empty until the first edit-class tool completes.
   */
  editedFiles: FileDiff[];
}

export interface StepNode {
  thinking: string | null;
  text: string | null;
  tools: Tool[];
}

export interface Tool {
  toolUseId: string;
  name: string;
  input: Record<string, unknown>;
  status: "running" | "done" | "error";
  result: unknown;
  /**
   * Streaming partial result, updated by pi's `tool_execution_update`
   * events while the tool is running. Replaced on each update; cleared
   * (left in place but no longer authoritative) once `result` is set on
   * tool_execution_end. Terminals that want live progress can render
   * this; terminals that only render the final result can ignore it.
   * Null for Claude Code tools — CC delivers full output in one shot
   * via PostToolUse.
   */
  partial: unknown;
  timestamp: number;
  duration: number | null;
  turn: number;

  // Assistant context around this tool
  assistantText: string | null;
  assistantThinking: string | null;
  postText: string | null;
  postThinking: string | null;

  // Agent attribution
  parentAgentId: string | null;
  ambiguous: boolean;
  candidateAgentIds: string[] | null;

  // Linked agent (for Task tools that spawn agents)
  agentId: string | null;
}

export interface Agent {
  agentId: string;
  type: string;
  status: "running" | "done";
  steps: StepNode[];
  toolCount: number;
  stopText: string | null;
  stopThinking: string | null;
  duration: number | null;
  timestamp: number;
  transcriptPath: string | null;

  // Linked Task tool that spawned this agent
  taskToolUseId: string | null;
}

export interface Task {
  taskId: string;
  subject: string | null;
  description: string | null;
  activeForm: string | null;
  status: "pending" | "in_progress" | "completed";
  turn: number;
}

export interface FileEntry {
  ops: string[];
  lastTouched: number;
}

/**
 * One hunk of a unified diff, in the same shape as the `structuredPatch`
 * field of Claude Code's Edit/Write tool results and jsdiff's
 * `structuredPatch()` output. Each entry of `lines` is prefixed with
 * " " (context), "-" (removed), or "+" (added).
 */
export interface StructuredPatchHunk {
  oldStart: number;
  oldLines: number;
  newStart: number;
  newLines: number;
  lines: string[];
}

/**
 * One file's net change within a single turn. When a file is edited
 * multiple times in a turn, `hunks` holds a single consolidated diff —
 * the file content before the turn's first edit (baseline) versus the
 * content after the last edit (final) — never a stack of per-edit diffs.
 */
export interface FileDiff {
  /** Absolute path of the edited file. */
  path: string;
  /** Edit-class tool ops applied this turn, in order, e.g. ["edit","edit","write"]. */
  ops: string[];
  /** Number of edit-class tool calls that touched this file this turn. */
  editCount: number;
  status: "created" | "modified" | "deleted";
  /** Lines added across the consolidated diff. */
  added: number;
  /** Lines removed across the consolidated diff. */
  removed: number;
  /**
   * Consolidated baseline→final diff. Null when the diff could not be
   * computed (e.g. NotebookEdit with no usable patch, unreadable file)
   * or was elided for size — terminals then render the entry path-only.
   */
  hunks: StructuredPatchHunk[] | null;
  /** True when `hunks` was elided because the diff exceeded the size cap. */
  truncated: boolean;
}

export interface Plan {
  content: string;
  filePath: string | null;
  allowedPrompts: string[];
}

export interface PromptEntry {
  type: "user" | "question" | "phase-boundary";
  text: string;
  submitted: number;
  elapsed: number | null;
  // For question prompts
  toolUseId: string | null;
  answer: string | null;
}

// ── Inbox ────────────────────────────────────────────────────────────

export type InboxItemType = "permission" | "question" | "plan-review" | "idle";

export interface InboxItem {
  id: number;
  type: InboxItemType;
  sessionId: string;
  project: string | null;
  label: string;
  timestamp: number;
  summary: string;
  data: Record<string, unknown>;
}

// ── Semantic Patches ─────────────────────────────────────────────────
//
// Typed operations that map 1:1 to model mutations.
// Terminals apply patches incrementally; unknown ops trigger full refresh.

export type Patch =
  | { op: "set_status"; status: "active" | "ended" }
  | { op: "set_claude_status"; claudeStatus: "idle" | "responding" }
  | { op: "set_token_usage"; usage: TokenUsage }
  | { op: "set_cost"; cost: number | null }
  | { op: "set_context_usage"; contextUsage: { tokens: number | null; contextWindow: number; percent: number | null } | null }
  | { op: "set_plan"; plan: Plan | null }
  | { op: "set_streaming_text"; text: string | null }
  | { op: "set_permission_mode"; mode: string | null }
  | { op: "set_meta"; slug?: string; displayName?: string; branch?: string; pid?: number; modelName?: string; tmuxSession?: string; piSessionFile?: string }
  | { op: "add_turn"; turn: TurnNode }
  | { op: "freeze_turn"; turnNumber: number }
  | { op: "set_turn_stop"; turnNumber: number; stopText?: string; stopThinking?: string; stopReason?: string }
  | { op: "set_turn_tokens"; turnNumber: number; tokenIn: number; tokenOut: number }
  | { op: "add_step"; turnNumber: number; agentId?: string; step: StepNode }
  | { op: "add_tool"; turnNumber: number; stepIndex: number; agentId?: string; tool: Tool }
  | { op: "complete_tool"; toolUseId: string; result: unknown; status: "done" | "error"; duration?: number; postText?: string; postThinking?: string }
  | { op: "update_tool_partial"; toolUseId: string; partial: unknown }
  | { op: "add_compaction"; marker: CompactionMarker }
  | { op: "add_agent"; agent: Agent }
  | { op: "complete_agent"; agentId: string; stopText?: string; stopThinking?: string; duration?: number; transcriptPath?: string }
  | { op: "update_task"; taskId: string; task: Task }
  | { op: "track_file"; path: string; fileOp: string }
  | { op: "update_turn_file"; turnNumber: number; file: FileDiff }
  | { op: "add_prompt"; turnNumber: number; prompt: PromptEntry }
  | { op: "set_prompt_answer"; turnNumber: number; toolUseId: string; answer: string }
  | { op: "set_pi_commands"; commands: PiCommandDescriptor[] }
  | { op: "set_pi_models"; models: PiModel[] };

// ── Protocol Messages ────────────────────────────────────────────────
//
// Newline-delimited JSON over Unix domain socket.
// WebSocket upgrade path for web terminals later.

/**
 * Terminal ⇄ server protocol version. Bump on any breaking change to the
 * message shapes or delivery model.
 *
 * History:
 *  - 1 — push era: server proactively broadcast session.update / inbox.added.
 *  - 2 — pull-only: server emits `state-changed` signals; clients `poll`.
 *
 * Clients send this in their `hello`. A client that omits it (or sends a
 * lower number) is treated as legacy: the server logs a warning and replies
 * with a `protocol.mismatch` message so the client can surface it. This is
 * how a stale, long-running client (e.g. a menu bar app never relaunched
 * after a protocol change) gets flagged instead of silently degrading.
 */
export const PROTOCOL_VERSION = 2;

/** Areas of state that can change on the server. */
export type ChangedArea = "session" | "inbox" | "overview" | "notice";

/**
 * Server → terminal messages that carry a payload.
 *
 * Push terminal communication was removed: the server NEVER proactively
 * broadcasts replicated state. Within this union:
 *  - `session.update` / `inbox.added` / `inbox.removed` are **never sent**
 *    by the current server. Retained so a newer terminal can still decode
 *    an older server; new code must not rely on receiving them.
 *  - `session.snapshot` / `overview.snapshot` / `inbox.snapshot` are sent
 *    ONLY as replies to explicit `request.session` / `request.overview` /
 *    `request.resync`.
 *  - `session.removed` / `notice` / `pi.session` are out-of-band lifecycle
 *    / alert events (not replicated state) and remain direct messages.
 */
export type ServerPushMessage =
  | { type: "session.snapshot"; sessionId: string; session: Session }
  /** @deprecated never sent — pull via `state-changed` + `poll`. */
  | { type: "session.update"; sessionId: string; patches: Patch[] }
  | { type: "session.removed"; sessionId: string }
  /** @deprecated never sent — pull via `state-changed` + `poll`. */
  | { type: "inbox.added"; item: InboxItem }
  /** @deprecated never sent — pull via `state-changed` + `poll`. */
  | { type: "inbox.removed"; itemId: number }
  | { type: "inbox.snapshot"; items: InboxItem[] }
  | { type: "overview.snapshot"; projects: ProjectSummary[] }
  | { type: "notice"; level: "info" | "warn" | "error"; text: string }
  /**
   * Sent once, right after `hello`, when the client's `protocolVersion`
   * does not match the server's {@link PROTOCOL_VERSION}. Distinct from
   * `notice` because it is a persistent condition (the client must be
   * rebuilt/relaunched) — terminals should keep showing it, not auto-clear.
   */
  | { type: "protocol.mismatch"; serverVersion: number; clientVersion: number; text: string }
  /**
   * Pi session lifecycle signal. Out-of-band from the patch stream — used by
   * terminals to track the latest pi session id without waiting for a full
   * snapshot, and to surface server-side rejections.
   */
  | { type: "pi.session"; sessionId: string; event: "started"; cwd: string }
  | { type: "pi.session"; sessionId: string; event: "stopped" }
  | { type: "pi.session"; sessionId: string; event: "rejected"; reason: string };

/** Messages from server to terminal (pull mode — signals only, no payload). */
export type ServerSignalMessage =
  | { type: "state-changed"; what: ChangedArea; sessionId?: string; seq: number }
  | { type: "session-patches"; sessionId: string; seq: number; patches: Patch[] }
  | { type: "inbox-items"; items: InboxItem[] }
  | { type: "overview-data"; projects: ProjectSummary[] };

/** Union of all server → terminal messages. */
export type ServerMessage = ServerPushMessage | ServerSignalMessage;

/** Messages from terminal to server. */
export type TerminalMessage =
  | { type: "hello"; capabilities: string[]; protocolVersion?: number }
  | { type: "action.permission"; itemId: number; decision: "allow" | "deny"; message?: string; updatedPermissions?: unknown[] }
  | { type: "action.question"; itemId: number; answers: string[] }
  | { type: "action.plan-review"; itemId: number; decision: "allow" | "deny"; feedback?: PlanFeedback }
  | { type: "action.turn-auto-approve"; sessionId: string }
  | { type: "request.session"; sessionId: string }
  | { type: "request.overview" }
  | { type: "request.resync" }
  | { type: "hint.session-dead"; sessionId: string }
  | { type: "poll" }
  // Pi driver messages
  | { type: "pi.start"; cwd?: string; thinkingLevel?: string }
  | { type: "pi.prompt"; sessionId: string; text: string; images?: string[] }
  | { type: "pi.steer"; sessionId: string; text: string }
  | { type: "pi.abort"; sessionId: string }
  | { type: "pi.set-thinking"; sessionId: string; level: string }
  | { type: "pi.set-session-name"; sessionId: string; name: string }
  | { type: "pi.set-model"; sessionId: string; provider: string; modelId: string }
  | { type: "pi.resume"; sessionId?: string; sessionPath: string }
  | { type: "pi.compact"; sessionId?: string; customInstructions?: string }
  | { type: "pi.new-session"; sessionId?: string }
  | { type: "pi.stop"; sessionId?: string }
  | { type: "pi.refresh-commands"; sessionId: string }
  | { type: "pi.refresh-models"; sessionId: string }

export interface ProjectSummary {
  project: string;
  sessions: SessionSummary[];
}

export interface SessionSummary {
  sessionId: string;
  slug: string | null;
  displayName: string | null;
  status: "active" | "ended";
  claudeStatus: "idle" | "responding";
  toolCount: number;
  lastEventTime: number;
  latestMessage: string | null;
  latestUserPrompt: string | null;
}

export interface PlanFeedback {
  inlineComments: Array<{ line: number; nearText: string; comment: string }>;
  claudeMarkers: Array<{ line: number; nearText: string; text: string }>;
  diff: string | null;
  generalComment: string | null;
}

// ── Hook Socket Messages ─────────────────────────────────────────────
//
// Messages between the bridge shim and the gravity server.
// One-shot: shim connects, sends event, optionally waits for response.

export interface HookSocketMessage {
  event: HookEventName;
  session_id: string;
  cwd: string;
  pid: number | null;
  source: string;
  data: HookData;
  needs_response: boolean;
}

/**
 * Response written back to the hook socket by the gravity-server.
 *
 * In server mode, this is the full stdout-ready payload that the bridge
 * writes directly to stdout for Claude Code to consume.
 *
 * The hookSpecificOutput wrapper is required by Claude Code's hook protocol.
 * Additional top-level fields (answer, answers) are included for
 * AskUserQuestionIntercept responses.
 */
export interface HookSocketResponse {
  hookSpecificOutput?: {
    hookEventName: string;
    decision?: {
      behavior: "allow" | "deny";
      message?: string;
      updatedPermissions?: unknown[];
    };
    permissionDecision?: string;
    permissionDecisionReason?: string;
    updatedInput?: Record<string, unknown>;
  };
  /** Legacy format (used by Emacs socket direct mode) */
  decision?: {
    behavior: "allow" | "deny";
    message?: string;
  };
  answer?: string;
  answers?: string[];
}
