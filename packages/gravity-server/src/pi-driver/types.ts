// types.ts — Pi event types and translation result types for the pi-driver
//
// Defines the union of events emitted by pi in RPC mode, and the types
// used during translation from pi events to gravity-server's HookData format.

import type { HookData, HookEventName, TokenUsage } from "@gravity/shared";

// ── Thinking levels (pi effort control) ─────────────────────────────

/** Pi thinking levels, from off (no thinking) to xhigh (maximum thinking). */
export type ThinkingLevel = "off" | "minimal" | "low" | "medium" | "high" | "xhigh";

/** Map pi thinking levels to gravity effort levels. */
export const EFFORT_FROM_THINKING: Record<ThinkingLevel, string> = {
  off: "low",
  minimal: "low",
  low: "medium",
  medium: "medium",
  high: "high",
  xhigh: "high", // cap at "high"
};

// ── Pi Events (from pi subprocess stdout) ────────────────────────────

/** Base interface for all pi events. */
export interface PiBaseEvent {
  /** Event type discriminator */
  type: string;
}

/** Assistant sent a text delta (token-by-token streaming). */
export interface TextDeltaEvent extends PiBaseEvent {
  type: "text_delta";
  delta: string;
}

/** Assistant sent a thinking delta (token-by-token streaming). */
export interface ThinkingDeltaEvent extends PiBaseEvent {
  type: "thinking_delta";
  delta: string;
}

/** A turn started (assistant is responding). */
export interface TurnStartEvent extends PiBaseEvent {
  type: "turn_start";
  turn_id: string;
}

/** A turn ended (assistant finished responding). */
export interface TurnEndEvent extends PiBaseEvent {
  type: "turn_end";
  turn_id: string;
}

/**
 * Pi 0.74 sends a bare `{type: "agent_start"}`. Older drafts of this type
 * declared a `message` field — pi never actually emitted that shape; the
 * user prompt arrives in a subsequent `message_start` with role "user".
 */
export interface AgentStartEvent extends PiBaseEvent {
  type: "agent_start";
}

/**
 * One assistant message from pi's `agent_end.messages[]`.
 *
 * Pi 0.74 emits the agent run's full message list on `agent_end`. The
 * trailing `AssistantMessage` carries the final stop reason; the sum of
 * `usage` across assistant messages is the agent run's total token cost.
 * See pi `docs/session.md` for the full message-type union.
 */
export interface PiAssistantMessage {
  role: "assistant";
  content?: unknown;
  api?: string;
  provider?: string;
  model?: string;
  usage?: {
    input?: number;
    output?: number;
    cacheRead?: number;
    cacheWrite?: number;
    totalTokens?: number;
    cost?: unknown;
  };
  stopReason?: "stop" | "length" | "toolUse" | "error" | "aborted";
  errorMessage?: string;
  timestamp?: number;
}

/** Any message in `agent_end.messages[]`. */
export interface PiAgentMessage {
  role: string;
  [key: string]: unknown;
}

/**
 * Agent finished one prompt cycle (not the whole session).
 *
 * Pi 0.74 wire shape is `{ messages: AgentMessage[] }`. The legacy
 * `result: { type, error, usage }` fields are NOT emitted by pi 0.74 — they
 * are retained on this type for defensive parsing only, in case pi reverts
 * to that shape on a future version. The translator walks `messages` for
 * usage and stopReason; do not read `result`.
 */
export interface AgentEndEvent extends PiBaseEvent {
  type: "agent_end";
  messages?: PiAgentMessage[];
  // Legacy / defensive — pi 0.74 does not emit these.
  result?: {
    type: "success" | "error" | "aborted";
    error?: string;
    usage?: {
      input_tokens: number;
      output_tokens: number;
      cache_read_input_tokens?: number;
      cache_creation_input_tokens?: number;
    };
  };
}

/**
 * Tool execution started.
 *
 * Pi 0.74 emits camelCase fields (`toolCallId`, `toolName`, `args`). The
 * snake_case variants are kept as optional for defensive parsing if pi
 * ever changes back — translator code reads both.
 *
 * `needs_response` is a forward-compat placeholder; pi 0.74 does not emit
 * it and the adapter does not currently consume it. See design/pi-adapter.md
 * "Feature Parity with Claude Code".
 */
export interface ToolExecutionStartEvent extends PiBaseEvent {
  type: "tool_execution_start";
  toolCallId?: string;
  toolName?: string;
  args?: Record<string, unknown>;
  // Legacy / defensive
  tool_call_id?: string;
  tool_name?: string;
  tool_input?: Record<string, unknown>;
  needs_response?: boolean;
}

/**
 * Tool execution completed.
 *
 * Pi 0.74 emits `{toolCallId, toolName, result, isError, error?}`. The
 * snake_case variants are kept as optional for defensive parsing.
 */
export interface ToolExecutionEndEvent extends PiBaseEvent {
  type: "tool_execution_end";
  toolCallId?: string;
  toolName?: string;
  result?: unknown;
  isError?: boolean;
  error?: string;
  // Legacy / defensive
  tool_call_id?: string;
  tool_name?: string;
  tool_result?: unknown;
}

/**
 * Pi emits this between `tool_execution_start` and `tool_execution_end`
 * for streaming partial results. The adapter ignores it currently.
 */
export interface ToolExecutionUpdateEvent extends PiBaseEvent {
  type: "tool_execution_update";
  toolCallId?: string;
  // pi may attach partial result fragments here — shape varies, kept loose.
  partial?: unknown;
}

/**
 * Model selected. Pi emits this via the extension event channel (see
 * pi-coding-agent source `agent-session.ts:_extensionRunner.emit({type:
 * "model_select", ...})`), not as a core session event in `docs/rpc.md`.
 * Handler retained defensively; harmless if pi never surfaces it on
 * stdout in RPC mode.
 */
export interface ModelSelectEvent extends PiBaseEvent {
  type: "model_select";
  model: string;
  provider: string;
}

/**
 * Pi 0.74 emits `message_start` as a full snapshot of one message. For
 * `role: "user"` it carries the prompt text. For `role: "assistant"` the
 * text streams in via `message_update` events and this snapshot is a
 * no-op for our purposes.
 */
export interface MessageStartEvent extends PiBaseEvent {
  type: "message_start";
  message?: {
    role?: "user" | "assistant" | "system";
    // Per pi docs/session.md, UserMessage.content is
    //   `string | (TextContent | ImageContent)[]`.
    // Accept both shapes; the translator handles them.
    content?: string | Array<{ type?: string; text?: string }>;
  };
}

/** Counterpart to `message_start`; we don't act on it. */
export interface MessageEndEvent extends PiBaseEvent {
  type: "message_end";
  message?: unknown;
}

/**
 * Streaming update inside an assistant message.
 *
 * Pi 0.74 uses `assistantMessageEvent`; older drafts of these types used
 * the inner field name `message_update`. Both shapes are accepted by the
 * translator.
 */
export interface MessageUpdateEvent extends PiBaseEvent {
  type: "message_update";
  assistantMessageEvent?: {
    type?: "text_delta" | "thinking_delta" | "status" | string;
    delta?: string;
    contentIndex?: number;
    partial?: unknown;
    status?: "in_progress" | "done" | "error";
  };
  // Legacy / defensive
  message_update?: {
    type: "text_delta" | "thinking_delta" | "status";
    delta?: string;
    status?: "in_progress" | "done" | "error";
  };
}

/** Error event from pi. */
export interface ErrorEvent extends PiBaseEvent {
  type: "error";
  error: string;
  code?: string;
}

/**
 * Git branch changed (pi emits this when the user switches branches).
 * Updates the accumulator's branch field so the next SessionStart
 * carries the correct value.
 *
 * Not in pi's documented event vocabulary (`docs/rpc.md` session events)
 * — sourced from pi's extension event channel. Handler retained
 * defensively; harmless if pi never emits it.
 */
export interface BranchUpdateEvent extends PiBaseEvent {
  type: "branch_update";
  /** Git branch name, or null when leaving a git repo. */
  branch: string | null;
}

/**
 * Pending steering / follow-up queue changed. Informational; gravity
 * doesn't currently surface the queue. See `docs/rpc.md` `queue_update`.
 */
export interface QueueUpdateEvent extends PiBaseEvent {
  type: "queue_update";
  steering?: string[];
  followUp?: string[];
}

/**
 * Compaction begins. Pi is about to summarize and discard older
 * conversation history to free context window space.
 *
 * `reason` is `"manual"`, `"threshold"`, or `"overflow"`. Compaction can
 * fire between turns or mid-stream during a long tool loop.
 */
export interface CompactionStartEvent extends PiBaseEvent {
  type: "compaction_start";
  reason?: "manual" | "threshold" | "overflow" | string;
}

/**
 * Compaction completes. Pi has replaced older history with a summary.
 * The session's cumulative token totals (from `get_session_stats`) are
 * unaffected — compaction only resets the model's context window, not
 * the historical token cost. `tokensBefore` records what was discarded.
 */
export interface CompactionEndEvent extends PiBaseEvent {
  type: "compaction_end";
  reason?: "manual" | "threshold" | "overflow" | string;
  aborted?: boolean;
  willRetry?: boolean;
  result?: {
    summary?: string;
    firstKeptEntryId?: string;
    tokensBefore?: number;
    details?: unknown;
  };
}

/**
 * Auto-retry begins after a transient provider error (overloaded, rate
 * limit, 5xx). Informational — gravity could surface as a status
 * indicator but currently just logs.
 */
export interface AutoRetryStartEvent extends PiBaseEvent {
  type: "auto_retry_start";
  attempt?: number;
  maxAttempts?: number;
  delayMs?: number;
  errorMessage?: string;
}

/** Auto-retry completes. `success` reports the outcome. */
export interface AutoRetryEndEvent extends PiBaseEvent {
  type: "auto_retry_end";
  success?: boolean;
  attempt?: number;
  finalError?: string;
}

/** An extension threw an error. Logged; no gravity surface. */
export interface ExtensionErrorEvent extends PiBaseEvent {
  type: "extension_error";
  extensionPath?: string;
  event?: string;
  error?: string;
}

/**
 * Pi extension UI request — extensions can call `ctx.ui.select`,
 * `ctx.ui.confirm`, `ctx.ui.input`, `ctx.ui.editor`, etc. Dialog methods
 * (select / confirm / input / editor) expect an `extension_ui_response`
 * on stdin keyed by `id`. Fire-and-forget methods (notify / setStatus /
 * setWidget / setTitle / set_editor_text) expect no response.
 *
 * See pi docs/rpc.md "Extension UI Protocol" for the canonical fields
 * per method.
 */
export interface ExtensionUIRequestEvent extends PiBaseEvent {
  type: "extension_ui_request";
  id: string;
  method:
    | "select"
    | "confirm"
    | "input"
    | "editor"
    | "notify"
    | "setStatus"
    | "setWidget"
    | "setTitle"
    | "set_editor_text"
    | string; // future-proofing against new pi methods
  // dialog fields (select / confirm / input / editor)
  title?: string;
  message?: string;
  options?: string[];
  timeout?: number;
  placeholder?: string;
  prefill?: string;
  // notify
  notifyType?: "info" | "warning" | "error";
  // setStatus
  statusKey?: string;
  statusText?: string;
  // setWidget
  widgetKey?: string;
  widgetLines?: string[];
  widgetPlacement?: "aboveEditor" | "belowEditor";
  // setTitle / set_editor_text
  text?: string;
}

/** Union of all pi event types. */
export type PiEvent =
  | TextDeltaEvent
  | ThinkingDeltaEvent
  | TurnStartEvent
  | TurnEndEvent
  | AgentStartEvent
  | AgentEndEvent
  | ToolExecutionStartEvent
  | ToolExecutionEndEvent
  | ToolExecutionUpdateEvent
  | ModelSelectEvent
  | MessageStartEvent
  | MessageEndEvent
  | MessageUpdateEvent
  | ExtensionUIRequestEvent
  | ErrorEvent
  | BranchUpdateEvent
  | QueueUpdateEvent
  | CompactionStartEvent
  | CompactionEndEvent
  | AutoRetryStartEvent
  | AutoRetryEndEvent
  | ExtensionErrorEvent
  | PiBaseEvent; // fallback for unknown events

// ── Translation result types ────────────────────────────────────────

/**
 * A single gravity-server event emitted by the hook translator.
 * Contains the event name and HookData to pass to handleEvent().
 */
export interface TranslationResult {
  /** The gravity event name to dispatch */
  hookEvent: HookEventName;
  /** The HookData payload for handleEvent() */
  hookData: HookData;
  /** Optional token usage extracted from this event */
  tokenUsage?: TokenUsage;
  /** The session ID for routing (set from AccState.sessionId) */
  sessionId: string;
}

/**
 * Result of translating a pi event. Either yields zero or more
 * gravity events to emit, or is a no-op (state was mutated, no events).
 *
 * `results` always has length >= 1 when kind is "emit". State-only
 * mutations (text_delta, turn_start, tool_execution_start) return "noop".
 */
export type TranslateEventResult =
  | { kind: "emit"; results: TranslationResult[] }
  | { kind: "noop" };

// ── Turn accumulator state ──────────────────────────────────────────

/**
 * Mutable state for batching pi turns into gravity turns.
 *
 * A gravity turn = one user prompt + zero or more pi turns (tool calls
 * within that response cycle).
 *
 * The accumulator collects:
 * - Pending assistant text/thinking (from message_update deltas)
 * - Tool queue (between turn_start and turn_end)
 * - Turn boundaries
 * - Session metadata
 */
export interface AccState {
  // Session context
  sessionId: string;
  cwd: string;
  branch: string | null;  // git branch, populated after session init
  modelName: string | null;
  effortLevel: string;

  // Content accumulation (from message_update)
  pendingAssistantText: string;
  pendingAssistantThinking: string;
  pendingPostText: string;
  pendingPostThinking: string;

  // Tool tracking
  currentToolUseId: string | null;
  currentToolName: string | null;
  currentToolInput: Record<string, unknown> | null;
  currentToolStartTime: number | null;
  /** Snapshot of pendingAssistantText taken at tool_execution_start. The
   * pending accumulator is cleared after the snapshot so the next tool's
   * preceding text starts fresh. accToolEnd consumes this. */
  currentToolAssistantText: string | undefined;
  currentToolAssistantThinking: string | undefined;

  // Turn tracking
  turns: AccTurn[];
  currentTurn: number; // index into turns[]
  inTurn: boolean; // true between turn_start and turn_end
}

export interface AccTurn {
  turnNumber: number;
  startedAt: number;
  endedAt: number | null;
  tools: AccTool[];
  stepIndex: number;
}

export interface AccTool {
  toolUseId: string;
  toolName: string;
  toolInput: Record<string, unknown>;
  assistantText: string | undefined;
  assistantThinking: string | undefined;
  startTime: number;
  endTime: number | null;
  result: unknown;
  error: string | null;
  postText: string | undefined;
  postThinking: string | undefined;
}

// ── RPC command types ───────────────────────────────────────────────

/**
 * Response delivered back to pi for a dialog `extension_ui_request`. Pi
 * matches by `id`. Exactly one of `value` / `confirmed` / `cancelled`
 * should be set, depending on the originating method.
 */
export interface ExtensionUIResponsePayload {
  id: string;
  value?: string;
  confirmed?: boolean;
  cancelled?: boolean;
}

/** Commands sent to pi via stdin (JSONL). */
export type PiCommand =
  | { type: "prompt"; message: string; images?: string[] }
  | { type: "steer"; message: string }
  | { type: "abort" }
  | { type: "set_thinking_level"; level: ThinkingLevel }
  | { type: "set_model"; provider: string; modelId: string }
  | { type: "get_session_stats" }
  | { type: "get_state" }
  | { type: "switch_session"; sessionPath: string }
  | { type: "compact"; customInstructions?: string }
  | { type: "new_session"; parentSession?: string }
  | ({ type: "extension_ui_response" } & ExtensionUIResponsePayload);

/** Event emitted by protocol.ts for parsed pi events. */
export type PiProtocolEvent = {
  event: PiEvent;
  raw?: string;
};

/**
 * Response to an RPC command sent over stdin. Pi 0.74 always sets
 * `type: "response"`; `command` echoes the request type; `id` is present
 * iff the request carried one (used for request/response correlation).
 */
export interface PiResponse {
  type: "response";
  command: string;
  id?: string;
  success: boolean;
  data?: unknown;
  error?: string;
}

/**
 * Subset of `get_session_stats` response data the adapter consumes. Pi's
 * full response includes message counts and a session file path which the
 * adapter currently ignores. See pi docs/rpc.md `get_session_stats`.
 */
export interface PiSessionStats {
  tokens?: {
    input?: number;
    output?: number;
    cacheRead?: number;
    cacheWrite?: number;
    total?: number;
  };
  cost?: number;
  contextUsage?: {
    tokens: number | null;
    contextWindow: number;
    percent: number | null;
  };
  sessionFile?: string;
  sessionId?: string;
}

// ── Driver options ──────────────────────────────────────────────────

/** Options for starting the pi driver. */
export interface PiDriverOptions {
  /** Working directory for the pi subprocess. Defaults to process.cwd(). */
  cwd?: string;
  /** Thinking level to pass on spawn. Defaults to "medium". */
  thinkingLevel?: ThinkingLevel;
  /** Optional explicit model override (passed via env). */
  model?: string;
  /** Optional explicit provider override (passed via env). */
  provider?: string;
  /** Path to pi binary. Defaults to "pi" (PATH lookup). */
  piBinaryPath?: string;
  /**
   * Directory where pi stores session files (`--session-dir`). Defaults to
   * `~/.local/state/gravity-pi-sessions`. The directory is created if it
   * doesn't exist. Pi writes one `.jsonl` file per session here.
   */
  sessionDir?: string;
  /**
   * If set, pi spawns with `--session <id-or-path>`, resuming that session.
   * Accepts a path to a `.jsonl` file or a partial UUID — pi resolves
   * either form against `--session-dir`.
   */
  resumeSession?: string;
}

// ── Driver API ──────────────────────────────────────────────────────

/** Return value from startPiDriver(). */
export interface PiDriver {
  /**
   * Send a user prompt to pi.
   * Returns a promise that resolves when the prompt is acknowledged.
   */
  prompt(text: string, images?: string[]): Promise<void>;
  /**
   * Send a steering message to pi (interrupts/guides current response).
   */
  steer(text: string): void;
  /**
   * Abort the current agent execution.
   */
  abort(): void;
  /**
   * Set the thinking/effort level at runtime.
   */
  setThinkingLevel(level: ThinkingLevel): void;
  /**
   * Switch to a specific model at runtime. Pi accepts provider+modelId pairs
   * (see pi RPC docs: set_model command). Pi 0.74 replies with the full Model
   * object; the driver doesn't currently surface that response — callers
   * track the chosen model independently if they care.
   */
  setModel(provider: string, modelId: string): void;
  /**
   * Request `get_session_stats` from pi (tokens, cost, contextUsage).
   * Resolves when pi responds; rejects on timeout or pi error.
   */
  getSessionStats(): Promise<PiSessionStats>;
  /**
   * Request `get_state` from pi. Returns the response data verbatim
   * (see pi RPC docs `get_state`): contains `sessionFile`, `sessionId`,
   * `model`, `thinkingLevel`, `messageCount`, …
   */
  getState(): Promise<Record<string, unknown>>;
  /**
   * Switch the running pi process to a different session file
   * (`switch_session` RPC). Pi reloads the .jsonl at `sessionPath`.
   * Returns whether the switch was accepted (it can be cancelled by a
   * `session_before_switch` extension event handler).
   */
  switchSession(sessionPath: string): Promise<boolean>;
  /**
   * Reply to a dialog `extension_ui_request` by pi's request id.
   * Fire-and-forget — pi does not acknowledge the response.
   */
  sendExtensionUIResponse(payload: ExtensionUIResponsePayload): void;
  /**
   * Manually compact pi's conversation context (`compact` RPC). Pi
   * responds with a summary and token-savings info; the driver awaits
   * the response so callers can surface the result.
   */
  compact(customInstructions?: string): Promise<{ summary?: string; tokensBefore?: number }>;
  /**
   * Start a fresh session inside the running pi process (`new_session`
   * RPC). Returns true unless cancelled by a `session_before_switch`
   * extension event handler.
   */
  newSession(parentSession?: string): Promise<boolean>;
  /**
   * Stop the pi subprocess and clean up resources.
   * Returns a promise that resolves when the subprocess exits.
   */
  stop(): Promise<void>;
  /**
   * Get the current accumulator state (branch, cwd, etc.).
   * Used by gravity-server to update branch after session-file resolution.
   */
  getAccState(): AccState;
}
