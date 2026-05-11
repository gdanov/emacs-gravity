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

/** Agent started (after user prompt submitted). */
export interface AgentStartEvent extends PiBaseEvent {
  type: "agent_start";
  message: {
    role: "user";
    content: Array<{ type: "text"; text: string } | { type: "input_image"; source: { type: "url"; url: string } }>;
  };
}

/** Agent finished (all turns complete). */
export interface AgentEndEvent extends PiBaseEvent {
  type: "agent_end";
  result: {
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

/** Tool execution started. */
export interface ToolExecutionStartEvent extends PiBaseEvent {
  type: "tool_execution_start";
  tool_call_id: string;
  tool_name: string;
  tool_input: Record<string, unknown>;
  needs_response?: boolean; // true for permission/request tools
}

/** Tool execution completed. */
export interface ToolExecutionEndEvent extends PiBaseEvent {
  type: "tool_execution_end";
  tool_call_id: string;
  tool_name: string;
  tool_result: unknown;
  error?: string;
}

/** Model selected (logged on startup). */
export interface ModelSelectEvent extends PiBaseEvent {
  type: "model_select";
  model: string;
  provider: string;
}

/** Message update (text or thinking delta, or status change). */
export interface MessageUpdateEvent extends PiBaseEvent {
  type: "message_update";
  message_update: {
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
  | ModelSelectEvent
  | MessageUpdateEvent
  | ErrorEvent
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

/** Commands sent to pi via stdin (JSONL). */
export type PiCommand =
  | { type: "prompt"; message: string; images?: string[] }
  | { type: "steer"; message: string }
  | { type: "abort" }
  | { type: "set_thinking_level"; level: ThinkingLevel };

/** Event emitted by protocol.ts for parsed pi events. */
export type PiProtocolEvent = {
  event: PiEvent;
  raw?: string;
};

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
   * Stop the pi subprocess and clean up resources.
   * Returns a promise that resolves when the subprocess exits.
   */
  stop(): Promise<void>;
}
