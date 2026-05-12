// turn-accumulator.ts — State machine for batching pi turns into gravity turns
//
// A gravity turn = one user prompt + zero or more pi turns (tool calls
// within that response cycle).
//
// The accumulator:
// - Collects pending assistant text/thinking from message_update deltas
// - Tracks tool start/end between turn boundaries
// - Emits gravity events when turn_end is received

import type {
  AccState,
  AccTurn,
  AccTool,
  TranslationResult,
} from "./types.js";
import type { HookData, TokenUsage } from "@gravity/shared";

/**
 * Create initial accumulator state for a new session.
 */
export function createAccState(sessionId: string, cwd: string, effortLevel: string = "medium"): AccState {
  return {
    sessionId,
    cwd,
    branch: null,
    modelName: null,
    effortLevel,
    pendingAssistantText: "",
    pendingAssistantThinking: "",
    pendingPostText: "",
    pendingPostThinking: "",
    currentToolUseId: null,
    currentToolName: null,
    currentToolInput: null,
    currentToolStartTime: null,
    currentToolAssistantText: undefined,
    currentToolAssistantThinking: undefined,
    turns: [],
    currentTurn: -1,
    inTurn: false,
  };
}

/**
 * Flush any pending text/thinking and return as assistant context.
 */
function flushPendingAssistantContext(state: AccState): {
  assistantText: string | undefined;
  assistantThinking: string | undefined;
} {
  const assistantText = state.pendingAssistantText.trim() || undefined;
  const assistantThinking = state.pendingAssistantThinking.trim() || undefined;
  state.pendingAssistantText = "";
  state.pendingAssistantThinking = "";
  return { assistantText, assistantThinking };
}

/**
 * Flush any pending post-tool text/thinking.
 */
function flushPendingPostContext(state: AccState): {
  postText: string | undefined;
  postThinking: string | undefined;
} {
  const postText = state.pendingPostText.trim() || undefined;
  const postThinking = state.pendingPostThinking.trim() || undefined;
  state.pendingPostText = "";
  state.pendingPostThinking = "";
  return { postText, postThinking };
}

/**
 * Called on turn_start from pi.
 * Creates a new AccTurn and sets inTurn = true.
 *
 * Note: turnNumber is `currentTurn + 1` so pi turn numbering aligns with
 * gravity's convention (turn 0 = pre-prompt activity, turn 1 = first user
 * prompt). `state.currentTurn` is the 0-based index into `state.turns`.
 */
export function accTurnStart(state: AccState, turnId: string): AccState {
  state.inTurn = true;
  state.currentTurn++;

  const turn: AccTurn = {
    turnNumber: state.currentTurn + 1,
    startedAt: Date.now(),
    endedAt: null,
    tools: [],
    stepIndex: 0,
  };

  state.turns.push(turn);

  // Flush any previous pending context at turn boundary
  flushPendingAssistantContext(state);
  flushPendingPostContext(state);

  return state;
}

/**
 * Called on turn_end from pi.
 * Finalizes the turn. The pi protocol contract guarantees tool_execution_end
 * arrives before turn_end, so paired PreToolUse/PostToolUse events are handled
 * entirely by accToolEnd. This function only marks the turn as ended.
 */
export function accTurnEnd(state: AccState, turnId: string): AccState {
  if (!state.inTurn) return state;

  state.inTurn = false;

  // Mark turn as ended
  const turn = state.turns[state.turns.length - 1];
  if (turn) {
    turn.endedAt = Date.now();
  }

  return state;
}

/**
 * Called on tool_execution_start from pi.
 */
export function accToolStart(
  state: AccState,
  toolCallId: string,
  toolName: string,
  toolInput: Record<string, unknown>,
): AccState {
  state.currentToolUseId = toolCallId;
  state.currentToolName = toolName;
  state.currentToolInput = toolInput;
  state.currentToolStartTime = Date.now();

  // Snapshot whatever assistant text/thinking pi streamed BEFORE this tool
  // into per-tool slots. accToolEnd reads from these slots to populate the
  // emitted PreToolUse hookData. The flush also clears the pending
  // accumulator so the next tool's preceding-text window starts fresh.
  // (Earlier this just discarded the flushed values, which is why every
  // tool's assistantText/assistantThinking ended up null.)
  const flushed = flushPendingAssistantContext(state);
  state.currentToolAssistantText = flushed.assistantText;
  state.currentToolAssistantThinking = flushed.assistantThinking;

  return state;
}

/**
 * Called on tool_execution_end from pi.
 * Returns the paired PreToolUse + PostToolUse events for this tool.
 * Returns an empty array if the end event doesn't match the current tool
 * (the contract is that tool_execution_start precedes tool_execution_end).
 */
export function accToolEnd(
  state: AccState,
  toolCallId: string,
  toolName: string,
  toolResult: unknown,
  error?: string,
): TranslationResult[] {
  // Flush post-tool context first
  const { postText, postThinking } = flushPendingPostContext(state);

  // Match the current tool by ID. (Name-only fallback removed — see issue #3.)
  if (state.currentToolUseId !== toolCallId) {
    return [];
  }

  const toolUseId = state.currentToolUseId;
  const toolInput = state.currentToolInput ?? {};

  // Use the assistant text/thinking that accToolStart snapshotted from the
  // pending accumulator. The pending accumulator may have received MORE
  // text after the tool started (rare for pi — usually streaming pauses
  // during tool execution) but for our purposes the pre-tool snapshot is
  // the canonical "text shown before this tool" value.
  const assistantText = state.currentToolAssistantText;
  const assistantThinking = state.currentToolAssistantThinking;

  // The server's handlePostToolUse reads `tool_response` (not tool_result)
  // from the hookData and stores it on the Tool object via completeTool.
  // Without this field every tool ended up with result=undefined and the
  // UI couldn't render results.
  const hookData: HookData = {
    tool_name: toolName,
    tool_use_id: toolUseId,
    tool_input: toolInput,
    tool_response: toolResult,
    assistant_text: assistantText,
    assistant_thinking: assistantThinking,
    post_tool_text: postText,
    post_tool_thinking: postThinking,
    cwd: state.cwd,
    ...(error ? { error } : {}),
  };

  const results: TranslationResult[] = [
    { hookEvent: "PreToolUse", hookData, sessionId: state.sessionId },
    { hookEvent: error ? "PostToolUseFailure" : "PostToolUse", hookData, sessionId: state.sessionId },
  ];

  // Track in turn
  const turn = state.turns[state.turns.length - 1];
  if (turn) {
    const tool: AccTool = {
      toolUseId,
      toolName,
      toolInput,
      assistantText: assistantText ?? undefined,
      assistantThinking: assistantThinking ?? undefined,
      startTime: state.currentToolStartTime ?? Date.now(),
      endTime: Date.now(),
      result: toolResult,
      error: error ?? null,
      postText: postText ?? undefined,
      postThinking: postThinking ?? undefined,
    };
    turn.tools.push(tool);
  }

  // Reset tool state
  state.currentToolUseId = null;
  state.currentToolName = null;
  state.currentToolInput = null;
  state.currentToolStartTime = null;
  state.currentToolAssistantText = undefined;
  state.currentToolAssistantThinking = undefined;

  return results;
}

/**
 * Called on message_update with text_delta.
 */
export function accTextDelta(state: AccState, delta: string): AccState {
  state.pendingAssistantText += delta;
  return state;
}

/**
 * Called on message_update with thinking_delta.
 */
export function accThinkingDelta(state: AccState, delta: string): AccState {
  state.pendingAssistantThinking += delta;
  return state;
}

/**
 * Called on model_select from pi.
 */
export function accModelSelect(state: AccState, model: string, provider: string): AccState {
  state.modelName = model;
  return state;
}

/**
 * Called on agent_start (a new pi run begins). Emits TurnOpen.
 *
 * Pi 0.74's `agent_start` event is bare — it doesn't carry the user
 * message. The user's prompt arrives in a subsequent `message_start` event
 * with role "user"; use `accUserPromptMessage` to emit UserPromptSubmit
 * from there. The TurnOpen handler creates an empty turn that
 * UserPromptSubmit later attaches a label to.
 *
 * SessionStart for the pi session is synthesized eagerly at subprocess
 * spawn time by gravity-server's `startPiSession` — agent_start does NOT
 * emit SessionStart. A long-lived pi process can fire agent_start N times;
 * SessionStart must fire exactly once.
 */
export function accAgentStart(state: AccState): TranslationResult[] {
  return [{
    hookEvent: "TurnOpen",
    hookData: {
      cwd: state.cwd,
      source: "pi",
      branch: state.branch ?? undefined,
      model: state.modelName ?? undefined,
      effort_level: state.effortLevel,
    },
    sessionId: state.sessionId,
  }];
}

/**
 * Called on `message_start` (or `message_end`) with role "user". Emits the
 * gravity UserPromptSubmit event for that prompt. No-op if text is empty.
 *
 * Sets `source: "pi"` on the hookData so `handleUserPromptSubmit` routes
 * the event into `attachPrompt` (attach to the pre-opened pi turn) rather
 * than `addPrompt` (create a new turn). Without `source` the handler falls
 * back to checking `session.source`, which works as long as the eager
 * SessionStart from `startPiSession` has run first — but belt-and-
 * suspenders: any future pi path that bypasses that ordering would
 * otherwise silently degrade to "create new turn" + miss the boundary.
 */
export function accUserPromptMessage(state: AccState, promptText: string): TranslationResult[] {
  if (!promptText) return [];
  return [{
    hookEvent: "UserPromptSubmit",
    hookData: {
      prompt: promptText,
      cwd: state.cwd,
      source: "pi",
    },
    sessionId: state.sessionId,
  }];
}

/**
 * Called on agent_end (one pi prompt cycle completes).
 *
 * Emits TurnClose, which closes (and freezes) the current turn. The
 * gravity-server handler calls `closeTurn(stopText, stopThinking, usage)`
 * and additionally toggles claude status to idle + drops an "idle" inbox
 * item — that work used to live in the Stop handler and pi rode through
 * it; now the TurnClose handler owns it.
 *
 * Token usage comes from the trailing AssistantMessage in
 * `agent_end.messages[]`, not the imaginary `result.usage` (pi 0.74 does
 * not emit that field). The translator computes usage before calling here
 * and passes it in.
 */
export function accAgentEnd(
  state: AccState,
  resultType: "success" | "error" | "aborted",
  usage?: TokenUsage,
  error?: string,
  stopReason?: string,
): TranslationResult {
  // Flush any remaining pending text as stop_text
  const { assistantText, assistantThinking } = flushPendingAssistantContext(state);

  const hookData: HookData = {
    stop_text: assistantText,
    stop_thinking: assistantThinking,
    cwd: state.cwd,
    ...(usage ? { token_usage: usage } : {}),
    ...(stopReason ? { stop_reason: stopReason } : {}),
  };

  return {
    hookEvent: "TurnClose",
    hookData,
    sessionId: state.sessionId,
  };
}

/**
 * Update effort level in state.
 */
export function accSetEffortLevel(state: AccState, level: string): AccState {
  state.effortLevel = level;
  return state;
}

/**
 * Update the git branch in accumulator state. Called after the session
 * file path is resolved, so we can read the transcript and extract branch.
 */
export function accSetBranch(state: AccState, branch: string | null): AccState {
  state.branch = branch;
  return state;
}
