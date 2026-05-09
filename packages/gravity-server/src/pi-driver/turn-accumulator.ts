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
  TranslateEventResult,
  TranslationResult,
} from "./types.js";
import type { HookData, HookEventName, TokenUsage } from "@gravity/shared";

/**
 * Create initial accumulator state for a new session.
 */
export function createAccState(sessionId: string, cwd: string, effortLevel: string = "medium"): AccState {
  return {
    sessionId,
    cwd,
    modelName: null,
    effortLevel,
    pendingAssistantText: "",
    pendingAssistantThinking: "",
    pendingPostText: "",
    pendingPostThinking: "",
    currentToolUseId: null,
    currentToolName: null,
    currentToolInput: null,
    turns: [],
    currentTurn: -1,
    inTurn: false,
    pendingToolEvents: [],
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
 * Emit a PreToolUse event for the current tool.
 */
function emitPreToolUse(state: AccState): TranslationResult {
  const toolUseId = state.currentToolUseId ?? `tool_${Date.now()}`;
  const toolName = state.currentToolName ?? "unknown";
  const toolInput = state.currentToolInput ?? {};
  const { assistantText, assistantThinking } = flushPendingAssistantContext(state);

  const hookData: HookData = {
    tool_name: toolName,
    tool_use_id: toolUseId,
    tool_input: toolInput,
    assistant_text: assistantText,
    assistant_thinking: assistantThinking,
    cwd: state.cwd,
  };

  return {
    hookEvent: "PreToolUse",
    hookData,
  };
}

/**
 * Emit a PostToolUse event for the completed tool.
 */
function emitPostToolUse(state: AccState, result: unknown, error: string | null): TranslationResult {
  const toolUseId = state.currentToolUseId ?? "";
  const toolName = state.currentToolName ?? "";
  const toolInput = state.currentToolInput ?? {};
  const { postText, postThinking } = flushPendingPostContext(state);

  // Also grab any remaining assistant context (might have arrived after tool started)
  const { assistantText, assistantThinking } = flushPendingAssistantContext(state);

  const hookData: HookData = {
    tool_name: toolName,
    tool_use_id: toolUseId,
    tool_input: toolInput,
    assistant_text: assistantText,
    assistant_thinking: assistantThinking,
    post_tool_text: postText,
    post_tool_thinking: postThinking,
    cwd: state.cwd,
    ...(error ? { error } : {}),
  };

  // Reset tool state
  state.currentToolUseId = null;
  state.currentToolName = null;
  state.currentToolInput = null;

  return {
    hookEvent: error ? "PostToolUseFailure" : "PostToolUse",
    hookData,
  };
}

/**
 * Called on turn_start from pi.
 * Creates a new AccTurn and sets inTurn = true.
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
 * Emits all pending tool events and finalizes the turn.
 */
export function accTurnEnd(state: AccState, turnId: string): AccState {
  if (!state.inTurn) return state;

  // Emit PreToolUse if there's a pending tool
  if (state.currentToolUseId && state.currentToolName) {
    const preEvent = emitPreToolUse(state);
    state.pendingToolEvents.push(preEvent);

    // Track tool in turn
    const turn = state.turns[state.turns.length - 1];
    if (turn) {
      const tool: AccTool = {
        toolUseId: state.currentToolUseId,
        toolName: state.currentToolName,
        toolInput: state.currentToolInput ?? {},
        assistantText: undefined,
        assistantThinking: undefined,
        startTime: Date.now(),
        endTime: Date.now(),
        result: null,
        error: null,
        postText: undefined,
        postThinking: undefined,
      };
      turn.tools.push(tool);
    }
  }

  // Emit PostToolUse for any tool that ended
  // (tool_execution_end events are accumulated separately)

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

  // Flush pending text before starting tool (this text precedes the tool)
  flushPendingAssistantContext(state);

  return state;
}

/**
 * Called on tool_execution_end from pi.
 * Emits PreToolUse + PostToolUse events for this tool.
 */
export function accToolEnd(
  state: AccState,
  toolCallId: string,
  toolName: string,
  toolResult: unknown,
  error?: string,
): AccState {
  // Flush post-tool context first
  const { postText, postThinking } = flushPendingPostContext(state);

  // If this is the current tool, emit the paired events
  if (state.currentToolUseId === toolCallId || state.currentToolName === toolName) {
    const toolUseId = state.currentToolUseId ?? toolCallId;
    const toolInput = state.currentToolInput ?? {};

    // Flush any remaining assistant context
    const { assistantText, assistantThinking } = flushPendingAssistantContext(state);

    const hookData: HookData = {
      tool_name: toolName,
      tool_use_id: toolUseId,
      tool_input: toolInput,
      assistant_text: assistantText,
      assistant_thinking: assistantThinking,
      post_tool_text: postText,
      post_tool_thinking: postThinking,
      cwd: state.cwd,
      ...(error ? { error } : {}),
    };

    state.pendingToolEvents.push({
      hookEvent: "PreToolUse",
      hookData,
    });

    state.pendingToolEvents.push({
      hookEvent: error ? "PostToolUseFailure" : "PostToolUse",
      hookData,
    });

    // Track in turn
    const turn = state.turns[state.turns.length - 1];
    if (turn) {
      const tool: AccTool = {
        toolUseId,
        toolName,
        toolInput,
        assistantText: assistantText ?? undefined,
        assistantThinking: assistantThinking ?? undefined,
        startTime: Date.now() - 100, // approximate
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
  }

  return state;
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
 * Called on agent_start (user prompt submission).
 * Emits UserPromptSubmit + SessionStart.
 */
export function accAgentStart(state: AccState, promptText: string): TranslationResult[] {
  const events: TranslationResult[] = [];

  // SessionStart event
  events.push({
    hookEvent: "SessionStart",
    hookData: {
      session_id: state.sessionId,
      cwd: state.cwd,
      model: state.modelName ?? undefined,
      effort_level: state.effortLevel,
    },
  });

  // UserPromptSubmit event
  events.push({
    hookEvent: "UserPromptSubmit",
    hookData: {
      prompt: promptText,
      cwd: state.cwd,
    },
  });

  return events;
}

/**
 * Called on agent_end (all turns complete).
 * Emits Stop event with token usage.
 */
export function accAgentEnd(
  state: AccState,
  resultType: "success" | "error" | "aborted",
  usage?: TokenUsage,
  error?: string,
): TranslationResult {
  // Flush any remaining pending text as stop_text
  const { assistantText, assistantThinking } = flushPendingAssistantContext(state);

  const hookData: HookData = {
    stop_text: assistantText,
    stop_thinking: assistantThinking,
    cwd: state.cwd,
    ...(usage ? { token_usage: usage } : {}),
  };

  return {
    hookEvent: "Stop",
    hookData,
  };
}

/**
 * Drain all pending events from the accumulator.
 * Returns them and clears the pending queue.
 */
export function drainPendingEvents(state: AccState): TranslationResult[] {
  const events = [...state.pendingToolEvents];
  state.pendingToolEvents = [];
  return events;
}

/**
 * Update effort level in state.
 */
export function accSetEffortLevel(state: AccState, level: string): AccState {
  state.effortLevel = level;
  return state;
}
