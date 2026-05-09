// hook-translator.ts — Translate pi events to gravity-server HookData
//
// Pure function: translatePiEvent(event: PiEvent, state: AccState): TranslateEventResult
//
// The translator:
// - Handles content accumulation from message_update events
// - Maps pi events to gravity events
// - Updates accumulator state for turn batching
// - Emits TranslationResult objects for gravity-server to process

import type {
  PiEvent,
  AccState,
  TranslationResult,
  TranslateEventResult,
  AccTool,
  AgentStartEvent,
  AgentEndEvent,
  TurnStartEvent,
  TurnEndEvent,
  ToolExecutionStartEvent,
  ToolExecutionEndEvent,
  MessageUpdateEvent,
  ModelSelectEvent,
  ErrorEvent,
} from "./types.js";
import type { HookData, HookEventName, TokenUsage } from "@gravity/shared";
import {
  accTurnStart,
  accTurnEnd,
  accToolStart,
  accToolEnd,
  accTextDelta,
  accThinkingDelta,
  accModelSelect,
  accAgentStart,
  accAgentEnd,
  accSetEffortLevel,
  drainPendingEvents,
} from "./turn-accumulator.js";

/**
 * Translate a pi event into gravity events.
 *
 * Returns:
 * - { kind: "emit", result } — emit a gravity event now
 * - { kind: "accumulate" } — update state, don't emit yet
 * - { kind: "noop" } — no action needed
 *
 * After each call, check state.pendingToolEvents for any accumulated
 * tool events that should be emitted.
 */
export function translatePiEvent(
  event: PiEvent,
  state: AccState,
): TranslateEventResult {
  switch (event.type) {
    case "agent_start": {
      const e = event as AgentStartEvent;
      // Extract user prompt text from agent_start message
      let promptText = "";
      if (e.message?.content) {
        for (const content of e.message.content) {
          if (content.type === "text") {
            promptText += content.text;
          }
        }
      }

      const events = accAgentStart(state, promptText);
      // Add the UserPromptSubmit and SessionStart events
      // They will be emitted along with pending events
      state.pendingToolEvents.push(...events);

      // Emit SessionStart and UserPromptSubmit now
      return { kind: "emit", result: events[0] };
    }

    case "agent_end": {
      const e = event as AgentEndEvent;
      // Build token usage with required fields, optional fields only if present
      const usage: TokenUsage | undefined = e.result?.usage
        ? {
            input_tokens: e.result.usage.input_tokens,
            output_tokens: e.result.usage.output_tokens,
            cache_read_input_tokens: e.result.usage.cache_read_input_tokens ?? 0,
            cache_creation_input_tokens: e.result.usage.cache_creation_input_tokens ?? 0,
          }
        : undefined;
      const result = accAgentEnd(
        state,
        e.result?.type ?? "success",
        usage,
        e.result?.error,
      );

      // Also drain any remaining pending events
      const pending = drainPendingEvents(state);

      // Return the Stop event (the pending events should have been drained earlier)
      return { kind: "emit", result };
    }

    case "turn_start": {
      const e = event as TurnStartEvent;
      accTurnStart(state, e.turn_id);
      return { kind: "accumulate", state };
    }

    case "turn_end": {
      const e = event as TurnEndEvent;
      accTurnEnd(state, e.turn_id);

      // Drain any pending tool events created during this turn
      const pending = drainPendingEvents(state);

      if (pending.length > 0) {
        // Return the first pending event; the rest are queued in state
        return { kind: "emit", result: pending[0] };
      }

      return { kind: "noop" };
    }

    case "tool_execution_start": {
      const e = event as ToolExecutionStartEvent;
      accToolStart(state, e.tool_call_id, e.tool_name, e.tool_input);
      return { kind: "accumulate", state };
    }

    case "tool_execution_end": {
      const e = event as ToolExecutionEndEvent;
      accToolEnd(
        state,
        e.tool_call_id,
        e.tool_name,
        e.tool_result,
        e.error,
      );

      // Don't drain - just access first pending event (don't clear)
      if (state.pendingToolEvents.length > 0) {
        const first = state.pendingToolEvents[0];
        // Remove the first event from pending (without draining all)
        state.pendingToolEvents.shift();
        return { kind: "emit", result: first };
      }

      return { kind: "noop" };
    }

    case "message_update": {
      const e = event as MessageUpdateEvent;
      const update = e.message_update;

      if (update.type === "text_delta" && update.delta) {
        accTextDelta(state, update.delta);
        return { kind: "accumulate", state };
      }

      if (update.type === "thinking_delta" && update.delta) {
        accThinkingDelta(state, update.delta);
        return { kind: "accumulate", state };
      }

      // Status changes are informational only
      return { kind: "noop" };
    }

    case "model_select": {
      const e = event as ModelSelectEvent;
      accModelSelect(state, e.model, e.provider);
      // Also update hookData with model info
      const hookData: HookData = {
        model: e.model,
        cwd: state.cwd,
      };
      return {
        kind: "emit",
        result: {
          hookEvent: "SessionStart",
          hookData,
        },
      };
    }

    case "error": {
      const e = event as ErrorEvent;
      // Log error and treat as noop for now
      // The error will be visible in stderr
      process.stderr.write(`[pi-adapter] error event: ${e.error}\n`);
      return { kind: "noop" };
    }

    default:
      // Unknown event type — ignore
      return { kind: "noop" };
  }
}

/**
 * Get all pending events from state (for batch emission).
 */
export function getPendingEvents(state: AccState): TranslationResult[] {
  return [...state.pendingToolEvents];
}

/**
 * Create a SessionEnd translation result.
 */
export function createSessionEnd(state: AccState): TranslationResult {
  return {
    hookEvent: "SessionEnd",
    hookData: {
      session_id: state.sessionId,
      cwd: state.cwd,
    },
  };
}

/**
 * Create a SessionStart translation result for initial setup.
 */
export function createSessionStart(state: AccState): TranslationResult {
  return {
    hookEvent: "SessionStart",
    hookData: {
      session_id: state.sessionId,
      cwd: state.cwd,
      model: state.modelName ?? undefined,
      effort_level: state.effortLevel,
    },
  };
}