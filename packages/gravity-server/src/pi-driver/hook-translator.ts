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
  AgentEndEvent,
  TurnStartEvent,
  TurnEndEvent,
  ToolExecutionStartEvent,
  ToolExecutionEndEvent,
  MessageStartEvent,
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
  accUserPromptMessage,
  accAgentEnd,
  accSetEffortLevel,
} from "./turn-accumulator.js";

const stamp = (state: AccState, r: TranslationResult): TranslationResult =>
  ({ ...r, sessionId: state.sessionId });

/**
 * Translate a pi event into gravity events.
 *
 * Returns either { kind: "emit", results } with one or more events to emit
 * in order, or { kind: "noop" } when only state was mutated (text deltas,
 * turn boundaries, tool start). Every TranslationResult carries `sessionId`
 * from `state.sessionId`; callers do not need to stamp it again. The
 * translator is the single source of truth for emitted events — there is
 * no shared queue between handlers.
 */
export function translatePiEvent(
  event: PiEvent,
  state: AccState,
): TranslateEventResult {
  switch (event.type) {
    case "agent_start": {
      // Pi 0.74's agent_start is bare (no message). The user prompt arrives in
      // a later message_start event with role "user" — handled below.
      const events = accAgentStart(state);
      return { kind: "emit", results: events.map((r) => stamp(state, r)) };
    }

    case "agent_end": {
      const e = event as AgentEndEvent;
      const usage: TokenUsage | undefined = e.result?.usage
        ? {
            input_tokens: e.result.usage.input_tokens,
            output_tokens: e.result.usage.output_tokens,
            cache_read_input_tokens: e.result.usage.cache_read_input_tokens ?? 0,
            cache_creation_input_tokens: e.result.usage.cache_creation_input_tokens ?? 0,
          }
        : undefined;
      const stop = accAgentEnd(
        state,
        e.result?.type ?? "success",
        usage,
        e.result?.error,
      );
      return { kind: "emit", results: [stamp(state, stop)] };
    }

    case "turn_start": {
      const e = event as TurnStartEvent;
      accTurnStart(state, e.turn_id);
      return { kind: "noop" };
    }

    case "turn_end": {
      const e = event as TurnEndEvent;
      accTurnEnd(state, e.turn_id);
      return { kind: "noop" };
    }

    case "tool_execution_start": {
      // Pi 0.74 wire format: { toolCallId, toolName, args }. Older drafts of
      // our types used snake_case; pi never emitted that shape. Read both
      // forms defensively in case pi changes back.
      const e = event as ToolExecutionStartEvent;
      const id = e.toolCallId ?? e.tool_call_id ?? "";
      const name = e.toolName ?? e.tool_name ?? "";
      const input = e.args ?? e.tool_input ?? {};
      accToolStart(state, id, name, input);
      return { kind: "noop" };
    }

    case "tool_execution_end": {
      // Pi 0.74 wire format: { toolCallId, toolName, result, isError }.
      const e = event as ToolExecutionEndEvent;
      const id = e.toolCallId ?? e.tool_call_id ?? "";
      const name = e.toolName ?? e.tool_name ?? "";
      const toolResult = e.result ?? e.tool_result;
      const errorMsg = e.isError === true
        ? (e.error ?? "tool execution failed")
        : e.error;
      const results = accToolEnd(state, id, name, toolResult, errorMsg);
      if (results.length === 0) return { kind: "noop" };
      return { kind: "emit", results };
    }

    // Pi emits streaming partial-result updates between start and end.
    // No event for us to emit; just ignore.
    case "tool_execution_update":
      return { kind: "noop" };

    // Pi 0.74 emits message_start / message_end as full snapshot events.
    // For user-role messages, this is where the prompt text lives — extract
    // it once (on message_start) and emit UserPromptSubmit. Assistant-role
    // messages are streamed via message_update and surfaced elsewhere.
    case "message_start": {
      const e = event as MessageStartEvent;
      const msg = e.message;
      if (msg?.role === "user" && Array.isArray(msg.content)) {
        const text = msg.content
          .filter((c) => c?.type === "text" && typeof c.text === "string")
          .map((c) => c.text as string)
          .join("");
        const results = accUserPromptMessage(state, text);
        if (results.length > 0) {
          return { kind: "emit", results: results.map((r) => stamp(state, r)) };
        }
      }
      return { kind: "noop" };
    }

    case "message_end":
      return { kind: "noop" };

    case "message_update": {
      // Pi 0.74: { assistantMessageEvent: { type, contentIndex, partial, ... } }
      // Also accept the legacy {message_update: {...}} shape for safety.
      const e = event as MessageUpdateEvent;
      const update = e.assistantMessageEvent ?? e.message_update;
      if (!update) return { kind: "noop" };
      if (update.type === "text_delta" && update.delta) {
        accTextDelta(state, update.delta);
      } else if (update.type === "thinking_delta" && update.delta) {
        accThinkingDelta(state, update.delta);
      }
      // Snapshot-style updates (thinking_start, content_start, etc.) carry
      // no incremental delta; ignore.
      return { kind: "noop" };
    }

    // Pi emits flat text_delta / thinking_delta events too (in some modes),
    // not always wrapped in message_update. Handle both.
    case "text_delta": {
      const e = event as { delta?: string };
      if (e.delta) accTextDelta(state, e.delta);
      return { kind: "noop" };
    }

    case "thinking_delta": {
      const e = event as { delta?: string };
      if (e.delta) accThinkingDelta(state, e.delta);
      return { kind: "noop" };
    }

    case "model_select": {
      const e = event as ModelSelectEvent;
      accModelSelect(state, e.model, e.provider);
      const hookData: HookData = {
        model: e.model,
        cwd: state.cwd,
      };
      return {
        kind: "emit",
        results: [{ hookEvent: "SessionStart", hookData, sessionId: state.sessionId }],
      };
    }

    case "error": {
      const e = event as ErrorEvent;
      process.stderr.write(`[pi-adapter] error event: ${e.error}\n`);
      return { kind: "noop" };
    }

    default:
      return { kind: "noop" };
  }
}

/**
 * Create a SessionEnd translation result. Used by mod.ts on subprocess exit.
 */
export function createSessionEnd(state: AccState): TranslationResult {
  return {
    hookEvent: "SessionEnd",
    hookData: {
      session_id: state.sessionId,
      cwd: state.cwd,
    },
    sessionId: state.sessionId,
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
    sessionId: state.sessionId,
  };
}