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
  PiAgentMessage,
  PiAssistantMessage,
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
  accSetBranch,
} from "./turn-accumulator.js";

const stamp = (state: AccState, r: TranslationResult): TranslationResult =>
  ({ ...r, sessionId: state.sessionId });

/**
 * Sum AssistantMessage usage across an agent run's messages[].
 *
 * Pi's per-message `Usage` uses camelCase fields (`input`, `output`,
 * `cacheRead`, `cacheWrite`) — gravity's `TokenUsage` uses snake_case
 * (`input_tokens`, `output_tokens`, `cache_read_input_tokens`,
 * `cache_creation_input_tokens`). This function translates and aggregates.
 *
 * Falls back to the legacy `result.usage` shape if `messages` is missing
 * (pi 0.74 does not emit `result.usage`; retained defensively).
 */
function extractAgentRunUsage(
  messages: PiAgentMessage[] | undefined,
): TokenUsage | undefined {
  if (!Array.isArray(messages) || messages.length === 0) return undefined;
  let input = 0;
  let output = 0;
  let cacheRead = 0;
  let cacheWrite = 0;
  let seen = false;
  for (const m of messages) {
    if (m?.role !== "assistant") continue;
    const u = (m as PiAssistantMessage).usage;
    if (!u) continue;
    seen = true;
    input += u.input ?? 0;
    output += u.output ?? 0;
    cacheRead += u.cacheRead ?? 0;
    cacheWrite += u.cacheWrite ?? 0;
  }
  if (!seen) return undefined;
  return {
    input_tokens: input,
    output_tokens: output,
    cache_read_input_tokens: cacheRead,
    cache_creation_input_tokens: cacheWrite,
  };
}

/**
 * Pick a result type ("success" | "error" | "aborted"), the raw
 * `stopReason`, and an optional error message for the agent run.
 *
 * The trailing AssistantMessage's `stopReason` is the canonical signal
 * per pi 0.74 (`docs/session.md`). One of `"stop"`, `"length"`,
 * `"toolUse"`, `"error"`, `"aborted"`. Falls back to the legacy
 * `result.{type,error}` shape if `messages` is missing.
 */
function extractAgentRunResult(
  e: AgentEndEvent,
): { resultType: "success" | "error" | "aborted"; stopReason?: string; error?: string } {
  if (Array.isArray(e.messages) && e.messages.length > 0) {
    for (let i = e.messages.length - 1; i >= 0; i--) {
      const m = e.messages[i];
      if (m?.role !== "assistant") continue;
      const am = m as PiAssistantMessage;
      const sr = am.stopReason;
      if (sr === "error") return { resultType: "error", stopReason: sr, error: am.errorMessage };
      if (sr === "aborted") return { resultType: "aborted", stopReason: sr, error: am.errorMessage };
      return { resultType: "success", stopReason: sr };
    }
  }
  if (e.result) {
    return { resultType: e.result.type ?? "success", error: e.result.error };
  }
  return { resultType: "success" };
}

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
      // Pi 0.74 wire shape: { messages: AgentMessage[] }. Walk the messages
      // and sum AssistantMessage usage across the agent run. The trailing
      // AssistantMessage's stopReason is surfaced verbatim. Fall back to
      // the legacy { result: { usage, type, error } } shape if pi ever
      // reverts to it.
      const usage = extractAgentRunUsage(e.messages);
      const { resultType, stopReason, error } = extractAgentRunResult(e);

      const stop = accAgentEnd(state, resultType, usage, error, stopReason);
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
      // Pi's UserMessage.content is `string | (TextContent | ImageContent)[]`
      // (docs/session.md). Accept both shapes — handling only the array form
      // silently drops string-form prompts. The boundary still survives
      // (agent_start opened the turn) but the label is missed without this.
      const e = event as MessageStartEvent;
      const msg = e.message;
      if (msg?.role === "user") {
        const content = msg.content as unknown;
        let text = "";
        if (typeof content === "string") {
          text = content;
        } else if (Array.isArray(content)) {
          text = content
            .filter((c) => c?.type === "text" && typeof c.text === "string")
            .map((c) => c.text as string)
            .join("");
        }
        if (text.length > 0) {
          const results = accUserPromptMessage(state, text);
          if (results.length > 0) {
            return { kind: "emit", results: results.map((r) => stamp(state, r)) };
          }
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

    case "branch_update": {
      // Pi reports a git branch change. Update accumulator state so
      // the next SessionStart carries the correct branch field.
      const e = event as { branch: string | null };
      accSetBranch(state, e.branch);
      return { kind: "noop" };
    }

    // ── Documented pi events with no current gravity surface ────────────
    //
    // Listed explicitly so additions / renames in pi's event vocabulary
    // surface as TypeScript narrowing changes rather than silently falling
    // through `default`. See design/pi-adapter.md "Gaps and parity TODOs".

    case "queue_update":
      // Pending steering / follow-up queue changed. Informational.
      // TODO: surface as a hint to the user when the queue is non-empty.
      return { kind: "noop" };

    case "compaction_start": {
      // Pi is about to summarize older history. Mid-stream compaction can
      // fire during a long tool loop; turn boundaries are unaffected.
      const e = event as { reason?: string };
      process.stderr.write(`[pi-adapter] compaction_start reason=${e.reason ?? "?"}\n`);
      // TODO: emit a synthetic prompt-like marker in the turn tree so the
      // compaction boundary is visible to the user.
      return { kind: "noop" };
    }

    case "compaction_end": {
      const e = event as {
        reason?: string;
        aborted?: boolean;
        result?: { tokensBefore?: number; summary?: string };
      };
      process.stderr.write(
        `[pi-adapter] compaction_end reason=${e.reason ?? "?"} aborted=${e.aborted ?? false} tokensBefore=${e.result?.tokensBefore ?? "?"}\n`,
      );
      // TODO: surface the summary text on the turn tree (no spec yet).
      return { kind: "noop" };
    }

    case "auto_retry_start": {
      // Provider rate-limit / transient error retry. Could surface as a
      // status indicator; for now just log.
      const e = event as { attempt?: number; maxAttempts?: number; delayMs?: number };
      process.stderr.write(
        `[pi-adapter] auto_retry_start attempt=${e.attempt ?? "?"}/${e.maxAttempts ?? "?"} delayMs=${e.delayMs ?? "?"}\n`,
      );
      return { kind: "noop" };
    }

    case "auto_retry_end": {
      const e = event as { success?: boolean; attempt?: number; finalError?: string };
      const detail = e.success === false ? ` finalError=${e.finalError ?? "?"}` : "";
      process.stderr.write(
        `[pi-adapter] auto_retry_end success=${e.success ?? false} attempt=${e.attempt ?? "?"}${detail}\n`,
      );
      return { kind: "noop" };
    }

    case "extension_error": {
      const e = event as { extensionPath?: string; event?: string; error?: string };
      process.stderr.write(
        `[pi-adapter] extension_error path=${e.extensionPath ?? "?"} event=${e.event ?? "?"}: ${e.error ?? "?"}\n`,
      );
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
      branch: state.branch ?? undefined,
      model: state.modelName ?? undefined,
      effort_level: state.effortLevel,
    },
    sessionId: state.sessionId,
  };
}