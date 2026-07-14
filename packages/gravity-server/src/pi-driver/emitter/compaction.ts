// compaction.ts — Payload compaction for two high-volume pi events.
//
// Per the spec, `message_update` and `tool_execution_update` are the only
// event types whose raw payload is too large / too noisy to forward
// unmodified. Everything else is shipped byte-identical (after `type`
// injection in index.ts).
//
// These two functions are pure: they read `rawEvent`, return a fresh
// envelope object, and DO NOT mutate the input. The caller (index.ts)
// is responsible for queueing the returned envelope (or skipping it if
// null).

/**
 * Build a `message_update` envelope from the raw pi event.
 *
 * Pi's streaming assistant deltas arrive as `{ message_update: { type,
 * delta, ... } }` inside `MessageUpdateEvent`. The translator on the
 * server side reads `assistantMessageEvent.delta` to accumulate text,
 * so we ship ONLY that inner delta object — no full message snapshot.
 *
 * Returns null when `assistantMessageEvent` is missing, signaling to the
 * caller to drop the event entirely (do not enqueue).
 */
export function compactMessageUpdate(
  rawEvent: Record<string, unknown>,
): Record<string, unknown> | null {
  const assistantMessageEvent = rawEvent["assistantMessageEvent"];
  if (assistantMessageEvent === undefined) return null;
  return {
    type: "message_update",
    assistantMessageEvent,
  };
}

/** Tail truncation length for streaming tool partial results. */
export const TOOL_PARTIAL_TAIL_CHARS = 4000;

/** Coalescing window for tool_execution_update ticks. */
export const TOOL_UPDATE_COALESCE_MS = 250;

/** Last seen state per toolCallId, used by compactToolExecutionUpdate. */
export interface ToolUpdateLastSeen {
  atMs: number;
  /** Truncated tail of the last partialResult string (empty when the
   *  last partialResult wasn't a string). */
  tail: string;
}

/**
 * Build a `tool_execution_update` envelope from the raw pi event,
 * with source-side coalescing and tail-truncation of string partials.
 *
 * Behavior (matches the frozen contract in plan.md §3):
 *  - If rawEvent.toolCallId is undefined → null (caller drops).
 *  - Read lastSeen.get(toolCallId):
 *    - If present AND (nowMs - atMs) < 250 AND the newly-scalarized
 *      tail string equals the stored .tail → return null
 *      (intentional source-side coalescing; NOT a queue drop).
 *    - Otherwise → update lastSeen, return the envelope.
 *  - Scalarize partialResult:
 *      string  → keep only the trailing TOOL_PARTIAL_TAIL_CHARS chars;
 *                if truncated, set `partialResultTruncated: true`.
 *      number/boolean → keep as-is.
 *      object/array/undefined → OMIT the `partialResult` key entirely.
 */
export function compactToolExecutionUpdate(
  rawEvent: Record<string, unknown>,
  lastSeen: Map<string, ToolUpdateLastSeen>,
  nowMs: number,
): Record<string, unknown> | null {
  const toolCallId = rawEvent["toolCallId"];
  const toolName = rawEvent["toolName"];

  if (typeof toolCallId !== "string") return null;

  const partial = rawEvent["partialResult"];
  const { tail, kept, omitted } = scalarizePartial(partial);

  // Coalesce only when the partial carried actual content to compare
  // on. When the partial was omitted (object/array/undefined/null) the
  // envelope carries no `partialResult` at all, so there is no
  // meaningful "same tail" to dedupe against — letting those through
  // unconditionally is also what the per-tool update consumer expects.
  const prev = lastSeen.get(toolCallId);
  if (
    !omitted &&
    prev !== undefined &&
    nowMs - prev.atMs < TOOL_UPDATE_COALESCE_MS &&
    prev.tail === tail
  ) {
    return null;
  }

  lastSeen.set(toolCallId, { atMs: nowMs, tail });

  const envelope: Record<string, unknown> = {
    type: "tool_execution_update",
    toolCallId,
  };
  if (typeof toolName === "string") {
    envelope["toolName"] = toolName;
  }
  if (!omitted) {
    envelope["partialResult"] = kept;
    if (typeof partial === "string" && partial.length > TOOL_PARTIAL_TAIL_CHARS) {
      envelope["partialResultTruncated"] = true;
    }
  }
  return envelope;
}

/** Result of scalarizing a tool partial result. */
interface ScalarizedPartial {
  /** Truncated tail (empty when input wasn't a string). */
  tail: string;
  /** Value to put on the envelope's partialResult key. */
  kept: string | number | boolean;
  /** True when partialResult should be omitted entirely. */
  omitted: boolean;
}

function scalarizePartial(input: unknown): ScalarizedPartial {
  if (typeof input === "string") {
    const truncated = input.length > TOOL_PARTIAL_TAIL_CHARS;
    const tail = truncated
      ? input.slice(input.length - TOOL_PARTIAL_TAIL_CHARS)
      : input;
    return { tail, kept: tail, omitted: false };
  }
  if (typeof input === "number" || typeof input === "boolean") {
    // Coalesce key for non-strings: use the stringified form so two
    // identical numbers collapse but distinct ones don't.
    return { tail: String(input), kept: input, omitted: false };
  }
  // object / array / undefined / null / symbol / function → omit
  return { tail: "", kept: "", omitted: true };
}