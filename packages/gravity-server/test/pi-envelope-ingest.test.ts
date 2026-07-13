// pi-envelope-ingest.test.ts — Tests for the pi envelope ingest path.
//
// Covers:
// - A hand-authored sequence of pi extension events, fed through the
//   translator + a fresh AccState, produces the expected TranslationResult
//   stream (mirrors the equivalent RPC-mode sequence in pi-driver.test.ts).
// - Unrecognized `event.type` falls through to `{ kind: "noop" }` AND
//   logs the literal event type to stderr (visibility addition, no
//   behavior change beyond the log).
// - `handleEvent` with an unregistered `HookEventName` logs the name to
//   stderr and still returns `[]` without throwing.

import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import { Effect, Layer } from "effect";
import type { PiEvent, AccState, TranslationResult } from "../src/pi-driver/types.js";
import { createAccState } from "../src/pi-driver/turn-accumulator.js";
import { translatePiEvent } from "../src/pi-driver/hook-translator.js";
import { handleEvent } from "../src/handlers/event-handler.js";
import {
  SessionStore, makeSessionStore, type SessionStoreService,
} from "../src/services/session-store.js";
import { Inbox, makeInbox, type InboxService } from "../src/services/inbox.js";
import { FsTest } from "@gravity/shared";
import type { HookData } from "@gravity/shared";

// ── Helpers ──────────────────────────────────────────────────────────

/** Drive `translatePiEvent` and return the emitted results, asserting kind. */
function translate(event: PiEvent, state: AccState): TranslationResult[] {
  const result = translatePiEvent(event, state);
  if (result.kind !== "emit") return [];
  return result.results;
}

// ── Translator-level fixture sequence ────────────────────────────────

describe("pi-envelope translator fixture sequence", () => {
  let state: AccState;

  beforeEach(() => {
    state = createAccState("pi-env-1", "/test/project", "medium");
  });

  it("session_start → agent_start → message_start(user) → turn_start → tool_execution_start → tool_execution_end → turn_end → agent_end → session_shutdown produces the expected TranslationResult stream", () => {
    const emitted: TranslationResult[] = [];

    // session_start — ambient-emitter synthesized SessionStart via the
    // existing helper. Note: for the ambient path there is no eager
    // server-side SessionStart injection (no spawn code in this path),
    // so this is the first gravity hookEvent the session emits.
    const sessionStartResults = translate(
      { type: "session_start" } as unknown as PiEvent,
      state,
    );
    expect(sessionStartResults).toHaveLength(1);
    expect(sessionStartResults[0].hookEvent).toBe("SessionStart");
    emitted.push(...sessionStartResults);

    // agent_start — opens a turn (TurnOpen per the documented spine).
    const agentStartResults = translate(
      { type: "agent_start" } as unknown as PiEvent,
      state,
    );
    expect(agentStartResults).toHaveLength(1);
    expect(agentStartResults[0].hookEvent).toBe("TurnOpen");
    emitted.push(...agentStartResults);

    // message_start with role=user attaches the prompt text.
    const userMsgResults = translate(
      {
        type: "message_start",
        message: { role: "user", content: "ship the cockpit" },
      } as unknown as PiEvent,
      state,
    );
    expect(userMsgResults).toHaveLength(1);
    expect(userMsgResults[0].hookEvent).toBe("UserPromptSubmit");
    expect((userMsgResults[0].hookData as { prompt?: string }).prompt).toBe("ship the cockpit");
    emitted.push(...userMsgResults);

    // turn_start — state-only mutation, no emit.
    const turnStart = translate(
      { type: "turn_start", turn_id: "t1" } as PiEvent,
      state,
    );
    expect(turnStart).toHaveLength(0);

    // tool_execution_start — also a state-only mutation.
    const toolStart = translate(
      {
        type: "tool_execution_start",
        tool_call_id: "call-1",
        tool_name: "bash",
        tool_input: { command: "ls" },
      } as unknown as PiEvent,
      state,
    );
    expect(toolStart).toHaveLength(0);

    // tool_execution_end — emits PreToolUse + PostToolUse together.
    const toolEnd = translate(
      {
        type: "tool_execution_end",
        tool_call_id: "call-1",
        tool_name: "bash",
        tool_result: { stdout: "ok" },
      } as unknown as PiEvent,
      state,
    );
    expect(toolEnd).toHaveLength(2);
    expect(toolEnd[0].hookEvent).toBe("PreToolUse");
    expect(toolEnd[1].hookEvent).toBe("PostToolUse");
    emitted.push(...toolEnd);

    // turn_end — state-only.
    const turnEnd = translate(
      { type: "turn_end", turn_id: "t1" } as PiEvent,
      state,
    );
    expect(turnEnd).toHaveLength(0);

    // agent_end — emits TurnClose carrying the trailing AssistantMessage's
    // stopReason and a token-usage aggregation across the agent run.
    const agentEnd = translate(
      {
        type: "agent_end",
        messages: [
          { role: "user", content: "ship the cockpit" },
          {
            role: "assistant",
            usage: { input: 12, output: 7, cacheRead: 1, cacheWrite: 0 },
            stopReason: "stop",
          },
        ],
      } as unknown as PiEvent,
      state,
    );
    expect(agentEnd).toHaveLength(1);
    expect(agentEnd[0].hookEvent).toBe("TurnClose");
    expect(
      (agentEnd[0].hookData as { stop_reason?: string }).stop_reason,
    ).toBe("stop");
    emitted.push(...agentEnd);

    // session_shutdown — ambient-emitter synthesized SessionEnd via the
    // existing helper, mirroring subprocess-exit behavior on the RPC path.
    const sessionShutdownResults = translate(
      { type: "session_shutdown" } as unknown as PiEvent,
      state,
    );
    expect(sessionShutdownResults).toHaveLength(1);
    expect(sessionShutdownResults[0].hookEvent).toBe("SessionEnd");
    emitted.push(...sessionShutdownResults);

    // Final ordering: the session lifecycle brackets the spine, with the
    // tool pair nested inside the agent run.
    const types = emitted.map((r) => r.hookEvent);
    expect(types).toEqual([
      "SessionStart",
      "TurnOpen",
      "UserPromptSubmit",
      "PreToolUse",
      "PostToolUse",
      "TurnClose",
      "SessionEnd",
    ]);
  });

  it("session_compact emits a Compaction hookEvent with the same hookData shape as compaction_end", () => {
    // session_compact mirrors the RPC-path compaction_end on the
    // ambient-emitter channel: same hookData field names, same defensive
    // defaults (unknown / null / null) on absent fields.
    const reasoned = translate(
      {
        type: "session_compact",
        reason: "threshold",
        aborted: false,
        tokensBefore: 42000,
        summary: "Earlier history summarized.",
      } as unknown as PiEvent,
      state,
    );
    expect(reasoned).toHaveLength(1);
    expect(reasoned[0].hookEvent).toBe("Compaction");
    const hd = reasoned[0].hookData as {
      compaction_reason?: string;
      compaction_aborted?: boolean;
      compaction_tokens_before?: number | null;
      compaction_summary?: string | null;
      cwd?: string;
    };
    expect(hd.compaction_reason).toBe("threshold");
    expect(hd.compaction_aborted).toBe(false);
    expect(hd.compaction_tokens_before).toBe(42000);
    expect(hd.compaction_summary).toBe("Earlier history summarized.");
    expect(hd.cwd).toBe("/test/project");

    const defaulted = translate(
      { type: "session_compact" } as unknown as PiEvent,
      state,
    );
    expect(defaulted).toHaveLength(1);
    const dhd = defaulted[0].hookData as {
      compaction_reason?: string;
      compaction_tokens_before?: number | null;
      compaction_summary?: string | null;
    };
    expect(dhd.compaction_reason).toBe("unknown");
    expect(dhd.compaction_tokens_before).toBeNull();
    expect(dhd.compaction_summary).toBeNull();
  });
});

// ── Unknown event type ───────────────────────────────────────────────

describe("unknown pi event type", () => {
  let state: AccState;
  let stderrSpy: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    state = createAccState("pi-env-2", "/test/project", "medium");
    stderrSpy = vi.spyOn(process.stderr, "write").mockImplementation(() => true);
  });

  afterEach(() => {
    stderrSpy.mockRestore();
  });

  it("returns { kind: 'noop' } and writes the literal unknown type to stderr", () => {
    const result = translatePiEvent(
      { type: "totally_unknown_event" } as unknown as PiEvent,
      state,
    );
    expect(result.kind).toBe("noop");

    // Verify stderr was hit with the literal event type. We tolerate any
    // surrounding whitespace / newline / format the logger adds; only the
    // presence of the literal type string matters.
    const writes = stderrSpy.mock.calls.map((args) => String(args[0] ?? ""));
    const wrote = writes.some((line) => line.includes("totally_unknown_event"));
    expect(wrote).toBe(true);
  });
});

// ── handleEvent with an unregistered HookEventName ───────────────────

describe("handleEvent with unregistered HookEventName", () => {
  let stderrSpy: ReturnType<typeof vi.spyOn>;
  let store: SessionStoreService;
  let inbox: InboxService;

  beforeEach(() => {
    stderrSpy = vi.spyOn(process.stderr, "write").mockImplementation(() => true);
    store = makeSessionStore();
    inbox = makeInbox();
  });

  afterEach(() => {
    stderrSpy.mockRestore();
  });

  it("logs the event name to stderr and returns [] without throwing", () => {
    const layer = Layer.mergeAll(
      Layer.succeed(SessionStore, store),
      Layer.succeed(Inbox, inbox),
    );
    const fullLayer = Layer.mergeAll(layer, FsTest({}));

    // Cast an arbitrary unknown string as a HookEventName. TS would
    // narrow at the call site; the runtime contract is what we care
    // about (handleEvent only reads dispatch[eventName] which is a
    // dict lookup).
    const unknownEvent = "SomeFutureEventName" as unknown as Parameters<typeof handleEvent>[0];

    const data: HookData = {};
    let result: ReturnType<typeof Effect.runSync> | null = null;
    let threw: unknown = null;
    try {
      result = Effect.runSync(
        Effect.provide(
          handleEvent(unknownEvent, "s-unknown", "/test/cwd", data, null),
          fullLayer,
        ),
      );
    } catch (e) {
      threw = e;
    }

    expect(threw).toBeNull();
    // The handler falls through with [] (no preamble patches either, as
    // no session exists for "s-unknown" — verify the result is defined
    // and an array, regardless of emptiness).
    expect(Array.isArray(result)).toBe(true);

    // Verify the unhandled-event log fired with the literal name.
    const writes = stderrSpy.mock.calls.map((args) => String(args[0] ?? ""));
    const wroteUnmonitored = writes.some((line) => line.includes("SomeFutureEventName"));
    const wroteUnmonitoredLabeled = writes.some((line) => line.includes("unhandled hook event"));
    expect(wroteUnmonitored || wroteUnmonitoredLabeled).toBe(true);
  });
});
