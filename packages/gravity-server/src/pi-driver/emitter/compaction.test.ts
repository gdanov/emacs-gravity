// compaction.test.ts — Tests for the two payload compaction functions.

import { describe, it, expect } from "vitest";
import {
  compactMessageUpdate,
  compactToolExecutionUpdate,
  TOOL_PARTIAL_TAIL_CHARS,
  type ToolUpdateLastSeen,
} from "./compaction.js";

describe("compactMessageUpdate", () => {
  it("returns null when assistantMessageEvent is undefined", () => {
    expect(compactMessageUpdate({ type: "message_update" })).toBeNull();
    expect(compactMessageUpdate({})).toBeNull();
  });

  it("returns an envelope carrying only type + assistantMessageEvent", () => {
    const delta = { type: "text_delta", delta: "hello" };
    const out = compactMessageUpdate({ assistantMessageEvent: delta });
    expect(out).toEqual({
      type: "message_update",
      assistantMessageEvent: delta,
    });
  });

  it("does NOT mutate the input event object", () => {
    const input = { assistantMessageEvent: { type: "text_delta", delta: "x" } };
    const snapshot = JSON.stringify(input);
    compactMessageUpdate(input);
    expect(JSON.stringify(input)).toBe(snapshot);
  });
});

describe("compactToolExecutionUpdate", () => {
  function makeLastSeen(): Map<string, ToolUpdateLastSeen> {
    return new Map();
  }

  it("returns null when toolCallId is missing or not a string", () => {
    const seen = makeLastSeen();
    expect(
      compactToolExecutionUpdate({ partialResult: "x" }, seen, 0),
    ).toBeNull();
    expect(
      compactToolExecutionUpdate(
        { toolCallId: 123 as unknown as string, partialResult: "x" },
        seen,
        0,
      ),
    ).toBeNull();
  });

  it("string partial: kept as trailing 4000 chars, no marker when short", () => {
    const seen = makeLastSeen();
    const partial = "abc";
    const out = compactToolExecutionUpdate(
      { toolCallId: "t1", toolName: "bash", partialResult: partial },
      seen,
      1000,
    );
    expect(out?.["partialResult"]).toBe("abc");
    expect(out?.["partialResultTruncated"]).toBeUndefined();
    expect(out?.["toolCallId"]).toBe("t1");
    expect(out?.["toolName"]).toBe("bash");
    expect(out?.["type"]).toBe("tool_execution_update");
  });

  it("string partial: truncates to the trailing 4000 chars and sets marker", () => {
    const seen = makeLastSeen();
    const long = "x".repeat(TOOL_PARTIAL_TAIL_CHARS + 1000) + "TAIL";
    const out = compactToolExecutionUpdate(
      { toolCallId: "t1", toolName: "bash", partialResult: long },
      seen,
      1000,
    );
    const kept = out?.["partialResult"] as string;
    expect(kept.length).toBe(TOOL_PARTIAL_TAIL_CHARS);
    expect(kept.endsWith("TAIL")).toBe(true);
    expect(out?.["partialResultTruncated"]).toBe(true);
  });

  it("number partial: kept as-is, no truncation marker", () => {
    const seen = makeLastSeen();
    const out = compactToolExecutionUpdate(
      { toolCallId: "t1", partialResult: 42 },
      seen,
      1000,
    );
    expect(out?.["partialResult"]).toBe(42);
    expect(out?.["partialResultTruncated"]).toBeUndefined();
  });

  it("boolean partial: kept as-is", () => {
    const seen = makeLastSeen();
    const out = compactToolExecutionUpdate(
      { toolCallId: "t1", partialResult: false },
      seen,
      1000,
    );
    expect(out?.["partialResult"]).toBe(false);
  });

  it("object/array/undefined partial: partialResult key OMITTED", () => {
    const seen = makeLastSeen();
    for (const value of [{}, [], { nested: 1 }, undefined, null]) {
      const out = compactToolExecutionUpdate(
        { toolCallId: "t1", partialResult: value },
        seen,
        1000,
      );
      expect(out).not.toBeNull();
      expect(Object.prototype.hasOwnProperty.call(out!, "partialResult")).toBe(
        false,
      );
      expect(out?.["partialResultTruncated"]).toBeUndefined();
    }
  });

  it("coalesces: same tail within 250ms → null", () => {
    const seen = makeLastSeen();
    // First emission establishes the baseline.
    const first = compactToolExecutionUpdate(
      { toolCallId: "t1", partialResult: "hello" },
      seen,
      1000,
    );
    expect(first).not.toBeNull();
    // Same content, 100ms later → coalesced.
    const second = compactToolExecutionUpdate(
      { toolCallId: "t1", partialResult: "hello" },
      seen,
      1100,
    );
    expect(second).toBeNull();
  });

  it("does NOT coalesce when content differs (even within window)", () => {
    const seen = makeLastSeen();
    compactToolExecutionUpdate(
      { toolCallId: "t1", partialResult: "hello" },
      seen,
      1000,
    );
    const second = compactToolExecutionUpdate(
      { toolCallId: "t1", partialResult: "world" },
      seen,
      1100,
    );
    expect(second).not.toBeNull();
  });

  it("does NOT coalesce when window has elapsed even if content same", () => {
    const seen = makeLastSeen();
    compactToolExecutionUpdate(
      { toolCallId: "t1", partialResult: "hello" },
      seen,
      1000,
    );
    const second = compactToolExecutionUpdate(
      { toolCallId: "t1", partialResult: "hello" },
      seen,
      2000, // 1000ms later, well past the 250ms window
    );
    expect(second).not.toBeNull();
  });

  it("coalescing is per toolCallId", () => {
    const seen = makeLastSeen();
    compactToolExecutionUpdate(
      { toolCallId: "t1", partialResult: "hello" },
      seen,
      1000,
    );
    // Different toolCallId → not coalesced even if same content.
    const out = compactToolExecutionUpdate(
      { toolCallId: "t2", partialResult: "hello" },
      seen,
      1100,
    );
    expect(out).not.toBeNull();
  });

  it("does not mutate rawEvent", () => {
    const seen = makeLastSeen();
    const input = {
      toolCallId: "t1",
      toolName: "bash",
      partialResult: "hello",
    };
    const snapshot = JSON.stringify(input);
    compactToolExecutionUpdate(input, seen, 1000);
    expect(JSON.stringify(input)).toBe(snapshot);
  });
});