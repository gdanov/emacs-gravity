// pi-driver.test.ts — Unit tests for pi-driver module
//
// Tests:
// - types.ts: PiEvent types, translation result types
// - protocol.ts: JSONL parsing, RPC command formatting
// - turn-accumulator.ts: State machine for batching pi turns
// - hook-translator.ts: Event translation

import { describe, it, expect, beforeEach } from "vitest";
import type { PiEvent, AccState, TranslationResult, ThinkingLevel } from "../src/pi-driver/types.js";

// Import the modules
import { PiProtocol } from "../src/pi-driver/protocol.js";
import { createAccState, accTurnStart, accTurnEnd, accToolStart, accToolEnd, accTextDelta, accThinkingDelta, accAgentStart, accUserPromptMessage, accAgentEnd } from "../src/pi-driver/turn-accumulator.js";
import { translatePiEvent, createSessionStart, createSessionEnd } from "../src/pi-driver/hook-translator.js";
import { thinkingToEffort, normalizeThinkingLevel, createSessionMetadata, updateModel, updateThinkingLevel } from "../src/pi-driver/session.js";

// ── Types tests ─────────────────────────────────────────────────────

describe("PiEvent types", () => {
  it("should have all required event types", () => {
    const events: PiEvent[] = [
      { type: "text_delta", delta: "hello" },
      { type: "thinking_delta", delta: "thinking..." },
      { type: "turn_start", turn_id: "t1" },
      { type: "turn_end", turn_id: "t1" },
      { type: "agent_start" } as unknown as PiEvent,
      { type: "message_start", message: { role: "user", content: [{ type: "text", text: "hello" }] } } as unknown as PiEvent,
      { type: "agent_end", result: { type: "success" } },
      { type: "tool_execution_start", tool_call_id: "c1", tool_name: "bash", tool_input: {} },
      { type: "tool_execution_end", tool_call_id: "c1", tool_name: "bash", tool_result: {} },
      { type: "model_select", model: "claude-3", provider: "anthropic" },
      { type: "message_update", message_update: { type: "text_delta", delta: "hello" } },
      { type: "error", error: "something went wrong" },
    ];

    expect(events).toHaveLength(12);
  });

  it("should allow unknown event type", () => {
    const unknownEvent: PiEvent = { type: "unknown", raw: "some data" } as PiEvent;
    expect(unknownEvent.type).toBe("unknown");
  });
});

describe("ThinkingLevel", () => {
  it("should define valid thinking levels", () => {
    const levels: ThinkingLevel[] = ["off", "minimal", "low", "medium", "high", "xhigh"];
    expect(levels).toHaveLength(6);
  });
});

// ── Protocol tests ─────────────────────────────────────────────────

describe("PiProtocol", () => {
  it("should parse single JSON line", () => {
    const events: any[] = [];
    const proto = new PiProtocol({
      onEvent: (evt) => events.push(evt),
    });

    proto.feed('{"type":"text_delta","delta":"hello"}\n');
    expect(events).toHaveLength(1);
    expect(events[0].event.type).toBe("text_delta");
    expect(events[0].event.delta).toBe("hello");
  });

  it("should handle multiple lines", () => {
    const events: any[] = [];
    const proto = new PiProtocol({
      onEvent: (evt) => events.push(evt),
    });

    proto.feed('{"type":"text_delta","delta":"hello"}\n{"type":"turn_start","turn_id":"t1"}\n');
    expect(events).toHaveLength(2);
    expect(events[0].event.type).toBe("text_delta");
    expect(events[1].event.type).toBe("turn_start");
  });

  it("should handle partial JSON (buffering)", () => {
    const events: any[] = [];
    const proto = new PiProtocol({
      onEvent: (evt) => events.push(evt),
    });

    proto.feed('{"type":"text_delta","delta":"hel');
    proto.feed('lo"}\n');
    expect(events).toHaveLength(1);
    expect(events[0].event.delta).toBe("hello");
  });

  it("should skip empty lines", () => {
    const events: any[] = [];
    const proto = new PiProtocol({
      onEvent: (evt) => events.push(evt),
    });

    proto.feed('\n\n{"type":"text_delta","delta":"hello"}\n\n');
    expect(events).toHaveLength(1);
  });

  it("should handle malformed JSON", () => {
    const events: any[] = [];
    const proto = new PiProtocol({
      onEvent: (evt) => events.push(evt),
    });

    proto.feed('not json\n{"type":"text_delta","delta":"hello"}\n');
    expect(events).toHaveLength(2);
    expect(events[0].event.type).toBe("unknown");
    expect(events[1].event.type).toBe("text_delta");
  });

  it("should format prompt command", () => {
    const line = PiProtocol.formatPrompt("Hello, world!");
    const parsed = JSON.parse(line.trim());
    expect(parsed.type).toBe("prompt");
    expect(parsed.message).toBe("Hello, world!");
  });

  it("should format prompt with images", () => {
    const line = PiProtocol.formatPrompt("Describe this", ["https://example.com/img.png"]);
    const parsed = JSON.parse(line.trim());
    expect(parsed.images).toHaveLength(1);
    expect(parsed.images[0]).toBe("https://example.com/img.png");
  });

  it("should format steer command", () => {
    const line = PiProtocol.formatSteer("Try a different approach");
    const parsed = JSON.parse(line.trim());
    expect(parsed.type).toBe("steer");
    expect(parsed.message).toBe("Try a different approach");
  });

  it("should format abort command", () => {
    const line = PiProtocol.formatAbort();
    const parsed = JSON.parse(line.trim());
    expect(parsed.type).toBe("abort");
  });

  it("should format thinking level command", () => {
    const line = PiProtocol.formatThinkingLevel("high");
    const parsed = JSON.parse(line.trim());
    expect(parsed.type).toBe("set_thinking_level");
    expect(parsed.level).toBe("high");
  });
});

// ── Turn accumulator tests ─────────────────────────────────────────

describe("Turn accumulator", () => {
  let state: AccState;

  beforeEach(() => {
    state = createAccState("test-session", "/test/cwd", "medium");
  });

  it("should create initial state", () => {
    expect(state.sessionId).toBe("test-session");
    expect(state.cwd).toBe("/test/cwd");
    expect(state.effortLevel).toBe("medium");
    expect(state.inTurn).toBe(false);
    expect(state.currentTurn).toBe(-1);
  });

  it("should handle turn start", () => {
    const result = accTurnStart(state, "turn-1");
    expect(result.inTurn).toBe(true);
    expect(result.currentTurn).toBe(0);
    expect(result.turns).toHaveLength(1);
    expect(result.turns[0].turnNumber).toBe(1);
  });

  it("should handle multiple turn starts", () => {
    accTurnStart(state, "turn-1");
    accTurnStart(state, "turn-2");
    expect(state.turns).toHaveLength(2);
    expect(state.currentTurn).toBe(1);
  });

  it("should handle tool start", () => {
    accTurnStart(state, "turn-1");
    accToolStart(state, "call-1", "bash", { command: "ls" });
    expect(state.currentToolUseId).toBe("call-1");
    expect(state.currentToolName).toBe("bash");
    expect(state.currentToolInput.command).toBe("ls");
  });

  it("should accumulate text deltas", () => {
    accTextDelta(state, "Hello ");
    accTextDelta(state, "world!");
    expect(state.pendingAssistantText).toBe("Hello world!");
  });

  it("should accumulate thinking deltas", () => {
    accThinkingDelta(state, "Let me ");
    accThinkingDelta(state, "think about this");
    expect(state.pendingAssistantThinking).toBe("Let me think about this");
  });

  it("should flush pending context on tool start", () => {
    accTextDelta(state, "Before tool");
    accToolStart(state, "call-1", "bash", {});
    expect(state.pendingAssistantText).toBe("");
  });

  it("should emit PreToolUse and PostToolUse on tool end", () => {
    accTurnStart(state, "turn-1");
    accToolStart(state, "call-1", "bash", { command: "ls" });
    const events = accToolEnd(state, "call-1", "bash", { stdout: "file.txt" });

    expect(events).toHaveLength(2);
    expect(events[0].hookEvent).toBe("PreToolUse");
    expect(events[1].hookEvent).toBe("PostToolUse");
  });

  it("should emit PostToolUseFailure on tool error", () => {
    accTurnStart(state, "turn-1");
    accToolStart(state, "call-1", "bash", { command: "ls" });
    const events = accToolEnd(state, "call-1", "bash", {}, "Command failed");

    expect(events).toHaveLength(2);
    expect(events[1].hookEvent).toBe("PostToolUseFailure");
  });

  it("should handle agent_start: emit TurnOpen (pi 0.74's agent_start is bare)", () => {
    // Per the boundary mapping in design/pi-adapter.md, agent_start opens a
    // new gravity turn. SessionStart is synthesized once eagerly at pi
    // subprocess spawn time by gravity-server, not on every agent_start.
    const events = accAgentStart(state);
    expect(events).toHaveLength(1);
    expect(events[0].hookEvent).toBe("TurnOpen");
  });

  it("should handle user message_start: UserPromptSubmit with the prompt text", () => {
    const events = accUserPromptMessage(state, "Hello, pi!");
    expect(events).toHaveLength(1);
    expect(events[0].hookEvent).toBe("UserPromptSubmit");
    expect((events[0].hookData as any).prompt).toBe("Hello, pi!");
  });

  it("accUserPromptMessage is a noop for empty text", () => {
    expect(accUserPromptMessage(state, "")).toEqual([]);
  });

  it("should handle agent_end with token usage", () => {
    const result = accAgentEnd(state, "success", {
      input_tokens: 100,
      output_tokens: 50,
      cache_read_input_tokens: 0,
      cache_creation_input_tokens: 0,
    });
    // agent_end closes the gravity turn — TurnClose, not Stop. Stop is the
    // Claude Code hook event; the pi spine uses TurnClose to guarantee the
    // turn freezes even if no further prompt arrives.
    expect(result.hookEvent).toBe("TurnClose");
    expect(result.hookData.token_usage).toBeDefined();
  });

  it("forwards stopReason on TurnClose hookData when provided", () => {
    const result = accAgentEnd(state, "success", undefined, undefined, "length");
    expect((result.hookData as { stop_reason?: string }).stop_reason).toBe("length");
  });

  it("should emit turn end with no pending events", () => {
    accTurnStart(state, "turn-1");
    accTurnEnd(state, "turn-1");
    expect(state.inTurn).toBe(false);
  });
});

// ── Hook translator tests ──────────────────────────────────────────

describe("Hook translator", () => {
  let state: AccState;

  beforeEach(() => {
    state = createAccState("test-session", "/test/cwd", "medium");
  });

  it("agent_start emits TurnOpen (pi 0.74's agent_start is bare)", () => {
    // Pi 0.74 sends a bare `{type: "agent_start"}` with no message attached.
    // The translator maps it to TurnOpen — the per-prompt boundary signal.
    const event = { type: "agent_start" } as unknown as PiEvent;
    const result = translatePiEvent(event, state);
    expect(result.kind).toBe("emit");
    if (result.kind === "emit") {
      expect(result.results).toHaveLength(1);
      expect(result.results[0].hookEvent).toBe("TurnOpen");
    }
  });

  it("agent_end sums usage across AssistantMessages and extracts stopReason", () => {
    // Pi 0.74 wire shape: { messages: AgentMessage[] }. Each AssistantMessage
    // carries its own per-LLM-call usage; the gravity turn = the whole agent
    // run, so usage is summed across all assistant messages. stopReason is
    // taken from the trailing AssistantMessage.
    const event = {
      type: "agent_end",
      messages: [
        { role: "user", content: "hi" },
        {
          role: "assistant",
          usage: { input: 10, output: 5, cacheRead: 1, cacheWrite: 0 },
          stopReason: "toolUse",
        },
        { role: "toolResult", content: [] },
        {
          role: "assistant",
          usage: { input: 20, output: 8, cacheRead: 2, cacheWrite: 0 },
          stopReason: "length",
        },
      ],
    } as unknown as PiEvent;
    const result = translatePiEvent(event, state);
    expect(result.kind).toBe("emit");
    if (result.kind === "emit") {
      const r = result.results[0];
      expect(r.hookEvent).toBe("TurnClose");
      const usage = (r.hookData as { token_usage?: { input_tokens?: number; output_tokens?: number; cache_read_input_tokens?: number } }).token_usage;
      expect(usage?.input_tokens).toBe(30); // 10 + 20
      expect(usage?.output_tokens).toBe(13); // 5 + 8
      expect(usage?.cache_read_input_tokens).toBe(3); // 1 + 2
      expect((r.hookData as { stop_reason?: string }).stop_reason).toBe("length");
    }
  });

  it("message_start with role=user emits UserPromptSubmit", () => {
    const event = {
      type: "message_start",
      message: {
        role: "user",
        content: [{ type: "text", text: "Hello, pi!" }],
        timestamp: 0,
      },
    } as unknown as PiEvent;
    const result = translatePiEvent(event, state);
    expect(result.kind).toBe("emit");
    if (result.kind === "emit") {
      expect(result.results).toHaveLength(1);
      expect(result.results[0].hookEvent).toBe("UserPromptSubmit");
      expect((result.results[0].hookData as { prompt?: string }).prompt).toBe("Hello, pi!");
    }
  });

  it("message_start with role=assistant is a noop (assistant text comes via message_update)", () => {
    const event = {
      type: "message_start",
      message: { role: "assistant", content: [{ type: "text", text: "" }] },
    } as unknown as PiEvent;
    const result = translatePiEvent(event, state);
    expect(result.kind).toBe("noop");
  });

  it("should translate turn_start as noop (state-only mutation)", () => {
    const event: PiEvent = { type: "turn_start", turn_id: "turn-1" };
    const result = translatePiEvent(event, state);
    expect(result.kind).toBe("noop");
  });

  it("should translate tool_execution_start as noop (state-only mutation)", () => {
    const event: PiEvent = {
      type: "tool_execution_start",
      tool_call_id: "call-1",
      tool_name: "bash",
      tool_input: { command: "ls" },
    };
    const result = translatePiEvent(event, state);
    expect(result.kind).toBe("noop");
  });

  it("should translate tool_execution_end to emit PreToolUse and PostToolUse together", () => {
    const startEvent: PiEvent = {
      type: "tool_execution_start",
      tool_call_id: "call-1",
      tool_name: "bash",
      tool_input: { command: "ls" },
    };
    translatePiEvent(startEvent, state);

    const endEvent: PiEvent = {
      type: "tool_execution_end",
      tool_call_id: "call-1",
      tool_name: "bash",
      tool_result: {},
    };

    const result = translatePiEvent(endEvent, state);
    expect(result.kind).toBe("emit");
    if (result.kind === "emit") {
      expect(result.results).toHaveLength(2);
      expect(result.results[0].hookEvent).toBe("PreToolUse");
      expect(result.results[1].hookEvent).toBe("PostToolUse");
    }
  });

  it("should accumulate message_update deltas", () => {
    const textEvent: PiEvent = {
      type: "message_update",
      message_update: { type: "text_delta", delta: "Hello " },
    };
    const thinkingEvent: PiEvent = {
      type: "message_update",
      message_update: { type: "thinking_delta", delta: "Thinking..." },
    };

    translatePiEvent(textEvent, state);
    translatePiEvent(thinkingEvent, state);

    expect(state.pendingAssistantText).toBe("Hello ");
    expect(state.pendingAssistantThinking).toBe("Thinking...");
  });

  it("should translate model_select to SessionStart", () => {
    const event: PiEvent = {
      type: "model_select",
      model: "claude-3-opus",
      provider: "anthropic",
    };
    const result = translatePiEvent(event, state);
    expect(result.kind).toBe("emit");
    if (result.kind === "emit") {
      expect(result.results[0].hookEvent).toBe("SessionStart");
      expect((result.results[0].hookData as any).model).toBe("claude-3-opus");
    }
  });

  it("should ignore unknown events", () => {
    const event: PiEvent = { type: "unknown", data: 123 } as PiEvent;
    const result = translatePiEvent(event, state);
    expect(result.kind).toBe("noop");
  });

  it("should create SessionEnd", () => {
    const sessionEnd = createSessionEnd(state);
    expect(sessionEnd.hookEvent).toBe("SessionEnd");
    expect(sessionEnd.hookData.session_id).toBe("test-session");
  });

  it("should create SessionStart", () => {
    state.modelName = "claude-3";
    const sessionStart = createSessionStart(state);
    expect(sessionStart.hookEvent).toBe("SessionStart");
    expect(sessionStart.hookData.model).toBe("claude-3");
  });

  it("tool_execution_update emits ToolPartial and stashes the latest partial", () => {
    // accToolStart registers the running tool; the next _update is matched
    // by toolCallId and surfaces as a ToolPartial event.
    const event = {
      type: "tool_execution_update",
      toolCallId: "call-1",
      toolName: "bash",
      partialResult: "line 1\nline 2\n",
    } as unknown as PiEvent;

    // First arm the accumulator with a running tool.
    state.currentToolUseId = "call-1";

    const result = translatePiEvent(event, state);
    expect(result.kind).toBe("emit");
    if (result.kind === "emit") {
      expect(result.results).toHaveLength(1);
      expect(result.results[0].hookEvent).toBe("ToolPartial");
      const hd = result.results[0].hookData as { tool_use_id?: string; partial?: unknown };
      expect(hd.tool_use_id).toBe("call-1");
      expect(hd.partial).toBe("line 1\nline 2\n");
    }
    // Stash is kept on the accumulator for accToolEnd's fallback.
    expect(state.currentToolPartial).toBe("line 1\nline 2\n");
  });

  it("tool_execution_update for a non-current tool does not stash (out-of-order safety)", () => {
    // If pi sends an _update keyed by an id we're not tracking, the
    // translator still emits ToolPartial (gravity may resolve the tool
    // independently), but does not poison the current accumulator slot.
    state.currentToolUseId = "current";
    state.currentToolPartial = "current partial";

    const event = {
      type: "tool_execution_update",
      toolCallId: "other",
      partialResult: "other partial",
    } as unknown as PiEvent;

    translatePiEvent(event, state);
    expect(state.currentToolPartial).toBe("current partial");
  });

  it("accToolEnd falls back to the stashed partial when toolResult is missing", () => {
    // Simulate: tool_execution_start → 2 _update events → tool_execution_end
    // with no result. The translator must surface the last partial as the
    // effective result in both the hookData and the AccTool record.
    const state2 = createAccState("s", "/tmp", "medium");
    accTurnStart(state2, "t");
    accToolStart(state2, "call-x", "bash", { command: "make build" });
    state2.currentToolPartial = "compiling step 5/10\n";

    const results = accToolEnd(state2, "call-x", "bash", null);
    const post = results.find((r) => r.hookEvent === "PostToolUse");
    expect(post).toBeDefined();
    expect((post!.hookData as { tool_response?: unknown }).tool_response).toBe(
      "compiling step 5/10\n",
    );
  });

  it("accToolEnd prefers an explicit toolResult over the stashed partial", () => {
    const state2 = createAccState("s", "/tmp", "medium");
    accTurnStart(state2, "t");
    accToolStart(state2, "call-x", "bash", { command: "ls" });
    state2.currentToolPartial = "stale partial";

    const results = accToolEnd(state2, "call-x", "bash", { stdout: "final" });
    const post = results.find((r) => r.hookEvent === "PostToolUse");
    expect((post!.hookData as { tool_response?: unknown }).tool_response).toEqual({ stdout: "final" });
  });

  it("accToolStart clears any partial left over from a previous tool", () => {
    const state2 = createAccState("s", "/tmp", "medium");
    accTurnStart(state2, "t");
    accToolStart(state2, "call-a", "bash", {});
    state2.currentToolPartial = "old";
    accToolStart(state2, "call-b", "bash", {});
    expect(state2.currentToolPartial).toBeUndefined();
  });

  it("compaction_end emits Compaction with reason/tokensBefore/summary/aborted on hookData", () => {
    const state2 = createAccState("s", "/tmp", "medium");
    const event = {
      type: "compaction_end",
      reason: "threshold",
      aborted: false,
      result: { tokensBefore: 50000, summary: "Earlier history summarized." },
    } as unknown as PiEvent;
    const result = translatePiEvent(event, state2);
    expect(result.kind).toBe("emit");
    if (result.kind === "emit") {
      expect(result.results).toHaveLength(1);
      expect(result.results[0].hookEvent).toBe("Compaction");
      const hd = result.results[0].hookData as {
        compaction_reason?: string;
        compaction_aborted?: boolean;
        compaction_tokens_before?: number | null;
        compaction_summary?: string | null;
      };
      expect(hd.compaction_reason).toBe("threshold");
      expect(hd.compaction_aborted).toBe(false);
      expect(hd.compaction_tokens_before).toBe(50000);
      expect(hd.compaction_summary).toBe("Earlier history summarized.");
    }
  });

  it("compaction_end with missing fields defaults to unknown/null", () => {
    const state2 = createAccState("s", "/tmp", "medium");
    const event = { type: "compaction_end" } as unknown as PiEvent;
    const result = translatePiEvent(event, state2);
    if (result.kind === "emit") {
      const hd = result.results[0].hookData as {
        compaction_reason?: string;
        compaction_tokens_before?: number | null;
        compaction_summary?: string | null;
      };
      expect(hd.compaction_reason).toBe("unknown");
      expect(hd.compaction_tokens_before).toBeNull();
      expect(hd.compaction_summary).toBeNull();
    }
  });
});

// ── Session module tests ───────────────────────────────────────────

describe("Session module", () => {
  it("should map thinking levels to effort", () => {
    expect(thinkingToEffort("off")).toBe("low");
    expect(thinkingToEffort("minimal")).toBe("low");
    expect(thinkingToEffort("low")).toBe("medium");
    expect(thinkingToEffort("medium")).toBe("medium");
    expect(thinkingToEffort("high")).toBe("high");
    expect(thinkingToEffort("xhigh")).toBe("high"); // capped
  });

  it("should normalize thinking levels", () => {
    expect(normalizeThinkingLevel("high")).toBe("high");
    expect(normalizeThinkingLevel("invalid")).toBe("medium"); // default
  });

  it("should create session metadata", () => {
    const metadata = createSessionMetadata("session-1", "/test", "medium");
    expect(metadata.sessionId).toBe("session-1");
    expect(metadata.cwd).toBe("/test");
    expect(metadata.thinkingLevel).toBe("medium");
    expect(metadata.effortLevel).toBe("medium");
  });

  it("should update model in metadata", () => {
    const metadata = createSessionMetadata("session-1", "/test", "medium");
    const updated = updateModel(metadata, "claude-3-opus", "anthropic");
    expect(updated.modelName).toBe("claude-3-opus");
    expect(updated.sessionId).toBe("session-1"); // unchanged
  });

  it("should update thinking level in metadata", () => {
    const metadata = createSessionMetadata("session-1", "/test", "medium");
    const updated = updateThinkingLevel(metadata, "high");
    expect(updated.thinkingLevel).toBe("high");
    expect(updated.effortLevel).toBe("high");
  });
});

// ── Integration test: full event sequence ─────────────────────────

describe("Full event sequence", () => {
  it("should translate a complete agent session", () => {
    const state = createAccState("test-session", "/test", "medium");
    const allEvents: TranslationResult[] = [];

    // Simulate a simple pi session: user prompt → text → tool → response
    const events: PiEvent[] = [
      // Agent starts (pi 0.74 sends a bare agent_start; prompt arrives next)
      { type: "agent_start" } as unknown as PiEvent,
      {
        type: "message_start",
        message: {
          role: "user",
          content: [{ type: "text", text: "List files in current directory" }],
        },
      } as unknown as PiEvent,
      // Assistant starts a turn
      { type: "turn_start", turn_id: "t1" },
      // Assistant sends text
      {
        type: "message_update",
        message_update: { type: "text_delta", delta: "I'll list the files." },
      },
      // Tool execution starts
      {
        type: "tool_execution_start",
        tool_call_id: "call-1",
        tool_name: "bash",
        tool_input: { command: "ls -la" },
      },
      // Tool execution ends
      {
        type: "tool_execution_end",
        tool_call_id: "call-1",
        tool_name: "bash",
        tool_result: { stdout: "file1.txt\nfile2.txt" },
      },
      // Turn ends
      { type: "turn_end", turn_id: "t1" },
      // Agent ends
      { type: "agent_end", result: { type: "success" } },
    ];

    for (const event of events) {
      const result = translatePiEvent(event, state);
      if (result.kind === "emit") {
        allEvents.push(...result.results);
      }
    }

    // Verify we got expected events. Boundary events are now TurnOpen /
    // TurnClose (per design/pi-adapter.md "the spine"). SessionStart is
    // synthesized outside the translator at subprocess spawn time.
    const hookEvents = allEvents.map(e => e.hookEvent);
    expect(hookEvents).toContain("TurnOpen");
    expect(hookEvents).toContain("UserPromptSubmit");
    expect(hookEvents).toContain("PreToolUse");
    expect(hookEvents).toContain("PostToolUse");
    expect(hookEvents).toContain("TurnClose");
  });

  it("should handle out-of-order: turn_end before tool_execution_end", () => {
    // Bug: the cleanup branch in accTurnEnd pushes a PreToolUse but no PostToolUse.
    // Then tool_execution_end fires, matches currentToolUseId (not reset), emits
    // another PreToolUse + PostToolUse, and adds a second AccTool to turn.tools.
    // Expected: exactly one PreToolUse, exactly one PostToolUse, tool in turn.tools once.
    const state = createAccState("test-session", "/test", "medium");
    const allEvents: TranslationResult[] = [];

    const events: PiEvent[] = [
      { type: "agent_start" } as unknown as PiEvent,
      { type: "message_start", message: { role: "user", content: [{ type: "text", text: "hello" }] } } as unknown as PiEvent,
      { type: "turn_start", turn_id: "t1" },
      {
        type: "tool_execution_start",
        tool_call_id: "call-1",
        tool_name: "bash",
        tool_input: { command: "ls" },
      },
      // Out-of-order: turn_end arrives BEFORE tool_execution_end
      { type: "turn_end", turn_id: "t1" },
      {
        type: "tool_execution_end",
        tool_call_id: "call-1",
        tool_name: "bash",
        tool_result: { stdout: "file.txt" },
      },
      { type: "agent_end", result: { type: "success" } },
    ];

    for (const event of events) {
      const result = translatePiEvent(event, state);
      if (result.kind === "emit") {
        allEvents.push(...result.results);
      }
    }

    const hookEvents = allEvents.map((e) => e.hookEvent);
    const preToolUseCount = hookEvents.filter((e) => e === "PreToolUse").length;
    const postToolUseCount = hookEvents.filter((e) => e === "PostToolUse" || e === "PostToolUseFailure").length;

    // Exactly one PreToolUse and one PostToolUse — no duplicates
    expect(preToolUseCount).toBe(1);
    expect(postToolUseCount).toBe(1);

    // Tool appears exactly once in turn.tools
    const turn = state.turns[0];
    expect(turn?.tools).toHaveLength(1);
    expect(turn?.tools[0].toolName).toBe("bash");
  });

  it("should handle abort: turn_end with no subsequent tool_execution_end", () => {
    // Bug: when pi crashes or is killed mid-tool, turn_end fires but tool_execution_end
    // never arrives. The cleanup branch pushes a bare PreToolUse with no matching PostToolUse.
    // Expected: the tool is either absent from final state, or marked as failed/aborted.
    const state = createAccState("test-session", "/test", "medium");
    const allEvents: TranslationResult[] = [];

    const events: PiEvent[] = [
      { type: "agent_start" } as unknown as PiEvent,
      { type: "message_start", message: { role: "user", content: [{ type: "text", text: "hello" }] } } as unknown as PiEvent,
      { type: "turn_start", turn_id: "t1" },
      {
        type: "tool_execution_start",
        tool_call_id: "call-1",
        tool_name: "bash",
        tool_input: { command: "ls" },
      },
      // No tool_execution_end — pi was killed
      { type: "turn_end", turn_id: "t1" },
      { type: "agent_end", result: { type: "success" } },
    ];

    for (const event of events) {
      const result = translatePiEvent(event, state);
      if (result.kind === "emit") {
        allEvents.push(...result.results);
      }
    }

    // The tool should NOT be left as a "running" orphan — either:
    // (a) it is absent from turn.tools, or
    // (b) it has a failure marker (PostToolUseFailure emitted)
    const hookEvents = allEvents.map((e) => e.hookEvent);
    const preToolUseCount = hookEvents.filter((e) => e === "PreToolUse").length;
    const postToolUseFailureCount = hookEvents.filter((e) => e === "PostToolUseFailure").length;
    const postToolUseCount = hookEvents.filter((e) => e === "PostToolUse").length;

    const turn = state.turns[0];

    // If the tool appears in turn.tools it MUST have a failure marker.
    // A PreToolUse with no matching PostToolUseFailure is a protocol violation.
    if ((turn?.tools?.length ?? 0) > 0) {
      // Tool is present → must have a failure event to balance it
      expect(postToolUseFailureCount).toBe(1);
      // There should be exactly one PreToolUse for this tool
      expect(preToolUseCount).toBe(1);
    } else {
      // Tool is absent — also acceptable (tool ignored on abort)
      // No PreToolUse should be emitted for an aborted tool
      expect(preToolUseCount).toBe(0);
    }

    // No successful PostToolUse for this session (no tool completed)
    expect(postToolUseCount).toBe(0);
  });
});

// ── Issue #4 + #5: pendingToolEvents leak ──────────────────────────────
//
// `tool_execution_end` shifts only the first pending event (leaves PostToolUse
// stuck). `agent_end` calls drainPendingEvents but discards the result. mod.ts
// reads via getPendingEvents (non-destructive) on accumulate, so every
// accumulate event re-emits the entire backlog. The combination corrupts
// emit ordering, drops events, and produces duplicates in any multi-tool turn.

/**
 * Mirrors the event-handler body in `pi-driver/mod.ts:setEventHandler` —
 * i.e. the exact production routing that drives onTranslation. Drives the
 * given pi events through translatePiEvent and emits every TranslationResult
 * the translator returns, in order. No shared-state queue.
 */
function runProductionEventHandler(
  events: PiEvent[],
  state: AccState,
  emit: (r: TranslationResult) => void,
): void {
  for (const evt of events) {
    const result = translatePiEvent(evt, state);
    if (result.kind === "emit") {
      for (const r of result.results) emit(r);
    }
  }
}

describe("issue #4: multi-tool turn — no duplicates, no drops", () => {
  it("a 3-tool turn emits exactly 1 TurnOpen, 1 UserPromptSubmit, 3 PreToolUse, 3 PostToolUse, 1 TurnClose", () => {
    const state = createAccState("test-issue-4", "/test", thinkingToEffort("medium"));
    const emitted: TranslationResult[] = [];

    const events: PiEvent[] = [
      { type: "agent_start" } as unknown as PiEvent,
      { type: "message_start", message: { role: "user", content: [{ type: "text", text: "do three things" }] } } as unknown as PiEvent,
      { type: "turn_start", turn_id: "t1" },
      { type: "tool_execution_start", tool_call_id: "c1", tool_name: "bash", tool_input: { command: "ls" } },
      { type: "tool_execution_end",   tool_call_id: "c1", tool_name: "bash", tool_result: { stdout: "" } },
      { type: "tool_execution_start", tool_call_id: "c2", tool_name: "bash", tool_input: { command: "pwd" } },
      { type: "tool_execution_end",   tool_call_id: "c2", tool_name: "bash", tool_result: { stdout: "/" } },
      { type: "tool_execution_start", tool_call_id: "c3", tool_name: "bash", tool_input: { command: "echo hi" } },
      { type: "tool_execution_end",   tool_call_id: "c3", tool_name: "bash", tool_result: { stdout: "hi" } },
      { type: "turn_end", turn_id: "t1" },
      { type: "agent_end", result: { type: "success" } },
    ];

    runProductionEventHandler(events, state, (r) => emitted.push(r));

    const counts: Record<string, number> = {};
    for (const e of emitted) counts[e.hookEvent] = (counts[e.hookEvent] ?? 0) + 1;

    expect(counts).toEqual({
      TurnOpen: 1,
      UserPromptSubmit: 1,
      PreToolUse: 3,
      PostToolUse: 3,
      TurnClose: 1,
    });

    // Each tool_use_id appears once in PreToolUse and once in PostToolUse
    const preIds = emitted
      .filter((e) => e.hookEvent === "PreToolUse")
      .map((e) => (e.hookData as { tool_use_id: string }).tool_use_id);
    const postIds = emitted
      .filter((e) => e.hookEvent === "PostToolUse")
      .map((e) => (e.hookData as { tool_use_id: string }).tool_use_id);
    expect(preIds.sort()).toEqual(["c1", "c2", "c3"]);
    expect(postIds.sort()).toEqual(["c1", "c2", "c3"]);
  });

  it("PostToolUse for the LAST tool of a turn is not dropped at agent_end", () => {
    const state = createAccState("test-issue-5", "/test", thinkingToEffort("medium"));
    const emitted: TranslationResult[] = [];

    const events: PiEvent[] = [
      { type: "agent_start" } as unknown as PiEvent,
      { type: "message_start", message: { role: "user", content: [{ type: "text", text: "one tool" }] } } as unknown as PiEvent,
      { type: "turn_start", turn_id: "t1" },
      { type: "tool_execution_start", tool_call_id: "only", tool_name: "bash", tool_input: {} },
      { type: "tool_execution_end",   tool_call_id: "only", tool_name: "bash", tool_result: {} },
      { type: "turn_end", turn_id: "t1" },
      { type: "agent_end", result: { type: "success" } },
    ];

    runProductionEventHandler(events, state, (r) => emitted.push(r));

    const postCount = emitted.filter((e) => e.hookEvent === "PostToolUse").length;
    expect(postCount).toBe(1);
  });
});

// ── Regression: thinking / text / result on tool hookData ──────────────
//
// Earlier `accToolStart` flushed pendingAssistantText/Thinking but
// THREW AWAY the result, so every emitted PreToolUse got null
// assistant_text/assistant_thinking and the renderer never had
// thinking or pre-tool text to show. Separately, `accToolEnd` never
// included `tool_response` in the hookData, so the server's
// handlePostToolUse stored undefined as the tool result.
//
// This test pins both fixes in one realistic event flow.
describe("regression: thinking/text/result reach the emitted hookData", () => {
  it("thinking_delta and text_delta before tool_start land on PreToolUse hookData", () => {
    const state = createAccState("test-thinking", "/test", thinkingToEffort("medium"));
    const emitted: TranslationResult[] = [];

    const events: PiEvent[] = [
      { type: "agent_start" } as unknown as PiEvent,
      { type: "message_start", message: { role: "user", content: [{ type: "text", text: "do it" }] } } as unknown as PiEvent,
      { type: "turn_start", turn_id: "t1" },
      // Pi streams thinking + text BEFORE the tool call. assistantMessageEvent
      // is pi 0.74's actual envelope; we also accept the legacy message_update.
      { type: "message_update", assistantMessageEvent: { type: "thinking_delta", delta: "Need to ls first." } } as unknown as PiEvent,
      { type: "message_update", assistantMessageEvent: { type: "text_delta", delta: "Listing files now." } } as unknown as PiEvent,
      { type: "tool_execution_start", tool_call_id: "c1", tool_name: "bash", tool_input: { command: "ls" } },
      { type: "tool_execution_end",   tool_call_id: "c1", tool_name: "bash", tool_result: { stdout: "file.txt" } },
      { type: "turn_end", turn_id: "t1" },
      { type: "agent_end", result: { type: "success" } },
    ];

    for (const evt of events) {
      const r = translatePiEvent(evt, state);
      if (r.kind === "emit") for (const x of r.results) emitted.push(x);
    }

    const pre = emitted.find((e) => e.hookEvent === "PreToolUse");
    expect(pre).toBeDefined();
    const data = pre!.hookData as Record<string, unknown>;
    expect(data.assistant_text).toBe("Listing files now.");
    expect(data.assistant_thinking).toBe("Need to ls first.");

    const post = emitted.find((e) => e.hookEvent === "PostToolUse");
    expect(post).toBeDefined();
    const postData = post!.hookData as Record<string, unknown>;
    expect(postData.tool_response).toEqual({ stdout: "file.txt" });
  });

  it("between-tool text/thinking attach to the NEXT tool, not the previous one", () => {
    // Three tools in one turn with text between them. Each tool should
    // get exactly the text/thinking that came BEFORE its tool_execution_start.
    const state = createAccState("test-between", "/test", thinkingToEffort("medium"));
    const emitted: TranslationResult[] = [];

    const events: PiEvent[] = [
      { type: "agent_start" } as unknown as PiEvent,
      { type: "message_start", message: { role: "user", content: [{ type: "text", text: "go" }] } } as unknown as PiEvent,
      { type: "turn_start", turn_id: "t1" },
      { type: "message_update", assistantMessageEvent: { type: "text_delta", delta: "First, ls." } } as unknown as PiEvent,
      { type: "tool_execution_start", tool_call_id: "c1", tool_name: "bash", tool_input: { command: "ls" } },
      { type: "tool_execution_end",   tool_call_id: "c1", tool_name: "bash", tool_result: { stdout: "ok1" } },
      { type: "message_update", assistantMessageEvent: { type: "text_delta", delta: "Now pwd." } } as unknown as PiEvent,
      { type: "tool_execution_start", tool_call_id: "c2", tool_name: "bash", tool_input: { command: "pwd" } },
      { type: "tool_execution_end",   tool_call_id: "c2", tool_name: "bash", tool_result: { stdout: "/" } },
      { type: "message_update", assistantMessageEvent: { type: "text_delta", delta: "Finally echo." } } as unknown as PiEvent,
      { type: "tool_execution_start", tool_call_id: "c3", tool_name: "bash", tool_input: { command: "echo hi" } },
      { type: "tool_execution_end",   tool_call_id: "c3", tool_name: "bash", tool_result: { stdout: "hi" } },
      { type: "turn_end", turn_id: "t1" },
      { type: "agent_end", result: { type: "success" } },
    ];

    for (const evt of events) {
      const r = translatePiEvent(evt, state);
      if (r.kind === "emit") for (const x of r.results) emitted.push(x);
    }

    const pres = emitted.filter((e) => e.hookEvent === "PreToolUse");
    expect(pres).toHaveLength(3);
    const text = (i: number) => (pres[i].hookData as Record<string, unknown>).assistant_text;
    expect(text(0)).toBe("First, ls.");
    expect(text(1)).toBe("Now pwd.");
    expect(text(2)).toBe("Finally echo.");
  });
});
import { normalizePiCommands } from "../src/pi-driver/spawn.js";

describe("normalizePiCommands", () => {
  it("flattens the shipping-build sourceInfo shape", () => {
    const out = normalizePiCommands([
      {
        name: "mcp",
        description: "Show MCP server status",
        source: "extension",
        sourceInfo: {
          path: "/n/pi-mcp-adapter/index.ts",
          scope: "user",
          source: "npm:pi-mcp-adapter",
          origin: "package",
        },
      },
    ]);
    expect(out).toEqual([
      {
        name: "mcp",
        description: "Show MCP server status",
        source: "extension",
        location: "user",
        path: "/n/pi-mcp-adapter/index.ts",
      },
    ]);
  });

  it("accepts the documented flat shape unchanged", () => {
    const out = normalizePiCommands([
      { name: "review", description: "Review", source: "prompt", location: "project", path: "/p/.pi/prompts/review.md" },
    ]);
    expect(out[0]).toEqual({
      name: "review",
      description: "Review",
      source: "prompt",
      location: "project",
      path: "/p/.pi/prompts/review.md",
    });
  });

  it("omits absent optional fields and defaults source", () => {
    const out = normalizePiCommands([{ name: "bare" }]);
    expect(out[0]).toEqual({ name: "bare", source: "extension" });
    expect("path" in out[0]).toBe(false);
    expect("location" in out[0]).toBe(false);
    expect("description" in out[0]).toBe(false);
  });

  it("prefers flat path over sourceInfo.path when both present", () => {
    const out = normalizePiCommands([
      { name: "x", source: "skill", path: "/flat", sourceInfo: { path: "/nested" } },
    ]);
    expect(out[0].path).toBe("/flat");
  });

  it("returns [] for non-array input", () => {
    expect(normalizePiCommands(undefined)).toEqual([]);
    expect(normalizePiCommands(null)).toEqual([]);
    expect(normalizePiCommands("nope")).toEqual([]);
  });
});
