import { describe, it, expect } from "vitest";
import {
  createSession,
  openTurn,
  attachPrompt,
  closeTurn,
  addPrompt,
  finalizeLastPrompt,
  addTool,
  updateToolPartial,
  completeTool,
} from "../src/state/session.js";
import type { PromptEntry, Tool } from "@gravity/shared";

const makePrompt = (text: string): PromptEntry => ({
  type: "user",
  text,
  submitted: Date.now(),
  elapsed: null,
  toolUseId: null,
  answer: null,
});

describe("openTurn", () => {
  it("creates an empty turn and emits add_turn", () => {
    const s = createSession("s1", "/tmp");
    // createSession pre-allocates turn 0; openTurn appends turn 1.
    const patches = openTurn(s);

    expect(s.turns.length).toBe(2);
    expect(s.currentTurn).toBe(1);
    expect(s.turns[1].turnNumber).toBe(1);
    expect(s.turns[1].prompt).toBeNull();
    expect(s.turns[1].frozen).toBe(false);

    const ops = patches.map((p) => p.op);
    expect(ops).toContain("add_turn");
    // No freeze for turn 0 → turn 1 only if turn 0 is unfrozen. Turn 0 is
    // unfrozen by default, so a freeze_turn IS expected here.
    expect(ops).toContain("freeze_turn");
  });

  it("freezes the previous turn when it is still open", () => {
    const s = createSession("s2", "/tmp");
    openTurn(s); // turn 1, unfrozen
    expect(s.turns[1].frozen).toBe(false);

    const patches = openTurn(s); // turn 2
    expect(s.turns[1].frozen).toBe(true);
    const freezes = patches.filter((p) => p.op === "freeze_turn");
    expect(freezes).toHaveLength(1);
    // @ts-expect-error narrowing through union
    expect(freezes[0].turnNumber).toBe(1);
  });

  it("does not double-freeze an already-frozen previous turn (restart guard)", () => {
    const s = createSession("s3", "/tmp");
    openTurn(s);
    // Simulate a Stop / closeTurn that froze turn 1.
    closeTurn(s, { stopText: "done" });
    expect(s.turns[1].frozen).toBe(true);

    // Now a respawned pi process emits agent_start → openTurn. The previous
    // turn is already frozen — no second freeze_turn patch should fire.
    const patches = openTurn(s);
    const freezes = patches.filter((p) => p.op === "freeze_turn");
    expect(freezes).toHaveLength(0);
    expect(s.turns[2].turnNumber).toBe(2);
  });

  it("defensive freeze fires when a pi process dies mid-turn and respawns", () => {
    // Restart hazard scenario: pi crashes between agent_start and agent_end.
    // No closeTurn ever ran, so turn 1 is unfrozen. Next pi spawn fires
    // agent_start → openTurn; the defensive freeze prevents the new turn
    // from stacking on top of the stale unfrozen one.
    const s = createSession("s4", "/tmp");
    openTurn(s); // turn 1
    attachPrompt(s, makePrompt("prompt before crash"));
    // (no closeTurn — process died here)
    expect(s.turns[1].frozen).toBe(false);

    const patches = openTurn(s); // simulated respawn → first agent_start
    expect(s.turns[1].frozen).toBe(true);
    const freezes = patches.filter((p) => p.op === "freeze_turn");
    expect(freezes).toHaveLength(1);
    expect(s.turns[2].turnNumber).toBe(2);
  });
});

describe("attachPrompt", () => {
  it("attaches prompt to the current turn and emits add_prompt", () => {
    const s = createSession("s1", "/tmp");
    openTurn(s);
    const patches = attachPrompt(s, makePrompt("hello"));
    expect(s.turns[1].prompt?.text).toBe("hello");
    expect(patches.map((p) => p.op)).toContain("add_prompt");
  });

  it("is a no-op if the current turn already has a prompt", () => {
    const s = createSession("s2", "/tmp");
    openTurn(s);
    attachPrompt(s, makePrompt("first"));
    const patches = attachPrompt(s, makePrompt("second"));
    expect(patches).toEqual([]);
    expect(s.turns[1].prompt?.text).toBe("first");
  });

  it("is a no-op when called with no open turn (only turn 0 exists)", () => {
    // Per the spine contract, attachPrompt does not create turns. Turn 0
    // is the pre-prompt activity bucket; attaching to it is legal at the
    // primitive level but discouraged by callers. Here we just verify the
    // primitive's contract (does not throw, mutates current turn).
    const s = createSession("s3", "/tmp");
    const patches = attachPrompt(s, makePrompt("strays"));
    expect(s.turns[0].prompt?.text).toBe("strays");
    expect(patches.map((p) => p.op)).toContain("add_prompt");
  });
});

describe("closeTurn", () => {
  it("sets stop text, freezes turn, emits set_turn_stop + freeze_turn", () => {
    const s = createSession("s1", "/tmp");
    openTurn(s);
    attachPrompt(s, makePrompt("p"));
    const patches = closeTurn(s, { stopText: "done", stopThinking: "tw" });

    expect(s.turns[1].stopText).toBe("done");
    expect(s.turns[1].stopThinking).toBe("tw");
    expect(s.turns[1].frozen).toBe(true);

    const ops = patches.map((p) => p.op);
    expect(ops).toContain("set_turn_stop");
    expect(ops).toContain("freeze_turn");
  });

  it("records stopReason on the turn and the set_turn_stop patch", () => {
    const s = createSession("s-stop", "/tmp");
    openTurn(s);
    const patches = closeTurn(s, { stopReason: "length" });
    expect(s.turns[1].stopReason).toBe("length");
    const stop = patches.find((p) => p.op === "set_turn_stop");
    // @ts-expect-error narrowing through patch union
    expect(stop?.stopReason).toBe("length");
  });

  it("emits set_turn_stop when only stopReason is provided (no text/thinking)", () => {
    const s = createSession("s-stop-only", "/tmp");
    openTurn(s);
    const patches = closeTurn(s, { stopReason: "aborted" });
    expect(patches.some((p) => p.op === "set_turn_stop")).toBe(true);
  });

  it("records token usage when provided and emits set_turn_tokens", () => {
    const s = createSession("s2", "/tmp");
    openTurn(s);
    const patches = closeTurn(s, { tokenIn: 100, tokenOut: 50 });
    expect(s.turns[1].tokenIn).toBe(100);
    expect(s.turns[1].tokenOut).toBe(50);
    const tokens = patches.filter((p) => p.op === "set_turn_tokens");
    expect(tokens).toHaveLength(1);
    // @ts-expect-error narrowing
    expect(tokens[0].tokenIn).toBe(100);
    // @ts-expect-error narrowing
    expect(tokens[0].tokenOut).toBe(50);
  });

  it("omits set_turn_tokens when no tokens provided", () => {
    const s = createSession("s3", "/tmp");
    openTurn(s);
    const patches = closeTurn(s, { stopText: "x" });
    expect(patches.some((p) => p.op === "set_turn_tokens")).toBe(false);
  });

  it("a second closeTurn is idempotent (no second freeze_turn)", () => {
    const s = createSession("s4", "/tmp");
    openTurn(s);
    closeTurn(s, { stopText: "first" });
    const patches = closeTurn(s, { stopText: "second" });
    // turn is already frozen → no second freeze_turn
    expect(patches.some((p) => p.op === "freeze_turn")).toBe(false);
    // stop_text is already set → closeTurn does not overwrite
    expect(s.turns[1].stopText).toBe("first");
  });

  it("returns [] when there is no current turn", () => {
    // Cannot construct a Session with empty turns via the factory; verified
    // indirectly by closeTurn early-return contract.
    const s = createSession("s5", "/tmp");
    s.turns = [];
    expect(closeTurn(s, { stopText: "x" })).toEqual([]);
  });
});

describe("addPrompt = openTurn + attachPrompt", () => {
  it("creates a new turn AND attaches the prompt (Claude Code path)", () => {
    const s = createSession("s1", "/tmp");
    const patches = addPrompt(s, makePrompt("hello"));
    expect(s.turns[1].prompt?.text).toBe("hello");
    const ops = patches.map((p) => p.op);
    expect(ops).toContain("add_turn");
    expect(ops).toContain("add_prompt");
  });
});

describe("updateToolPartial", () => {
  const mkTool = (toolUseId: string): Tool => ({
    toolUseId,
    name: "bash",
    input: { command: "make build" },
    status: "running",
    result: null,
    partial: null,
    timestamp: Date.now(),
    duration: null,
    turn: 1,
    assistantText: null,
    assistantThinking: null,
    postText: null,
    postThinking: null,
    parentAgentId: null,
    ambiguous: false,
    candidateAgentIds: null,
    agentId: null,
  });

  it("writes partial to tool.partial and emits update_tool_partial", () => {
    const s = createSession("s1", "/tmp");
    openTurn(s);
    addTool(s, mkTool("t1"));
    const patches = updateToolPartial(s, "t1", "line 1\nline 2\n");
    const t = s.toolIndex["t1"];
    expect(t).toBeDefined();
    // tool.partial is on the Tool object itself (located via toolIndex)
    const turn = s.turns[1];
    const stored = turn.steps[turn.steps.length - 1].tools.find((x) => x.toolUseId === "t1");
    expect(stored?.partial).toBe("line 1\nline 2\n");
    expect(patches).toHaveLength(1);
    expect(patches[0].op).toBe("update_tool_partial");
  });

  it("replaces (does not append) on each update — cumulative-snapshot model", () => {
    const s = createSession("s2", "/tmp");
    openTurn(s);
    addTool(s, mkTool("t1"));
    updateToolPartial(s, "t1", "a");
    updateToolPartial(s, "t1", "ab");
    updateToolPartial(s, "t1", "abc");
    const turn = s.turns[1];
    const stored = turn.steps[turn.steps.length - 1].tools.find((x) => x.toolUseId === "t1");
    expect(stored?.partial).toBe("abc");
  });

  it("is a no-op once the tool has completed (late _update arrives after _end)", () => {
    const s = createSession("s3", "/tmp");
    openTurn(s);
    addTool(s, mkTool("t1"));
    completeTool(s, "t1", { stdout: "final" }, "done");
    const patches = updateToolPartial(s, "t1", "stale");
    expect(patches).toEqual([]);
    const turn = s.turns[1];
    const stored = turn.steps[turn.steps.length - 1].tools.find((x) => x.toolUseId === "t1");
    expect(stored?.partial).toBeNull(); // partial never set; result holds the final
    expect(stored?.result).toEqual({ stdout: "final" });
  });

  it("is a no-op for unknown tool id", () => {
    const s = createSession("s4", "/tmp");
    const patches = updateToolPartial(s, "missing", "x");
    expect(patches).toEqual([]);
  });
});

describe("finalizeLastPrompt = closeTurn (freeze-on-Stop)", () => {
  it("Stop now freezes the current turn", () => {
    const s = createSession("s1", "/tmp");
    addPrompt(s, makePrompt("p"));
    expect(s.turns[1].frozen).toBe(false);
    finalizeLastPrompt(s, "done", "tw");
    expect(s.turns[1].frozen).toBe(true);
    expect(s.turns[1].stopText).toBe("done");
    expect(s.turns[1].stopThinking).toBe("tw");
  });
});
