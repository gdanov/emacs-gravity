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
  addCompaction,
  updateMeta,
} from "../src/state/session.js";
import { makeSessionStore, type SessionStoreService } from "../src/services/session-store.js";
import type { CompactionMarker, PromptEntry, Session, Tool } from "@gravity/shared";

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

describe("addCompaction", () => {
  const mkMarker = (overrides: Partial<CompactionMarker> = {}): CompactionMarker => ({
    reason: "threshold",
    turnNumber: 1,
    timestamp: Date.now(),
    tokensBefore: 50000,
    summary: "Earlier conversation summarized.",
    aborted: false,
    ...overrides,
  });

  it("appends to session.compactions and emits add_compaction patch", () => {
    const s = createSession("s1", "/tmp");
    const m = mkMarker();
    const patches = addCompaction(s, m);
    expect(s.compactions).toHaveLength(1);
    expect(s.compactions[0]).toEqual(m);
    expect(patches).toHaveLength(1);
    expect(patches[0].op).toBe("add_compaction");
    // @ts-expect-error narrowing through patch union
    expect(patches[0].marker).toEqual(m);
  });

  it("is append-only — multiple compactions all retained in order", () => {
    const s = createSession("s2", "/tmp");
    addCompaction(s, mkMarker({ reason: "threshold", tokensBefore: 100 }));
    addCompaction(s, mkMarker({ reason: "manual", tokensBefore: 200 }));
    addCompaction(s, mkMarker({ reason: "overflow", tokensBefore: 300 }));
    expect(s.compactions.map((m) => m.reason)).toEqual([
      "threshold",
      "manual",
      "overflow",
    ]);
  });

  it("records aborted compactions with aborted=true and summary=null", () => {
    const s = createSession("s3", "/tmp");
    addCompaction(s, mkMarker({ aborted: true, summary: null }));
    expect(s.compactions[0].aborted).toBe(true);
    expect(s.compactions[0].summary).toBeNull();
  });

  it("createSession initialises compactions as an empty array", () => {
    const s = createSession("s4", "/tmp");
    expect(s.compactions).toEqual([]);
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

describe("createSession: attribution defaults", () => {
  it("defaults role to \"worker\" when source is \"pi\"", () => {
    const s = createSession("s-pi", "/tmp/proj-a", "pi");
    expect(s.role).toBe("worker");
    expect(s.source).toBe("pi");
    // readOnly defaults to false on creation regardless of source — the
    // ingest path flips it to true later via updateMeta, not here.
    expect(s.readOnly).toBe(false);
  });

  it("defaults role to \"interactive\" when no source is given (Claude Code path)", () => {
    const s = createSession("s-cc", "/tmp/proj-a");
    expect(s.role).toBe("interactive");
    expect(s.source).toBeNull();
    expect(s.readOnly).toBe(false);
  });

  it("defaults role to \"interactive\" for non-pi sources", () => {
    const s = createSession("s-other", "/tmp/proj-a", "opencode");
    expect(s.role).toBe("interactive");
    expect(s.source).toBe("opencode");
  });

  it("populates repoKey/repoRoot/worktree from the cwd via deriveRepoAttribution", () => {
    // /tmp is not a git repo on most systems; the derivation must fall
    // back to cwd for all three fields without throwing.
    const s = createSession("s-attr", "/tmp/some-non-git-dir", "pi");
    expect(s.repoKey).toBe("/tmp/some-non-git-dir");
    expect(s.repoRoot).toBe("/tmp/some-non-git-dir");
    expect(s.worktree).toBe("/tmp/some-non-git-dir");
  });
});

describe("updateMeta: attribution fields thread through", () => {
  it("sets role/repoKey/repoRoot/worktree/readOnly when provided", () => {
    const s = createSession("s-meta", "/tmp", "pi");
    const patches = updateMeta(s, {
      role: "coordinator",
      repoKey: "/abs/.git",
      repoRoot: "/abs",
      worktree: "/abs/worktree",
      readOnly: true,
    });
    expect(s.role).toBe("coordinator");
    expect(s.repoKey).toBe("/abs/.git");
    expect(s.repoRoot).toBe("/abs");
    expect(s.worktree).toBe("/abs/worktree");
    expect(s.readOnly).toBe(true);
    // The returned patch carries every provided field so terminals can
    // mirror the update.
    const setMeta = patches.find((p) => p.op === "set_meta");
    expect(setMeta).toBeDefined();
    // @ts-expect-error narrowing through patch union
    expect(setMeta.role).toBe("coordinator");
    // @ts-expect-error narrowing through patch union
    expect(setMeta.repoKey).toBe("/abs/.git");
    // @ts-expect-error narrowing through patch union
    expect(setMeta.readOnly).toBe(true);
  });

  it("is a no-op on attribution fields when none are provided", () => {
    const s = createSession("s-noop", "/tmp", "pi");
    const initialRole = s.role;
    const initialRepoKey = s.repoKey;
    const patches = updateMeta(s, { displayName: "hello" });
    expect(s.role).toBe(initialRole);
    expect(s.repoKey).toBe(initialRepoKey);
    expect(s.displayName).toBe("hello");
    expect(patches).toHaveLength(1);
    // @ts-expect-error narrowing
    expect(patches[0].displayName).toBe("hello");
  });
});

describe("getProjectSummaries: grouping by repoKey", () => {
  // Construct sessions directly with repoKey set — no real git call is
  // needed for the grouping test, and createSession's git side-effect on
  // /tmp is the no-git fallback anyway.
  const makeSessionWithRepo = (overrides: Partial<Session> & { sessionId: string }): Session => ({
    sessionId: overrides.sessionId,
    cwd: overrides.cwd ?? "/tmp",
    project: overrides.project ?? "tmp",
    status: overrides.status ?? "active",
    claudeStatus: overrides.claudeStatus ?? "idle",
    slug: overrides.slug ?? null,
    displayName: overrides.displayName ?? null,
    branch: overrides.branch ?? null,
    pid: overrides.pid ?? null,
    modelName: overrides.modelName ?? null,
    tmuxSession: overrides.tmuxSession ?? null,
    source: overrides.source ?? "claude-code",
    repoKey: overrides.repoKey ?? null,
    repoRoot: overrides.repoRoot ?? null,
    worktree: overrides.worktree ?? null,
    role: overrides.role ?? "interactive",
    readOnly: overrides.readOnly ?? false,
    startTime: overrides.startTime ?? Date.now(),
    lastEventTime: overrides.lastEventTime ?? Date.now(),
    tokenUsage: overrides.tokenUsage ?? null,
    cost: overrides.cost ?? null,
    contextUsage: overrides.contextUsage ?? null,
    piSessionFile: overrides.piSessionFile ?? null,
    plan: overrides.plan ?? null,
    streamingText: overrides.streamingText ?? null,
    permissionMode: overrides.permissionMode ?? null,
    turns: overrides.turns ?? [],
    currentTurn: overrides.currentTurn ?? 0,
    toolIndex: overrides.toolIndex ?? {},
    agentIndex: overrides.agentIndex ?? {},
    tasks: overrides.tasks ?? {},
    files: overrides.files ?? {},
    compactions: overrides.compactions ?? [],
    totalToolCount: overrides.totalToolCount ?? 0,
    piCommands: overrides.piCommands ?? null,
    piModels: overrides.piModels ?? null,
  });

  it("groups two sessions sharing the same repoKey under one ProjectSummary", () => {
    const store: SessionStoreService = makeSessionStore();
    const sharedRepoKey = "/abs/path/to/.git";
    store.set("a", makeSessionWithRepo({
      sessionId: "a",
      cwd: "/abs/path/to",
      repoKey: sharedRepoKey,
      repoRoot: "/abs/path/to",
    }));
    store.set("b", makeSessionWithRepo({
      sessionId: "b",
      cwd: "/abs/path/to/.worktrees/feature",
      repoKey: sharedRepoKey,
      repoRoot: "/abs/path/to",
      worktree: "/abs/path/to/.worktrees/feature",
    }));

    const summaries = store.getProjectSummaries();
    expect(summaries).toHaveLength(1);
    expect(summaries[0].repoKey).toBe(sharedRepoKey);
    expect(summaries[0].sessions.map((s) => s.sessionId).sort()).toEqual(["a", "b"]);
    // Project display name comes from the first session's repoRoot basename.
    expect(summaries[0].project).toBe("to");
  });

  it("keeps sessions with distinct repoKeys in separate ProjectSummary entries", () => {
    const store: SessionStoreService = makeSessionStore();
    store.set("a", makeSessionWithRepo({
      sessionId: "a",
      repoKey: "/repo-a/.git",
      repoRoot: "/repo-a",
    }));
    store.set("b", makeSessionWithRepo({
      sessionId: "b",
      repoKey: "/repo-b/.git",
      repoRoot: "/repo-b",
    }));

    const summaries = store.getProjectSummaries();
    expect(summaries).toHaveLength(2);
    const keys = summaries.map((s) => s.repoKey).sort();
    expect(keys).toEqual(["/repo-a/.git", "/repo-b/.git"]);
  });

  it("groups a defensive-null-repoKey session by its project field", () => {
    const store: SessionStoreService = makeSessionStore();
    // Simulate a legacy session created before repoKey was wired in:
    // repoKey is null but the existing `project` field is set.
    store.set("a", makeSessionWithRepo({
      sessionId: "a",
      project: "legacy-name",
      repoKey: null,
      repoRoot: null,
    }));

    const summaries = store.getProjectSummaries();
    expect(summaries).toHaveLength(1);
    // The grouping key falls back to the project name; the repoKey field
    // on the summary carries that fallback verbatim.
    expect(summaries[0].repoKey).toBe("legacy-name");
    expect(summaries[0].project).toBe("legacy-name");
  });
});
