// gateway-client-logic.test.ts — Unit tests for the pure DOM-free
// helpers in `packages/gravity-server/src/gateway/client/logic.ts`.
//
// Pure function reducer / classifier / grouper. No WebSocket, no DOM,
// no jsdom — runs as plain vitest under Node. The DOM-manipulating
// renderer in `client/index.ts` is not exercised here per the
// "browser-only ACs covered at protocol level" deviation recorded in
// the plan.

import { describe, it, expect } from "vitest";
import {
  applyPatch,
  classifyInboxItem,
  groupedFleet,
  shouldSyncPoll,
} from "../src/gateway/client/logic.js";
import type {
  Agent,
  InboxItem,
  InboxItemType,
  Patch,
  ProjectSummary,
  Session,
  SessionRole,
  SessionSummary,
  StepNode,
  TokenUsage,
  Tool,
  ToolLocation,
  TurnNode,
} from "@gravity/shared";

const BASE_TS = 1_700_000_000_000;

function makeStep(text: string | null = null, tools: Tool[] = []): StepNode {
  return { thinking: null, text, tools };
}

function makeTurn(turnNumber: number, steps: StepNode[] = []): TurnNode {
  return {
    turnNumber,
    prompt: null,
    steps,
    agents: [],
    tasks: [],
    toolCount: 0,
    agentCount: 0,
    frozen: false,
    stopText: null,
    stopThinking: null,
    tokenIn: null,
    tokenOut: null,
    stopReason: null,
    editedFiles: [],
  };
}

function makeTool(overrides: Partial<Tool> = {}): Tool {
  return {
    toolUseId: "tu_default",
    name: "Bash",
    input: {},
    status: "running",
    result: null,
    partial: null,
    timestamp: BASE_TS,
    duration: null,
    turn: 0,
    assistantText: null,
    assistantThinking: null,
    postText: null,
    postThinking: null,
    parentAgentId: null,
    ambiguous: false,
    candidateAgentIds: null,
    agentId: null,
    ...overrides,
  };
}

function makeToolLocation(turnNumber: number, stepIndex: number, toolIndex: number): ToolLocation {
  return { turnNumber, stepIndex, toolIndex, agentId: null };
}

function makeAgent(agentId: string): Agent {
  return {
    agentId,
    type: "task",
    status: "running",
    steps: [],
    toolCount: 0,
    stopText: null,
    stopThinking: null,
    duration: null,
    timestamp: BASE_TS,
    transcriptPath: null,
    taskToolUseId: null,
  };
}

function makeSession(overrides: Partial<Session> = {}): Session {
  const base: Session = {
    sessionId: "s1",
    cwd: "/tmp/gravity-test",
    project: "gravity-test",
    status: "active",
    claudeStatus: "idle",
    slug: null,
    displayName: null,
    branch: null,
    pid: null,
    modelName: null,
    tmuxSession: null,
    source: null,
    repoKey: null,
    repoRoot: null,
    worktree: null,
    role: null,
    readOnly: false,
    startTime: BASE_TS,
    lastEventTime: BASE_TS,
    tokenUsage: null,
    cost: null,
    contextUsage: null,
    piSessionFile: null,
    plan: null,
    streamingText: null,
    permissionMode: null,
    turns: [makeTurn(0)],
    currentTurn: 0,
    toolIndex: {},
    agentIndex: {},
    tasks: {},
    files: {},
    compactions: [],
    totalToolCount: 0,
    piCommands: null,
    piModels: null,
  };
  return { ...base, ...overrides };
}

function makeInboxItem(overrides: Partial<InboxItem> = {}): InboxItem {
  return {
    id: 1,
    type: "permission",
    sessionId: "s1",
    project: "gravity-test",
    label: "Test item",
    timestamp: BASE_TS,
    summary: "",
    data: {},
    actionable: true,
    ...overrides,
  };
}

function makeTokenUsage(): TokenUsage {
  return { input_tokens: 1, output_tokens: 2, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 };
}

function makeRole(role: SessionRole): SessionRole {
  return role;
}

function makeProjectSummary(overrides: Partial<ProjectSummary>): ProjectSummary {
  const sessions: SessionSummary[] = overrides.sessions ?? [
    {
      sessionId: "s1",
      slug: null,
      displayName: null,
      status: "active",
      claudeStatus: "idle",
      toolCount: 0,
      lastEventTime: BASE_TS,
      latestMessage: null,
      latestUserPrompt: null,
      role: null,
      branch: null,
      worktree: null,
      turnCount: 0,
      currentTool: null,
      cost: null,
    },
  ];
  return {
    project: "gravity-test",
    repoKey: "/tmp/gravity-test",
    sessions,
    ...overrides,
  };
}

describe("shouldSyncPoll", () => {
  it("returns true for state-changed", () => {
    expect(shouldSyncPoll({ type: "state-changed" })).toBe(true);
  });

  it("returns false for any non-signal message", () => {
    for (const type of ["overview.snapshot", "inbox.snapshot", "session.snapshot", "session-patches", "inbox-items", "overview-data", "notice"]) {
      expect(shouldSyncPoll({ type })).toBe(false);
    }
  });
});

describe("classifyInboxItem", () => {
  const TYPES: InboxItemType[] = ["permission", "question", "plan-review", "idle", "attention"];

  for (const type of TYPES) {
    it(`classifies type=${type} actionable=true as "actionable" except for "attention"`, () => {
      const item = makeInboxItem({ type, actionable: true });
      const expected = type === "attention" ? "attention" : "actionable";
      expect(classifyInboxItem(item)).toBe(expected);
    });

    it(`classifies type=${type} actionable=false as "view-only" except for "attention"`, () => {
      const item = makeInboxItem({ type, actionable: false });
      const expected = type === "attention" ? "attention" : "view-only";
      expect(classifyInboxItem(item)).toBe(expected);
    });
  }

  it("treats the dedicated attention type as attention regardless of actionable", () => {
    expect(classifyInboxItem(makeInboxItem({ type: "attention", actionable: false }))).toBe("attention");
    expect(classifyInboxItem(makeInboxItem({ type: "attention", actionable: true }))).toBe("attention");
  });

  it("treats a non-attention, non-actionable item as view-only", () => {
    expect(classifyInboxItem(makeInboxItem({ type: "idle", actionable: false }))).toBe("view-only");
    expect(classifyInboxItem(makeInboxItem({ type: "permission", actionable: false }))).toBe("view-only");
    expect(classifyInboxItem(makeInboxItem({ type: "question", actionable: false }))).toBe("view-only");
    expect(classifyInboxItem(makeInboxItem({ type: "plan-review", actionable: false }))).toBe("view-only");
  });

  it("treats a non-attention, actionable item as actionable", () => {
    expect(classifyInboxItem(makeInboxItem({ type: "permission", actionable: true }))).toBe("actionable");
    expect(classifyInboxItem(makeInboxItem({ type: "question", actionable: true }))).toBe("actionable");
    expect(classifyInboxItem(makeInboxItem({ type: "plan-review", actionable: true }))).toBe("actionable");
    expect(classifyInboxItem(makeInboxItem({ type: "idle", actionable: true }))).toBe("actionable");
  });
});

describe("groupedFleet", () => {
  it("returns the input array unchanged (identity / documented seam)", () => {
    const projects = [makeProjectSummary({})];
    expect(groupedFleet(projects)).toBe(projects);
  });

  it("preserves the order and contents of the input projects array", () => {
    const a = makeProjectSummary({ project: "alpha", repoKey: "/r/alpha" });
    const b = makeProjectSummary({ project: "beta", repoKey: "/r/beta" });
    const projects = [a, b];
    const grouped = groupedFleet(projects);
    expect(grouped).toHaveLength(2);
    expect(grouped[0].project).toBe("alpha");
    expect(grouped[1].project).toBe("beta");
    expect(grouped[0].sessions).toBe(a.sessions);
  });

  it("returns the same reference for an empty array", () => {
    const empty: ProjectSummary[] = [];
    expect(groupedFleet(empty)).toBe(empty);
  });
});

describe("applyPatch — add_turn", () => {
  it("appends the supplied turn and updates currentTurn", () => {
    const session = makeSession();
    const patch: Patch = {
      op: "add_turn",
      turn: makeTurn(1, [makeStep("hi")]),
    };
    const next = applyPatch(session, patch);
    expect(next).not.toBe(session);
    expect(next.turns).toHaveLength(2);
    expect(next.turns[1].turnNumber).toBe(1);
    expect(next.turns[1].steps[0].text).toBe("hi");
    expect(next.currentTurn).toBe(1);
  });

  it("is a no-op when the turn number already exists (dedup, return input)", () => {
    const session = makeSession({
      turns: [makeTurn(0), makeTurn(1, [makeStep("existing")])],
    });
    const patch: Patch = {
      op: "add_turn",
      turn: makeTurn(1, [makeStep("duplicate")]),
    };
    const next = applyPatch(session, patch);
    expect(next).toBe(session);
    expect(next.turns[1].steps[0].text).toBe("existing");
  });
});

describe("applyPatch — add_tool", () => {
  it("appends to the addressed step and indexes it", () => {
    const session = makeSession({
      turns: [makeTurn(0), makeTurn(1, [makeStep(), makeStep()])],
      currentTurn: 1,
    });
    const tool = makeTool({ toolUseId: "tu_1", name: "Read" });
    const patch: Patch = {
      op: "add_tool",
      turnNumber: 1,
      stepIndex: 0,
      tool,
    };
    const next = applyPatch(session, patch);
    expect(next).not.toBe(session);
    expect(next.toolIndex["tu_1"]).toEqual({
      turnNumber: 1,
      stepIndex: 0,
      toolIndex: 0,
      agentId: null,
    });
    const turn = next.turns.find((t) => t.turnNumber === 1)!;
    expect(turn.steps[0].tools).toHaveLength(1);
    expect(turn.steps[0].tools[0].name).toBe("Read");
    expect(turn.toolCount).toBe(1);
    expect(next.totalToolCount).toBe(1);
  });

  it("is a no-op when the toolUseId is already indexed (dedup, return input)", () => {
    const existingLoc: ToolLocation = makeToolLocation(1, 0, 0);
    const session = makeSession({
      turns: [makeTurn(0), makeTurn(1, [makeStep("s0")])],
      currentTurn: 1,
      toolIndex: { tu_1: existingLoc },
    });
    const patch: Patch = {
      op: "add_tool",
      turnNumber: 1,
      stepIndex: 0,
      tool: makeTool({ toolUseId: "tu_1", name: "Read" }),
    };
    expect(applyPatch(session, patch)).toBe(session);
  });
});

describe("applyPatch — complete_tool", () => {
  it("marks the indexed tool done and records result", () => {
    const tool = makeTool({ toolUseId: "tu_x", name: "Bash", status: "running" });
    const session = makeSession({
      turns: [makeTurn(0), makeTurn(1, [makeStep("s", [tool])])],
      currentTurn: 1,
      toolIndex: { tu_x: makeToolLocation(1, 0, 0) },
    });
    const patch: Patch = {
      op: "complete_tool",
      toolUseId: "tu_x",
      result: { stdout: "ok" },
      status: "done",
      duration: 1.25,
    };
    const next = applyPatch(session, patch);
    expect(next).not.toBe(session);
    const turn = next.turns.find((t) => t.turnNumber === 1)!;
    const t = turn.steps[0].tools[0];
    expect(t.status).toBe("done");
    expect(t.result).toEqual({ stdout: "ok" });
    expect(t.duration).toBe(1.25);
  });

  it("returns the input unchanged when the toolUseId is unknown", () => {
    const session = makeSession();
    const patch: Patch = {
      op: "complete_tool",
      toolUseId: "missing",
      result: null,
      status: "done",
    };
    expect(applyPatch(session, patch)).toBe(session);
  });
});

describe("applyPatch — set_claude_status", () => {
  it("flips idle → responding", () => {
    const session = makeSession({ claudeStatus: "idle" });
    const next = applyPatch(session, { op: "set_claude_status", claudeStatus: "responding" });
    expect(next).not.toBe(session);
    expect(next.claudeStatus).toBe("responding");
  });

  it("returns the input unchanged when status is already the same", () => {
    const session = makeSession({ claudeStatus: "idle" });
    const next = applyPatch(session, { op: "set_claude_status", claudeStatus: "idle" });
    expect(next).toBe(session);
  });
});

describe("applyPatch — set_turn_stop", () => {
  it("records stopText / stopReason on the addressed turn", () => {
    const session = makeSession({ turns: [makeTurn(0), makeTurn(1)] });
    const next = applyPatch(session, {
      op: "set_turn_stop",
      turnNumber: 1,
      stopText: "done",
      stopReason: "stop",
    });
    expect(next).not.toBe(session);
    const t = next.turns.find((x) => x.turnNumber === 1)!;
    expect(t.stopText).toBe("done");
    expect(t.stopReason).toBe("stop");
  });

  it("returns the input unchanged when the turn number is unknown", () => {
    const session = makeSession();
    const next = applyPatch(session, { op: "set_turn_stop", turnNumber: 99 });
    expect(next).toBe(session);
  });
});

describe("applyPatch — add_agent", () => {
  it("appends to the current (last) turn and registers the agent index", () => {
    const session = makeSession({ currentTurn: 1, turns: [makeTurn(0), makeTurn(1)] });
    const agent = makeAgent("a1");
    const patch: Patch = { op: "add_agent", agent };
    const next = applyPatch(session, patch);
    expect(next).not.toBe(session);
    expect(next.agentIndex["a1"]).toEqual({ turnNumber: 1, agentIndex: 0 });
    const t = next.turns.find((x) => x.turnNumber === 1)!;
    expect(t.agents).toHaveLength(1);
    expect(t.agents[0].agentId).toBe("a1");
    expect(t.agentCount).toBe(1);
  });

  it("returns the input unchanged when the agent is already indexed (dedup)", () => {
    const session = makeSession({ agentIndex: { a1: { turnNumber: 0, agentIndex: 0 } } });
    const next = applyPatch(session, { op: "add_agent", agent: makeAgent("a1") });
    expect(next).toBe(session);
  });
});

describe("applyPatch — unhandled op", () => {
  it("returns the input session unchanged (by reference)", () => {
    const session = makeSession();
    // `set_token_usage` is intentionally out of this reducer's subset —
    // unknown ops fall back to the caller's "re-snapshot" path.
    const patch: Patch = { op: "set_token_usage", usage: makeTokenUsage() };
    expect(applyPatch(session, patch)).toBe(session);
  });

  it("does not mutate the input session for an unknown op", () => {
    const session = makeSession();
    const before = JSON.stringify(session);
    applyPatch(session, { op: "set_token_usage", usage: makeTokenUsage() });
    expect(JSON.stringify(session)).toBe(before);
  });

  it("does not throw on an unknown op", () => {
    const session = makeSession();
    expect(() =>
      applyPatch(session, { op: "set_token_usage", usage: makeTokenUsage() }),
    ).not.toThrow();
    expect(() =>
      applyPatch(session, { op: "set_cost", cost: 1.23 }),
    ).not.toThrow();
    expect(() =>
      applyPatch(session, { op: "add_compaction", marker: {
        reason: "manual",
        turnNumber: 0,
        timestamp: BASE_TS,
        tokensBefore: null,
        summary: null,
        aborted: false,
      } }),
    ).not.toThrow();
  });
});

// Suppress "unused" lint warnings on helpers retained for symmetry with
// the existing session-primitives test, in case a future case needs them.
void makeRole;
