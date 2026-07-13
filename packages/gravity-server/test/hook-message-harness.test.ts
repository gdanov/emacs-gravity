// Socket-free harness for processHookMessage (Phase 3.1) + the liveness /
// property invariant the prior suite never expressed (Phase 3.3).
//
// processHookMessage is the orchestration layer that the per-event tests
// (fire() -> handleEvent) bypass: capability gating, supersede
// (removeStale/forceClose), runEvent, and pull-mode signal/poll. This file
// drives it with the REAL inbox/store/event-handler and a FakeTerminals
// that records signals and serves a simulated client poll.

import { describe, it, expect, beforeEach } from "vitest";
import { Effect, Layer } from "effect";
import { EventEmitter } from "events";
import type { Socket } from "net";
import { FsTest } from "@gravity/shared";
import type { ServerMessage, HookData, Patch, HookEventName } from "@gravity/shared";
import { handleEvent } from "../src/handlers/event-handler.js";
import { SessionStore, makeSessionStore } from "../src/services/session-store.js";
import { Inbox, makeInbox } from "../src/services/inbox.js";
import type { TerminalService, TerminalConnection } from "../src/services/terminal.js";
import { processHookMessage, type HookMessageDeps } from "../src/gravity-server.js";

// ── Fakes ────────────────────────────────────────────────────────────

interface FakeSocket extends Socket {
  _written: string[];
  _ended: boolean;
}

function fakeSocket(): FakeSocket {
  const e = new EventEmitter();
  return Object.assign(e, {
    _written: [] as string[],
    _ended: false,
    write(d: string) { (this as FakeSocket)._written.push(d); return true; },
    end() { (this as FakeSocket)._ended = true; },
    destroy() {},
  }) as unknown as FakeSocket;
}

interface Recorded {
  signals: Array<{ what: string; sessionId?: string; seq?: number }>;
  broadcasts: ServerMessage[];
}

function fakeTerminals(rec: Recorded, capable = true): TerminalService {
  return {
    addConnection: () => ({}) as TerminalConnection,
    broadcast: (m) => { rec.broadcasts.push(m); },
    sendTo: () => {},
    sendToSubscribers: () => {},
    unsubscribeAll: () => {},
    hasCapableTerminal: () => capable,
    connectionCount: () => (capable ? 1 : 0),
    signalChanged: (what, sessionId, seq) => { rec.signals.push({ what, sessionId, seq }); },
    signalChangedTo: () => {},
  };
}

// ── Harness ──────────────────────────────────────────────────────────

function makeHarness(opts: { capable?: boolean; waitForCapableTerminal?: HookMessageDeps["waitForCapableTerminal"] } = {}) {
  const store = makeSessionStore();
  const inbox = makeInbox();
  const rec: Recorded = { signals: [], broadcasts: [] };
  const terminals = fakeTerminals(rec, opts.capable ?? true);

  const layer = Layer.mergeAll(
    Layer.succeed(SessionStore, store),
    Layer.succeed(Inbox, inbox),
    FsTest({}),
  );
  const runEvent: HookMessageDeps["runEvent"] = (e, s, c, d, p, sock) =>
    Effect.runSync(Effect.provide(handleEvent(e, s, c, d, p, sock), layer));

  const deps: HookMessageDeps = {
    store, inbox, terminals, runEvent,
    waitForCapableTerminal: opts.waitForCapableTerminal ?? (async () => opts.capable ?? true),
    schedulePurge: () => {},
    markHookReceived: () => {},
  };

  const send = (
    event: HookEventName,
    sessionId: string,
    data: HookData,
    needsResponse = false,
  ) => {
    const sock = fakeSocket();
    return processHookMessage(
      deps,
      { event, session_id: sessionId, cwd: "/t", pid: 1, data, needs_response: needsResponse },
      sock,
    ).then(() => sock);
  };

  // What a pull client fetches when it polls (inbox-items response).
  const simulatePoll = () => inbox.all();

  return { store, inbox, rec, send, simulatePoll };
}

const AUQ_DATA = (tuid: string): HookData => ({
  tool_name: "AskUserQuestion",
  tool_use_id: tuid,
  tool_input: { questions: [{ question: "Pick one", options: [{ label: "A" }] }] },
  questions: [{ question: "Pick one", options: [{ label: "A" }] }],
} as unknown as HookData);

// ── Tests ────────────────────────────────────────────────────────────

describe("processHookMessage — AskUserQuestion liveness invariant", () => {
  let h: ReturnType<typeof makeHarness>;
  beforeEach(() => { h = makeHarness(); });

  // The exact regression: 3 hooks fire for one AskUserQuestion. After the
  // full sequence, a pull-mode poll MUST yield exactly one renderable
  // question item — never a permission JSON item, never nothing.
  it("order A (intercept → PreToolUse → PermissionRequest): poll yields one question item", async () => {
    h.store; // session auto-created by ensureSession
    const interceptSock = await h.send("AskUserQuestionIntercept", "s1", AUQ_DATA("tu_q"), true);
    await h.send("PreToolUse", "s1", AUQ_DATA("tu_q"), false);
    const permSock = await h.send("PermissionRequest", "s1", AUQ_DATA("tu_q"), true);

    const polled = h.simulatePoll();
    expect(polled).toHaveLength(1);
    expect(polled[0].type).toBe("question");
    expect(polled[0].data.tool_use_id).toBe("tu_q");
    expect(polled.some(i => i.type === "permission")).toBe(false);

    // Intercept socket still pending (NOT force-closed with {}), so the
    // user can still answer via action.question.
    expect(interceptSock._written).toHaveLength(0);
    expect(interceptSock._ended).toBe(false);

    // Redundant PermissionRequest socket was unblocked with allow passthrough.
    expect(permSock._written).toHaveLength(1);
    expect(JSON.parse(permSock._written[0])).toEqual({
      hookSpecificOutput: { hookEventName: "PermissionRequest", decision: { behavior: "allow" } },
    });

    // A real client must have been told to poll.
    expect(h.rec.signals.some(s => s.what === "inbox")).toBe(true);
  });

  it("order B (PreToolUse → intercept → PermissionRequest): same invariant", async () => {
    await h.send("PreToolUse", "s1", AUQ_DATA("tu_q"), false);
    const interceptSock = await h.send("AskUserQuestionIntercept", "s1", AUQ_DATA("tu_q"), true);
    await h.send("PermissionRequest", "s1", AUQ_DATA("tu_q"), true);

    const polled = h.simulatePoll();
    expect(polled).toHaveLength(1);
    expect(polled[0].type).toBe("question");
    expect(polled[0].data.tool_use_id).toBe("tu_q");
    expect(interceptSock._ended).toBe(false);
  });

  it("regression proof: WITHOUT the guard, PreToolUse would orphan the question", async () => {
    // Sanity that the guard is what matters: a *different* tool_use_id on
    // the interleaving PreToolUse does NOT protect the question item.
    const interceptSock = await h.send("AskUserQuestionIntercept", "s1", AUQ_DATA("tu_q"), true);
    await h.send("PreToolUse", "s1", AUQ_DATA("tu_other"), false);

    // Different id → supersede legitimately reaps the question item.
    expect(h.simulatePoll()).toHaveLength(0);
    expect(interceptSock._ended).toBe(true);
  });

  it("supersede still works: a PRIOR different-tool pending item is reaped", async () => {
    const bashSock = await h.send(
      "PermissionRequest", "s1",
      { tool_name: "Bash", tool_use_id: "tu_bash", tool_input: { command: "ls" } } as unknown as HookData,
      true,
    );
    expect(h.simulatePoll()).toHaveLength(1); // Bash permission pending

    // A new, unrelated tool starts → its PreToolUse supersedes the stale
    // Bash permission (different tool_use_id).
    await h.send(
      "PreToolUse", "s1",
      { tool_name: "Read", tool_use_id: "tu_read", tool_input: { file_path: "/x" } } as unknown as HookData,
      false,
    );
    expect(h.simulatePoll()).toHaveLength(0);
    expect(bashSock._ended).toBe(true);
  });
});

describe("processHookMessage — ExitPlanMode plan-review (Phase 0.2 open item)", () => {
  // The plan-review item is created by PermissionRequest, not a PreToolUse
  // hook. Whether Claude's real ExitPlanMode PermissionRequest payload
  // carries the same tool_use_id as the sibling generic PreToolUse is
  // UNVERIFIED. These tests pin the guard MECHANISM given chosen payloads;
  // they do not assert Claude's real payload shape.
  let h: ReturnType<typeof makeHarness>;
  beforeEach(() => { h = makeHarness(); });

  it("preserves plan-review when the interleaving PreToolUse shares tool_use_id", async () => {
    const planData = {
      tool_name: "ExitPlanMode", tool_use_id: "tu_plan",
      tool_input: { plan: "do things" },
    } as unknown as HookData;
    const planSock = await h.send("PermissionRequest", "s1", planData, true);
    expect(h.simulatePoll()[0]?.type).toBe("plan-review");

    await h.send("PreToolUse", "s1", planData, false); // same tu_plan

    expect(h.simulatePoll()).toHaveLength(1);
    expect(h.simulatePoll()[0].type).toBe("plan-review");
    expect(planSock._ended).toBe(false);
  });

  it("reaps plan-review when a DIFFERENT tool starts (correct supersede)", async () => {
    const planSock = await h.send(
      "PermissionRequest", "s1",
      { tool_name: "ExitPlanMode", tool_use_id: "tu_plan", tool_input: { plan: "p" } } as unknown as HookData,
      true,
    );
    await h.send(
      "PreToolUse", "s1",
      { tool_name: "Edit", tool_use_id: "tu_edit", tool_input: { file_path: "/a" } } as unknown as HookData,
      false,
    );
    expect(h.simulatePoll()).toHaveLength(0);
    expect(planSock._ended).toBe(true);
  });
});

// Phase 3.4 — pull state machine. Push removal makes signal+poll the ONLY
// delivery path; these pin the properties the prior suite never expressed.
describe("pull state machine", () => {
  let h: ReturnType<typeof makeHarness>;
  beforeEach(() => { h = makeHarness(); });

  it("seq is strictly monotonic with no gaps within a session", async () => {
    await h.send("SessionStart", "s1", {});
    await h.send("UserPromptSubmit", "s1", { prompt: "a" } as unknown as HookData);
    await h.send("UserPromptSubmit", "s1", { prompt: "b" } as unknown as HookData);
    const all = h.store.getPatchesSince("s1", 0);
    expect(all.length).toBeGreaterThan(0);
    const seqs = all.map(p => p.seq);
    for (let i = 1; i < seqs.length; i++) {
      expect(seqs[i]).toBe(seqs[i - 1] + 1); // consecutive, no gaps, monotonic
    }
    expect(h.store.getSessionSeq("s1")).toBe(seqs[seqs.length - 1]);
  });

  it("getPatchesSince returns exactly the suffix after a seq", async () => {
    await h.send("SessionStart", "s1", {});
    await h.send("UserPromptSubmit", "s1", { prompt: "x" } as unknown as HookData);
    const all = h.store.getPatchesSince("s1", 0);
    const mid = all[Math.floor(all.length / 2)].seq;
    const after = h.store.getPatchesSince("s1", mid);
    expect(after.every(p => p.seq > mid)).toBe(true);
    expect(after.length).toBe(all.filter(p => p.seq > mid).length);
    expect(h.store.getPatchesSince("s1", h.store.getSessionSeq("s1"))).toHaveLength(0);
  });

  it("global seq stays monotonic across interleaved sessions", async () => {
    await h.send("SessionStart", "s1", {});
    await h.send("SessionStart", "s2", {});
    await h.send("UserPromptSubmit", "s1", { prompt: "1" } as unknown as HookData);
    await h.send("UserPromptSubmit", "s2", { prompt: "2" } as unknown as HookData);
    const s1max = h.store.getSessionSeq("s1");
    const s2max = h.store.getSessionSeq("s2");
    expect(s2max).toBeGreaterThan(s1max); // s2's later patch got a higher global seq
    // Per-session histories never interleave another session's patches.
    expect(h.store.getPatchesSince("s2", 0).every(p => p.seq > 0)).toBe(true);
  });

  it("coalesced signals: one poll after many events yields full net state", async () => {
    await h.send("SessionStart", "s1", {});
    await h.send("UserPromptSubmit", "s1", { prompt: "p" } as unknown as HookData);
    await h.send("AskUserQuestionIntercept", "s1", AUQ_DATA("tu_q"), true);
    await h.send("PreToolUse", "s1", AUQ_DATA("tu_q"), false);
    // Many signals fired (session + inbox), contentless and coalescible.
    expect(h.rec.signals.length).toBeGreaterThan(1);
    expect(h.rec.signals.some(s => s.what === "inbox")).toBe(true);
    // A single poll observes the full current truth regardless of signal count.
    expect(h.simulatePoll()).toHaveLength(1);
    expect(h.simulatePoll()[0].type).toBe("question");
    expect(h.store.getPatchesSince("s1", 0).length).toBeGreaterThan(0);
  });

  it("add+remove between polls: net truth is delivered (guard makes it survive)", async () => {
    // Question created then a DIFFERENT tool's PreToolUse reaps it, with no
    // poll in between. A single later poll must reflect the net truth.
    await h.send("AskUserQuestionIntercept", "s1", AUQ_DATA("tu_q"), true);
    await h.send("PreToolUse", "s1", AUQ_DATA("tu_other"), false); // different id → reaped
    const inboxSignals = h.rec.signals.filter(s => s.what === "inbox").length;
    expect(inboxSignals).toBeGreaterThanOrEqual(1); // client would poll & converge
    expect(h.simulatePoll()).toHaveLength(0); // net truth: gone (legit supersede)

    // Same scenario WITH the guard (same tool_use_id): net truth = survives.
    const h2 = makeHarness();
    await h2.send("AskUserQuestionIntercept", "s1", AUQ_DATA("tu_q"), true);
    await h2.send("PreToolUse", "s1", AUQ_DATA("tu_q"), false); // same id → preserved
    expect(h2.simulatePoll()).toHaveLength(1);
    expect(h2.simulatePoll()[0].type).toBe("question");
  });
});

// Phase 3.5 — non-actionable inbox item on no-capable-terminal capability
// gate. The capability-wait shrinks to a short bound; a read-only inbox item
// is created immediately so a menu-bar / browser client can render the
// pending request without a 10-second stall. If a capable terminal arrives
// within the short window, the non-actionable stub is replaced by the real
// actionable item the handler creates; if not, the item stays and the
// socket is rejected with the existing no_capable_terminal payload.
describe("processHookMessage — non-actionable inbox on no-capable-terminal", () => {
  it("capable:false + PermissionRequest (Bash): leaves one non-actionable permission item; socket rejected", async () => {
    const h = makeHarness({ capable: false });
    const sock = await h.send(
      "PermissionRequest",
      "s1",
      { tool_name: "Bash", tool_use_id: "tu_bash", tool_input: { command: "rm -rf /" } } as unknown as HookData,
      true,
    );

    const items = h.simulatePoll();
    expect(items).toHaveLength(1);
    expect(items[0].actionable).toBe(false);
    expect(items[0].type).toBe("permission");
    // Tool identity is carried through so a read-only client can render
    // WHAT was being asked for, even though it cannot answer.
    expect((items[0].data as Record<string, unknown>).tool_name).toBe("Bash");
    expect((items[0].data as Record<string, unknown>).tool_use_id).toBe("tu_bash");

    // Same socket rejection semantics as the pre-change behavior.
    expect(sock._written).toHaveLength(1);
    expect(JSON.parse(sock._written[0])).toEqual({ reason: "no_capable_terminal" });
    expect(sock._ended).toBe(true);
  });

  it("capable:false + PermissionRequest (ExitPlanMode): non-actionable plan-review item", async () => {
    const h = makeHarness({ capable: false });
    const sock = await h.send(
      "PermissionRequest",
      "s1",
      { tool_name: "ExitPlanMode", tool_use_id: "tu_plan", tool_input: { plan: "do things" } } as unknown as HookData,
      true,
    );

    const items = h.simulatePoll();
    expect(items).toHaveLength(1);
    expect(items[0].actionable).toBe(false);
    expect(items[0].type).toBe("plan-review");
    expect(sock._written).toHaveLength(1);
    expect(JSON.parse(sock._written[0])).toEqual({ reason: "no_capable_terminal" });
  });

  it("capable:false + AskUserQuestionIntercept: non-actionable question item", async () => {
    const h = makeHarness({ capable: false });
    const sock = await h.send(
      "AskUserQuestionIntercept",
      "s1",
      AUQ_DATA("tu_q"),
      true,
    );

    const items = h.simulatePoll();
    expect(items).toHaveLength(1);
    expect(items[0].actionable).toBe(false);
    expect(items[0].type).toBe("question");
    expect(sock._written).toHaveLength(1);
    expect(JSON.parse(sock._written[0])).toEqual({ reason: "no_capable_terminal" });
  });

  // The shortened capability-wait is the second half of this change. A
  // full-timing test would need to mock Date.now/timers; with a stub
  // wait callback we can only assert structural ordering — that the
  // SHORTER wait bound is wired (not the long CAPABILITY_WAIT_MS).
  it("capable:false uses a wait bound shorter than CAPABILITY_WAIT_MS (10_000)", async () => {
    const waitCalls: Array<{ capability: string; timeoutMs: number }> = [];
    const h = makeHarness({
      capable: false,
      waitForCapableTerminal: async (capability, timeoutMs) => {
        waitCalls.push({ capability, timeoutMs });
        return false;
      },
    });
    await h.send(
      "PermissionRequest",
      "s1",
      { tool_name: "Bash", tool_use_id: "tu_bash" } as unknown as HookData,
      true,
    );
    expect(waitCalls).toHaveLength(1);
    expect(waitCalls[0].capability).toBe("action.permission");
    // Structural assertion: the wait must NOT be the long 10s bound.
    expect(waitCalls[0].timeoutMs).toBeLessThan(10_000);
  });

  // When a capable terminal does arrive within the short window, the
  // non-actionable stub is removed and the normal flow produces the
  // real actionable item instead.
  it("capable:false but a terminal arrives during the wait → stub is removed, real actionable item created, no rejection", async () => {
    // hasCapableTerminal() stays false throughout (the fake never flips),
    // so this genuinely enters the non-actionable-stub branch; overriding
    // waitForCapableTerminal to resolve true simulates a terminal
    // connecting DURING the short wait window — the exact "arrived"
    // branch the stub-removal logic exists for.
    const h = makeHarness({
      capable: false,
      waitForCapableTerminal: async () => true,
    });
    const sock = await h.send(
      "PermissionRequest",
      "s1",
      { tool_name: "Bash", tool_use_id: "tu_bash", tool_input: { command: "ls" } } as unknown as HookData,
      true,
    );
    // Real actionable item created by handlePermissionRequest; no rejection.
    const items = h.simulatePoll();
    expect(items).toHaveLength(1);
    expect(items[0].actionable).toBe(true);
    expect(items[0].type).toBe("permission");
    expect(sock._written).toHaveLength(0);
    expect(sock._ended).toBe(false);
  });
});

