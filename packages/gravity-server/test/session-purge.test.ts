import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import { SessionStore } from "../src/state/session-store.js";
import type { Session } from "@gravity/shared";

// ── Helpers ──────────────────────────────────────────────────────────

function makeSession(overrides: Partial<Session> = {}): Session {
  return {
    sessionId: "s1",
    cwd: "/test",
    project: "test",
    status: "active",
    claudeStatus: "idle",
    slug: null,
    branch: null,
    pid: 123,
    modelName: null,
    startTime: Date.now(),
    lastEventTime: Date.now(),
    tokenUsage: null,
    plan: null,
    streamingText: null,
    turns: [],
    currentTurn: 0,
    toolIndex: new Map(),
    agentIndex: new Map(),
    tasks: new Map(),
    files: new Map(),
    totalToolCount: 0,
    ...overrides,
  } as Session;
}

// ── Tests ────────────────────────────────────────────────────────────

describe("SessionStore", () => {
  let store: SessionStore;

  beforeEach(() => {
    vi.useFakeTimers();
    store = new SessionStore();
  });

  afterEach(() => {
    store.clearAllPurgeTimers();
    vi.useRealTimers();
  });

  describe("getProjectSummaries", () => {
    it("excludes ended sessions", () => {
      store.set("s1", makeSession({ sessionId: "s1", status: "active" }));
      store.set("s2", makeSession({ sessionId: "s2", status: "ended" }));

      const summaries = store.getProjectSummaries();
      expect(summaries).toHaveLength(1);
      expect(summaries[0].sessions).toHaveLength(1);
      expect(summaries[0].sessions[0].sessionId).toBe("s1");
    });

    it("excludes projects where all sessions are ended", () => {
      store.set("s1", makeSession({ sessionId: "s1", project: "proj-a", status: "ended" }));
      store.set("s2", makeSession({ sessionId: "s2", project: "proj-b", status: "active" }));

      const summaries = store.getProjectSummaries();
      expect(summaries).toHaveLength(1);
      expect(summaries[0].project).toBe("proj-b");
    });

    it("returns empty array when all sessions ended", () => {
      store.set("s1", makeSession({ sessionId: "s1", status: "ended" }));
      store.set("s2", makeSession({ sessionId: "s2", status: "ended" }));

      expect(store.getProjectSummaries()).toHaveLength(0);
    });
  });

  describe("purge timers", () => {
    it("fires callback after delay", () => {
      const callback = vi.fn();
      store.schedulePurge("s1", 120_000, callback);

      vi.advanceTimersByTime(119_999);
      expect(callback).not.toHaveBeenCalled();

      vi.advanceTimersByTime(1);
      expect(callback).toHaveBeenCalledOnce();
    });

    it("cancelPurge prevents callback", () => {
      const callback = vi.fn();
      store.schedulePurge("s1", 120_000, callback);

      store.cancelPurge("s1");
      vi.advanceTimersByTime(200_000);

      expect(callback).not.toHaveBeenCalled();
    });

    it("schedulePurge resets the timer on duplicate", () => {
      const callback1 = vi.fn();
      const callback2 = vi.fn();

      store.schedulePurge("s1", 120_000, callback1);
      vi.advanceTimersByTime(60_000); // halfway

      store.schedulePurge("s1", 120_000, callback2); // reset
      vi.advanceTimersByTime(60_000); // 60s after reset — not yet

      expect(callback1).not.toHaveBeenCalled();
      expect(callback2).not.toHaveBeenCalled();

      vi.advanceTimersByTime(60_000); // now 120s after reset
      expect(callback1).not.toHaveBeenCalled(); // first was cancelled
      expect(callback2).toHaveBeenCalledOnce();
    });

    it("clearAllPurgeTimers cancels all", () => {
      const cb1 = vi.fn();
      const cb2 = vi.fn();
      store.schedulePurge("s1", 120_000, cb1);
      store.schedulePurge("s2", 120_000, cb2);

      store.clearAllPurgeTimers();
      vi.advanceTimersByTime(200_000);

      expect(cb1).not.toHaveBeenCalled();
      expect(cb2).not.toHaveBeenCalled();
    });

    it("delete cancels purge timer", () => {
      store.set("s1", makeSession({ sessionId: "s1" }));
      const callback = vi.fn();
      store.schedulePurge("s1", 120_000, callback);

      store.delete("s1");
      vi.advanceTimersByTime(200_000);

      expect(callback).not.toHaveBeenCalled();
    });
  });

  describe("self-healing (event on ended session cancels purge)", () => {
    it("cancelPurge called for active session prevents purge", () => {
      const callback = vi.fn();
      store.set("s1", makeSession({ sessionId: "s1", status: "ended" }));
      store.schedulePurge("s1", 120_000, callback);

      // Simulate self-heal: session goes back to active
      const session = store.get("s1")!;
      session.status = "active";
      store.cancelPurge("s1");

      vi.advanceTimersByTime(200_000);
      expect(callback).not.toHaveBeenCalled();
    });
  });
});
