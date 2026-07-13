// health-check-liveness.test.ts — Drives the REAL exported
// `evaluateSessionHealth` for each branch of gravity-server's
// health-check decision ladder.
//
// The health-check setInterval body in `program` used to inline the
// PID-reaping / source-pi-skip / staleness-fallback branches as a single
// imperative block, which made the ordering easy to get wrong by
// hand-rolled re-implementations. `evaluateSessionHealth` extracted
// that decision into a pure function; these tests cover the four cases
// the research surfaced, calling the real exported function (with an
// injected `killFn` where the pid branch is exercised) so any future
// reordering of the priority rules trips these tests loudly.

import { describe, it, expect } from "vitest";
import {
  evaluateSessionHealth,
  type SessionHealthInput,
} from "../src/gravity-server.js";

// Mirrors gravity-server.ts's STALENESS_THRESHOLD_MS (5 minutes). Inlined
// here rather than exported from production — keeps the test self-
// contained and forces a deliberate trip if the production constant
// moves.
const STALE_MS = 5 * 60 * 1000;

describe("evaluateSessionHealth — real-exported liveness decision", () => {
  // The pid branch uses process.kill(pid, 0) which throws ESRCH for a
  // dead pid. The function accepts an injectable `killFn` so we can
  // simulate both outcomes without touching real processes.
  const neverThrows: NonNullable<Parameters<typeof evaluateSessionHealth>[2]["killFn"]> =
    () => { /* simulate a live pid: no throw */ };
  const alwaysThrows: NonNullable<Parameters<typeof evaluateSessionHealth>[2]["killFn"]> =
    () => { throw new Error("ESRCH"); };

  it("pid-bearing pi session with a DEAD pid is reaped — NOT skipped by source==='pi'", () => {
    // Emitter-fed pi sessions carry a real pid threaded in from their
    // envelope. The PID-reaping branch must intercept them BEFORE the
    // source==='pi' exemption can elide the liveness check —
    // otherwise ambient-emitter-fed pi sessions whose child processes
    // died would never be cleaned up.
    const input: SessionHealthInput = {
      pid: 999999, // unallocated; process.kill(999999, 0) reliably throws ESRCH
      source: "pi",
      lastEventTime: 0,
    };
    const result = evaluateSessionHealth(input, Date.now(), {
      staleThresholdMs: STALE_MS,
      killFn: alwaysThrows,
    });
    expect(result).toEqual({ dead: true, reason: "pid-unreachable" });
  });

  it("pid-less pi session (RPC-driven) is NEVER reaped by the staleness fallback", () => {
    // The complement: an RPC-driven pi session (created by startPiDriver)
    // has no pid — its lifecycle is owned by child.on("exit") in the
    // piDrivers map, so the health-check must NOT mark it dead purely
    // because it has been idle for a long time. Even with a very long
    // elapsed time and a tiny threshold, the source==='pi' exemption
    // takes precedence over the staleness fallback.
    const input: SessionHealthInput = {
      pid: null,
      source: "pi",
      lastEventTime: 0, // very stale
    };
    const result = evaluateSessionHealth(input, Date.now() + 10 * STALE_MS, {
      staleThresholdMs: STALE_MS,
    });
    expect(result).toEqual({ dead: false });
  });

  it("pid-less non-pi session IS reaped once elapsed > staleThresholdMs", () => {
    // A pid-less non-pi session: no pid to reap, source === 'pi' check
    // is false, so the staleness fallback applies. After the threshold
    // has elapsed the session must be reported dead with reason "stale".
    const now = 1_700_000_000_000;
    const input: SessionHealthInput = {
      pid: null,
      source: "claude-code",
      lastEventTime: now - (STALE_MS + 1),
    };
    const result = evaluateSessionHealth(input, now, {
      staleThresholdMs: STALE_MS,
    });
    expect(result).toEqual({ dead: true, reason: "stale" });
  });

  it("live pid-bearing session is NEVER reaped regardless of staleness", () => {
    // A session with a real (live) pid, regardless of how long it has
    // been idle: the pid branch's killFn returns without throwing,
    // which means "alive", which must short-circuit the staleness
    // fallback so an idle-but-alive session does not get false-
    // positive reaped.
    const now = 1_700_000_000_000;
    const input: SessionHealthInput = {
      pid: 999999,
      source: "claude-code",
      lastEventTime: 0, // infinitely stale
    };
    const result = evaluateSessionHealth(input, now, {
      staleThresholdMs: STALE_MS,
      killFn: neverThrows,
    });
    expect(result).toEqual({ dead: false });
  });
});
