// claude-pid.test.ts — Drives the REAL exported `resolveClaudePid`
// against synthetic process trees.
//
// The bug this guards: hook scripts pass CLAUDE_PID=$PPID, which is the
// claude process only when claude spawns hooks directly. Where an
// intermediate shell sits between them, $PPID is that shell — it exits as
// soon as the hook returns, so the server sees a dead pid on every event
// and reaps live sessions. Both process layouts are covered here.

import { describe, it, expect } from "vitest";
import { resolveClaudePid, type ProcInfo, type ProcLookup } from "../src/claude-pid.js";

// Builds a ProcLookup over a literal pid → (ppid, comm) tree. Any pid not
// in the table reads as unknown, mirroring `ps` on a dead process.
const treeLookup = (tree: Record<number, ProcInfo>): ProcLookup =>
  (pid) => tree[pid] ?? null;

describe("resolveClaudePid", () => {
  it("finds claude through an intermediate shell (the $PPID-is-wrong layout)", () => {
    // bridge(100) → zsh(200, exits immediately) → claude(300).
    // $PPID would yield 200; the walk must reach 300.
    const lookup = treeLookup({
      100: { ppid: 200, comm: "node" },
      200: { ppid: 300, comm: "zsh" },
      300: { ppid: 400, comm: "claude" },
      400: { ppid: 1, comm: "tmux: server" },
    });
    expect(resolveClaudePid(100, lookup)).toBe(300);
  });

  it("finds claude when it is the direct parent (the layout $PPID got right)", () => {
    const lookup = treeLookup({
      100: { ppid: 300, comm: "node" },
      300: { ppid: 1, comm: "claude" },
    });
    expect(resolveClaudePid(100, lookup)).toBe(300);
  });

  it("matches an absolute path comm, as macOS ps can report", () => {
    const lookup = treeLookup({
      100: { ppid: 300, comm: "node" },
      300: { ppid: 1, comm: "/usr/local/bin/claude" },
    });
    expect(resolveClaudePid(100, lookup)).toBe(300);
  });

  it("returns null when no claude ancestor exists, rather than guessing", () => {
    // Caller falls back to the CLAUDE_PID env value on null; returning a
    // wrong pid here would silently resurrect the false-reaping bug.
    const lookup = treeLookup({
      100: { ppid: 200, comm: "node" },
      200: { ppid: 1, comm: "zsh" },
    });
    expect(resolveClaudePid(100, lookup)).toBeNull();
  });

  it("returns null on an unreadable pid instead of throwing", () => {
    expect(resolveClaudePid(100, () => null)).toBeNull();
  });

  it("gives up after maxHops on a pathologically deep tree", () => {
    // Guards against an unbounded walk (and against a cyclic ppid chain,
    // which a pid-namespace boundary can produce).
    const lookup: ProcLookup = (pid) => ({ ppid: pid + 1, comm: "zsh" });
    expect(resolveClaudePid(100, lookup, 4)).toBeNull();
  });

  it("stops at the init boundary without consulting the lookup", () => {
    const lookup: ProcLookup = () => { throw new Error("must not be called"); };
    expect(resolveClaudePid(1, lookup)).toBeNull();
    expect(resolveClaudePid(0, lookup)).toBeNull();
  });
});
