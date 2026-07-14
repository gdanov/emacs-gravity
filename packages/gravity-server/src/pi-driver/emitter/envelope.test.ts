// envelope.test.ts — Tests for the wire-envelope builder.

import { describe, it, expect, afterEach } from "vitest";
import { buildEnvelope, PI_WORKTREE_ENV, AMBIENT_EMITTER_ROLE } from "./envelope.js";
import type { PiEventEnvelope } from "@gravity/shared";

describe("buildEnvelope", () => {
  afterEach(() => {
    delete process.env[PI_WORKTREE_ENV];
  });

  it("fills src, session_id, cwd, pid, event from ctx", () => {
    const out = buildEnvelope(
      { sessionId: "sess-1", cwd: "/tmp/proj" },
      { type: "session_start" },
    );
    expect(out.src).toBe("pi");
    expect(out.session_id).toBe("sess-1");
    expect(out.cwd).toBe("/tmp/proj");
    expect(out.pid).toBe(process.pid);
    expect(out.event).toEqual({ type: "session_start" });
  });

  it("always sets attribution.role to the ambient-emitter role literal", () => {
    const out = buildEnvelope(
      { sessionId: "s", cwd: "/x" },
      { type: "session_start" },
    );
    expect(out.attribution?.role).toBe(AMBIENT_EMITTER_ROLE);
    expect(out.attribution?.role).toBe("worker");
  });

  it("includes attribution.worktree when ctx.worktree is non-empty", () => {
    const out = buildEnvelope(
      { sessionId: "s", cwd: "/x", worktree: "/path/to/worktree" },
      { type: "session_start" },
    );
    expect(out.attribution?.worktree).toBe("/path/to/worktree");
  });

  it("omits attribution.worktree when ctx.worktree is empty/undefined", () => {
    const out1 = buildEnvelope(
      { sessionId: "s", cwd: "/x", worktree: "" },
      { type: "session_start" },
    );
    expect(out1.attribution?.worktree).toBeUndefined();
    const out2 = buildEnvelope(
      { sessionId: "s", cwd: "/x" },
      { type: "session_start" },
    );
    expect(out2.attribution?.worktree).toBeUndefined();
  });

  it("NEVER sets attribution.branch", () => {
    const out: PiEventEnvelope = buildEnvelope(
      { sessionId: "s", cwd: "/x", worktree: "/wt" },
      { type: "session_start" },
    );
    expect(out.attribution?.branch).toBeUndefined();
    // Also: even when ctx has no worktree.
    const out2: PiEventEnvelope = buildEnvelope(
      { sessionId: "s", cwd: "/x" },
      { type: "session_start" },
    );
    expect(out2.attribution?.branch).toBeUndefined();
  });

  it("does not mutate the input event", () => {
    const input = { type: "session_start", reason: "startup" };
    const snapshot = JSON.stringify(input);
    buildEnvelope({ sessionId: "s", cwd: "/x" }, input);
    expect(JSON.stringify(input)).toBe(snapshot);
  });
});