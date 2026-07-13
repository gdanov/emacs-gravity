// envelope.ts — Build the wire envelope the server-side translator consumes.
//
// Per #22 §2 and the frozen contract in plan.md §3, every message on the
// hook socket from the ambient pi emitter carries:
//   { src: "pi", session_id, cwd, pid, event, attribution? }
//
// `attribution` is mandatory for the emitter's domain — every session is
// explicitly tagged with its swarm role so the server-side ingest path
// (see hook-translator.ts) can group sessions by worktree/branch/role
// rather than by source alone.
//
// `attribution.branch` is NEVER set by this function (the server derives
// it from cwd via git, per #22). `attribution.worktree` is set only when
// process.env.PI_WORKTREE is defined AND the caller passed it in via
// EnvelopeContext.worktree (caller-owned).

import type { PiEventEnvelope, SessionRole } from "@gravity/shared";

/**
 * The SessionRole literal that means "headless / non-interactive run"
 * — what every ambient-emitter invocation should report. Per plan.md §2
 * pinned decision: if multiple non-interactive literals exist, prefer the
 * one closest to "headless/worker"; `SessionRole`'s literals are
 * `"coordinator" | "worker" | "interactive" | "claude-helm" | "unknown"`.
 *
 * The runtime semantic of the emitter (which only runs inside `-p`,
 * `--mode json`, `--mode rpc`, or programmatic invocations — never
 * attached to a human TTY) maps to "worker": a headless agent that
 * does the work and reports. "coordinator" implies a higher-level
 * orchestrator role, not what this extension models; "unknown" is the
 * unset default that should never persist past first attribution.
 */
export const AMBIENT_EMITTER_ROLE: SessionRole = "worker";

/** Optional env var name a caller can read to populate `EnvelopeContext.worktree`. */
export const PI_WORKTREE_ENV = "PI_WORKTREE";

export interface EnvelopeContext {
  sessionId: string;
  cwd: string;
  worktree?: string;
}

/**
 * Build a wire envelope from a context and an event payload.
 *
 * Pure function. Does not mutate inputs. The returned envelope's
 * `attribution.role` is always the constant AMBIENT_EMITTER_ROLE;
 * `attribution.worktree` is set iff `ctx.worktree` is a non-empty
 * string (empty strings are omitted — env vars left unset often resolve
 * to "" rather than undefined, and an empty worktree path would be
 * nonsensical). `attribution.branch` is never set.
 */
export function buildEnvelope(
  ctx: EnvelopeContext,
  event: Record<string, unknown>,
): PiEventEnvelope {
  const worktree = ctx.worktree?.trim();
  const attribution: PiEventEnvelope["attribution"] = {
    role: AMBIENT_EMITTER_ROLE,
  };
  if (worktree && worktree.length > 0) {
    attribution.worktree = worktree;
  }
  return {
    src: "pi",
    session_id: ctx.sessionId,
    cwd: ctx.cwd,
    pid: process.pid,
    event,
    attribution,
  };
}