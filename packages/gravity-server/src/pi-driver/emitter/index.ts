// index.ts — Pi extension entry point.
//
// Subscribes to exactly the 14 events documented in plan.md §2 and
// forwards them to gravity-server's hook socket. The factory is
// synchronous (no `await`); every handler is synchronous-bodied (no
// `await`, no socket I/O on the event path); a detached drain loop
// flushes the BoundedQueue to the SocketWriter. The one place awaiting
// is allowed is session_shutdown — pi's dispose path awaits it before
// tearing down the runtime.
//
// IMPORTANT: per plan.md §2, this file is the only place that touches
// the @earendil-works/pi-coding-agent types. Every other module under
// ./ is plain TS, independently testable.
//
// IMPORTANT: never register a handler for `tool_call` or `tool_result`
// (forbidden by plan.md §2). The 14 events listed below are exhaustive
// AND minimal.

import type {
  ExtensionAPI,
  ExtensionContext,
} from "@earendil-works/pi-coding-agent";

import { basename, extname } from "node:path";

import { isInert } from "./inert.js";
import { BoundedQueue } from "./queue.js";
import { logToFile } from "./logger.js";
import {
  buildEnvelope,
  PI_WORKTREE_ENV,
} from "./envelope.js";
import { SocketWriter, DEFAULT_RECONNECT_INTERVAL_MS } from "./socket-writer.js";
import {
  compactMessageUpdate,
  compactToolExecutionUpdate,
  type ToolUpdateLastSeen,
} from "./compaction.js";

/** Default BoundedQueue capacity for the emitter. Plan.md §2 pinned this to 750. */
export const EMITTER_QUEUE_CAPACITY = 750;

/** Default drain interval (ms) for the background flush loop. */
export const DRAIN_INTERVAL_MS = 50;

/** Flush timeout (ms) used by session_shutdown. */
export const SHUTDOWN_FLUSH_MS = 2000;

/**
 * Resolve the session id to use in envelopes. Per plan.md §2 "session_id":
 * try `ctx.sessionManager.getSessionFile()` and use the basename without
 * extension as the id (this is the genuine "no better source exists"
 * case — pi's SessionManager only exposes a file PATH, not a bare id).
 */
function resolveSessionId(ctx: ExtensionContext): string {
  let filePath: string | undefined;
  try {
    // `getSessionFile` returns the session file path string or null when
    // there is no on-disk file (e.g. ephemeral run). Both are tolerated.
    const v = ctx.sessionManager.getSessionFile();
    if (typeof v === "string" && v.length > 0) filePath = v;
  } catch {
    filePath = undefined;
  }
  if (!filePath) return "ephemeral";
  const base = basename(filePath, extname(filePath));
  return base.length > 0 ? base : "ephemeral";
}

/** Read PI_WORKTREE from env (or undefined). Empty strings are omitted. */
function resolveWorktree(): string | undefined {
  const v = process.env[PI_WORKTREE_ENV];
  if (typeof v !== "string") return undefined;
  const trimmed = v.trim();
  return trimmed.length > 0 ? trimmed : undefined;
}

/** Derive the socket path: GRAVITY_HOOK_SOCK env, or default. */
function resolveHookSocketPath(): string {
  return (
    process.env["GRAVITY_HOOK_SOCK"] ??
    `${process.env["HOME"] ?? "/tmp"}/.local/state/gravity-hooks.sock`
  );
}

/**
 * The 14 event types we subscribe to. Exhaustive per plan.md §2.
 *
 * Used only as the parameter type for the small `enqueueRaw` helper
 * below — the actual `pi.on(...)` calls are spelled out explicitly so
 * each one resolves to its precise overload (the overloads are keyed on
 * the literal event-name string and reject wider string types).
 */
type EmitterEventName =
  | "session_start"
  | "session_shutdown"
  | "session_compact"
  | "agent_start"
  | "agent_end"
  | "turn_start"
  | "turn_end"
  | "message_start"
  | "message_update"
  | "message_end"
  | "tool_execution_start"
  | "tool_execution_update"
  | "tool_execution_end"
  | "model_select";

/**
 * The default-exported pi extension factory.
 *
 * Per plan.md §4 WP1 step 4:
 *   - Check isInert() FIRST inside the function body. If inert, log
 *     one line and return without registering any handler.
 *   - Construct (but DO NOT connect) a SocketWriter.
 *   - Construct a BoundedQueue of capacity 750.
 *   - Register the 14 pi.on(...) handlers — every handler is
 *     synchronous-bodied; the drain loop is detached.
 *   - session_start is the lazy-connect trigger AND the
 *     once-start-of-drain-interval trigger (guarded so a duplicate
 *     session_start does not double-start the loop).
 *   - session_shutdown awaits flush(2000) then close() then clearInterval.
 */
export default function gravityPiEmitter(pi: ExtensionAPI): void {
  // ── Kill-switch check, first thing inside the factory body ──
  if (isInert()) {
    logToFile("inert: GRAVITY_DRIVER=1 or GRAVITY_PI_EMITTER=off; no handlers registered");
    return;
  }

  // ── Per-process emitter state ──
  const queue = new BoundedQueue<Record<string, unknown>>({ capacity: EMITTER_QUEUE_CAPACITY });
  const writer = new SocketWriter({
    socketPath: resolveHookSocketPath(),
    reconnectIntervalMs: DEFAULT_RECONNECT_INTERVAL_MS,
  });
  const lastSeenToolUpdates = new Map<string, ToolUpdateLastSeen>();
  let drainTimer: NodeJS.Timeout | null = null;
  let started = false;
  // Cached session id + cwd; set on session_start and reused by every
  // later envelope. If session_start never fires, we fall back to
  // placeholders — the kill-switch ensures this extension only runs in
  // contexts where session_start does fire anyway.
  let cachedSessionId = "ephemeral";
  let cachedCwd = process.cwd();
  let cachedWorktree: string | undefined = resolveWorktree();

  /** Drain the queue into the socket. Always synchronous; never throws. */
  const drainOnce = (): void => {
    // Build the batch: optional drop marker first, then all items.
    const out: Record<string, unknown>[] = [];
    const marker = queue.drainDropMarker();
    if (marker !== null) {
      out.push(marker as unknown as Record<string, unknown>);
      // Drop counts are also visible in the log file (per plan.md §2
      // "Drop visibility") so a UI-less run can still be reconstructed.
      logToFile(
        `queue: dropped ${marker.droppedCount} event(s) since ${marker.since}`,
      );
    }
    out.push(...queue.drainAll());
    for (const event of out) {
      const ctx: EnvelopeCtx = {
        sessionId: cachedSessionId,
        cwd: cachedCwd,
        ...(cachedWorktree !== undefined ? { worktree: cachedWorktree } : {}),
      };
      const envelope = buildEnvelope(ctx, event);
      const ok = writer.send(envelope);
      if (!ok) {
        // Not connected. Item is already off the queue — this is the
        // "wedged/disconnected" drop path. Surface to the log so the
        // outage is countable from the file even when the on-wire
        // marker is unavailable (the queue is empty at this point).
        logToFile(`socket: send dropped (not connected) event=${String(event["type"])}`);
      }
    }
  };

  // ── Register raw-event handlers (everything except the 2 compacted types) ──
  // We register each name explicitly (rather than looping) so that each
  // call resolves to the precise `pi.on` overload — the overloads are
  // keyed on the literal event-name string and reject wider string types.
  const enqueueRaw = (name: EmitterEventName, rawEvent: Record<string, unknown>): void => {
    // Inject `type` since pi's event objects do not carry it themselves
    // (per plan.md §2 pinned decision on the injected type field) — the
    // translator switches on `event.type`.
    const withType: Record<string, unknown> = { type: name, ...rawEvent };
    queue.push(withType);
  };

  pi.on("session_start", (e) =>
    enqueueRaw("session_start", e as unknown as Record<string, unknown>),
  );
  pi.on("session_shutdown", (e) =>
    enqueueRaw("session_shutdown", e as unknown as Record<string, unknown>),
  );
  pi.on("session_compact", (e) =>
    enqueueRaw("session_compact", e as unknown as Record<string, unknown>),
  );
  pi.on("agent_start", (e) =>
    enqueueRaw("agent_start", e as unknown as Record<string, unknown>),
  );
  pi.on("agent_end", (e) =>
    enqueueRaw("agent_end", e as unknown as Record<string, unknown>),
  );
  pi.on("turn_start", (e) =>
    enqueueRaw("turn_start", e as unknown as Record<string, unknown>),
  );
  pi.on("turn_end", (e) =>
    enqueueRaw("turn_end", e as unknown as Record<string, unknown>),
  );
  pi.on("message_start", (e) =>
    enqueueRaw("message_start", e as unknown as Record<string, unknown>),
  );
  pi.on("message_end", (e) =>
    enqueueRaw("message_end", e as unknown as Record<string, unknown>),
  );
  pi.on("tool_execution_start", (e) =>
    enqueueRaw("tool_execution_start", e as unknown as Record<string, unknown>),
  );
  pi.on("tool_execution_end", (e) =>
    enqueueRaw("tool_execution_end", e as unknown as Record<string, unknown>),
  );
  pi.on("model_select", (e) =>
    enqueueRaw("model_select", e as unknown as Record<string, unknown>),
  );

  // ── message_update: compacted via compactMessageUpdate ──
  pi.on("message_update", (e) => {
    const env = compactMessageUpdate(e as unknown as Record<string, unknown>);
    if (env === null) return; // no assistantMessageEvent → drop
    queue.push(env);
  });

  // ── tool_execution_update: compacted via compactToolExecutionUpdate ──
  pi.on("tool_execution_update", (e) => {
    const env = compactToolExecutionUpdate(
      e as unknown as Record<string, unknown>,
      lastSeenToolUpdates,
      Date.now(),
    );
    if (env === null) return; // coalesced away
    queue.push(env);
  });

  // ── session_start: lazy connect + start the (once) drain loop ──
  pi.on("session_start", (_e, ctx) => {
    cachedSessionId = resolveSessionId(ctx);
    cachedCwd = ctx.cwd;
    cachedWorktree = resolveWorktree();
    writer.connect();
    if (!started) {
      started = true;
      drainTimer = setInterval(drainOnce, DRAIN_INTERVAL_MS);
      // Allow the process to exit naturally if nothing else holds it up.
      drainTimer.unref?.();
    }
  });

  // ── session_shutdown: bounded flush, then close. The ONLY async handler. ──
  pi.on("session_shutdown", async () => {
    // Stop the drain loop so it doesn't race the explicit flush.
    if (drainTimer !== null) {
      clearInterval(drainTimer);
      drainTimer = null;
    }
    // Drain anything currently queued synchronously first.
    drainOnce();
    // Then attempt a bounded drain of the socket's write buffer.
    try {
      await writer.flush(SHUTDOWN_FLUSH_MS);
    } catch {
      // flush() never throws by contract, but if a future change
      // makes it capable of throwing, do NOT propagate into the agent
      // loop.
    }
    writer.close();
  });
}

/** Internal type alias for the envelope builder's expected context shape. */
type EnvelopeCtx = {
  sessionId: string;
  cwd: string;
  worktree?: string;
};