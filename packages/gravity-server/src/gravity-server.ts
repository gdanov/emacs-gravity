// gravity-server — Stateful backend for emacs-gravity
//
// Long-running process that:
// 1. Accepts hook events from bridge shims (hook socket)
// 2. OR spawns pi driver as an alternative driver
// 3. Manages session state (turn tree, indexes, inbox)
// 4. Pushes view model updates to connected terminals (terminal socket)

import { createServer } from "net";
import type { Server, Socket } from "net";
import { unlinkSync } from "fs";
import { dirname } from "path";
import { pathToFileURL } from "url";
import { randomBytes } from "crypto";
import { Effect, Layer } from "effect";

import type { HookEventName, HookData, InboxItemType, Patch, ServerMessage, PiEventEnvelope, PlanFeedback } from "@gravity/shared";
import { parseTerminalMessage, isHookMessage, helloProtocolVersion, protocolMismatch, shouldSendInboxOnPoll } from "./protocol/messages.js";
import { handleEvent } from "./handlers/event-handler.js";
import { MermaidRpcServer } from "./handlers/mermaid-rpc-server.js";
import { WsGateway } from "./gateway/ws-gateway.js";
import { CLIENT_HTML_TEMPLATE } from "./gateway/client-bundle.generated.js";
import { sessionEnd, setCost, setContextUsage, setPiCommands, setPiModels, updateMeta } from "./state/session.js";
import { translatePiEvent } from "./pi-driver/hook-translator.js";
import { createAccState } from "./pi-driver/turn-accumulator.js";
import type { AccState } from "./pi-driver/types.js";

// Effect services
import { ServerConfig, ServerConfigLive } from "./services/config.js";
import { Fs, FsLive } from "@gravity/shared";
import { SessionStore, SessionStoreLive, type SessionStoreService } from "./services/session-store.js";
import { Inbox, InboxLive, type InboxService } from "./services/inbox.js";
import { Terminal, TerminalLive, type TerminalService } from "./services/terminal.js";
import type { TerminalConnection } from "./services/terminal.js";
import type { FsService } from "@gravity/shared";
import type { ServerConfigData } from "./services/config.js";

// Pi driver (optional)
import { startPiDriver, type StartPiDriverOptions, type PiSessionStats } from "./pi-driver/index.js";
import type { ExtensionUIRequestEvent } from "./pi-driver/types.js";
import { installEmitter } from "./pi-driver/install.js";
import { PI_EMITTER_SOURCE } from "./pi-driver/emitter-bundle.generated.js";

// ── Constants ────────────────────────────────────────────────────────

const CAPABILITY_WAIT_MS = 10_000;
// Short capability-wait bound used only by the no-capable-terminal
// pre-inbox path in `processHookMessage`. A read-only client can display
// the new non-actionable inbox item within this window — there is no
// reason to stall the full 10s of `CAPABILITY_WAIT_MS` when the item is
// already visible.
const NO_CAPABLE_WAIT_MS = 1000;
const CAPABILITY_POLL_MS = 500;
const PURGE_DELAY_MS = 2 * 60 * 1000;
const HEALTH_CHECK_INTERVAL_MS = 30_000;
const STALENESS_THRESHOLD_MS = 5 * 60 * 1000;
const HINT_RECENCY_GUARD_MS = 30_000;
const HOOKS_SILENCE_WARN_MS = 90_000;
const HOOKS_SILENCE_REARM_MS = 600_000;

const BIDIRECTIONAL_EVENTS: ReadonlySet<HookEventName> = new Set(["PermissionRequest", "AskUserQuestionIntercept"]);
const OVERVIEW_EVENTS: ReadonlySet<HookEventName> = new Set(["SessionStart", "SessionEnd", "UserPromptSubmit", "Stop", "PermissionRequest", "AskUserQuestionIntercept"]);


// ── Logging helper (simple, no service dependency for socket callbacks) ──

/**
 * Starts a `WsGateway` without failing the caller: resolves once
 * `start()` settles either way. A bind failure (e.g. busy port) is
 * logged at "warn" and swallowed rather than propagated, so the browser
 * gateway is best-effort and never takes down the rest of gravity-server.
 */
export function startGatewayNonFatal(
  gateway: WsGateway,
  host: string,
  log: (message: string, level?: string) => void,
): Promise<void> {
  return gateway
    .start()
    .then(() => log(`Browser gateway listening on http://${host}:${gateway.port}`))
    .catch((e: unknown) => log(`Browser gateway failed to start: ${e}`, "warn"));
}

function logMsg(message: string, level: string = "info"): void {
  const ts = new Date().toISOString();
  try {
    process.stderr.write(`[${ts}] [${level}] ${message}\n`);
  } catch { /* best effort */ }
}

// ── Pull-only state emission ─────────────────────────────────────────
//
// The server NEVER proactively pushes replicated state. It stores patches
// and emits a lightweight `state-changed` signal; clients fetch via poll.
// (Push terminal communication removed — see docs/refactor-implementation.)

function emitSessionPatches(
  store: SessionStoreService,
  terminals: TerminalService,
  sessionId: string,
  patches: Patch[],
): void {
  if (patches.length === 0) return;
  const stored = store.appendPatches(sessionId, patches);
  const seq = stored.length > 0
    ? stored[stored.length - 1].seq
    : store.getSessionSeq(sessionId);
  terminals.signalChanged("session", sessionId, seq);
}

// ── Hook message processing (extracted for socket-free testing) ──────
//
// The hook dispatch was an inner closure of `program`; extracted to a
// module-level function with explicit dependency injection so it can be
// driven by a test harness without real Unix sockets. Behaviour is a
// verbatim move — closure refs become `deps.*`, module consts unchanged.

export interface HookMessageDeps {
  readonly store: SessionStoreService;
  readonly inbox: InboxService;
  readonly terminals: TerminalService;
  readonly runEvent: (
    eventName: HookEventName, sessionId: string, cwd: string,
    data: HookData, pid: number | null, hookSocket?: Socket,
  ) => Patch[];
  readonly waitForCapableTerminal: (capability: string, timeoutMs: number) => Promise<boolean>;
  readonly schedulePurge: (sessionId: string) => void;
  readonly markHookReceived: () => void;
}

export const processHookMessage = async (
  deps: HookMessageDeps,
  msg: Record<string, unknown>,
  socket: Socket,
): Promise<void> => {
  const eventName = msg.event as HookEventName;
  const sessionId = (msg.session_id as string) || "unknown";
  const cwd = (msg.cwd as string) || "";
  const pid = (msg.pid as number) || null;
  const data = (msg.data as HookData) || {};
  const needsResponse = msg.needs_response === true;

  logMsg(`Hook event: ${eventName} session=${sessionId}`);
  deps.markHookReceived();

  // Reject bidirectional events if no capable terminal is connected.
  // The capability-wait is short (NO_CAPABLE_WAIT_MS, not the long
  // CAPABILITY_WAIT_MS) because we ALSO create a non-actionable inbox
  // item FIRST — a read-only client can see "permission needed" within
  // the same tick and there is no reason to stall the hook for the
  // full 10s bound.
  let preItem: ReturnType<InboxService["add"]> | null = null;
  if (needsResponse && BIDIRECTIONAL_EVENTS.has(eventName)) {
    if (!deps.terminals.hasCapableTerminal("action.permission")) {
      // Mirror the type/summary selection already used by
      // handlePermissionRequest / handleAskUserQuestionIntercept in
      // event-handler.ts so the non-actionable item looks like the
      // real item it is standing in for. The eventual real item will
      // be created when the hook proceeds; if no terminal ever
      // arrives, this one stays in place for read-only clients.
      const toolName = typeof data.tool_name === "string" ? data.tool_name : "unknown";
      const itemType: InboxItemType =
        eventName === "AskUserQuestionIntercept"
          ? "question"
          : toolName === "ExitPlanMode"
            ? "plan-review"
            : "permission";
      const itemSummary =
        eventName === "AskUserQuestionIntercept"
          ? ((data.tool_input as Record<string, unknown> | undefined)?.questions as
              Array<Record<string, unknown>> | undefined)?.[0]?.question as string
              ?? toolName
          : toolName;
      const trimmedCwd = cwd.replace(/\/+$/, "");
      const project = trimmedCwd.split("/").pop() || trimmedCwd || cwd;
      const label = sessionId.substring(0, 8);
      preItem = deps.inbox.add(
        itemType,
        sessionId,
        project,
        label,
        itemSummary.substring(0, 80),
        data as Record<string, unknown>,
        undefined, // no hook socket — read-only visibility, no pending response
        false, // actionable: false
      );
      logMsg(`No capable terminal connected — non-actionable inbox item ${preItem.id} created; waiting up to ${NO_CAPABLE_WAIT_MS}ms for reconnect`, "warn");
      const arrived = await deps.waitForCapableTerminal("action.permission", NO_CAPABLE_WAIT_MS);
      if (!arrived) {
        logMsg(`No capable terminal after ${NO_CAPABLE_WAIT_MS}ms — rejecting ${eventName} (non-actionable inbox item ${preItem.id} kept in place)`, "warn");
        try {
          socket.write(JSON.stringify({ reason: "no_capable_terminal" }) + "\n");
          socket.end();
        } catch { /* socket may already be closed */ }
        // Leave preItem in place — a read-only client can still see (but
        // not answer) the request after the socket is rejected. Signal
        // the inbox change so any connected read-only clients know to
        // poll and pick up the new non-actionable item.
        deps.terminals.signalChanged("inbox");
        return;
      }
      // A capable terminal arrived within the short window: clear the
      // non-actionable stub so the real actionable item (created by
      // handlePermissionRequest / handleAskUserQuestionIntercept right
      // below in the normal runEvent path) is the only item the user
      // sees for this hook.
      const removed = deps.inbox.remove(preItem.id);
      if (removed) {
        logMsg(`Capable terminal connected during wait — removed non-actionable stub ${preItem.id} before proceeding with ${eventName}`);
        deps.terminals.signalChanged("inbox");
      }
      logMsg(`Capable terminal connected during wait — proceeding with ${eventName}`);
    }
  }

  // Clean up stale bidirectional inbox items before processing
  if (!BIDIRECTIONAL_EVENTS.has(eventName)) {
    const staleRemoved = deps.inbox.removeStaleForSession(sessionId);
    for (const item of staleRemoved) {
      logMsg(`Inbox item ${item.id} (${item.type}) auto-removed: superseded by ${eventName}`);
      deps.terminals.signalChanged("inbox");
    }

    if (eventName !== "Notification") {
      // Preserve a sibling item from the SAME tool invocation: the generic
      // PreToolUse fires concurrently with AskUserQuestionIntercept (and the
      // ExitPlanMode PermissionRequest) for one tool_use_id. Without this,
      // PreToolUse force-closes the live question/plan-review item ~6ms
      // after the intercept created it. Supersede is for PRIOR tools only.
      const incomingToolUseId = (data as Record<string, unknown> | undefined)
        ?.tool_use_id as string | undefined;
      const forceClosed = deps.inbox.forceCloseStaleForSession(sessionId, incomingToolUseId);
      for (const item of forceClosed) {
        logMsg(`Inbox item ${item.id} (${item.type}) force-closed: superseded by ${eventName}`);
        deps.terminals.signalChanged("inbox");
      }
    }
  }

  const patches = deps.runEvent(eventName, sessionId, cwd, data, pid, needsResponse ? socket : undefined);

  if (patches.length > 0) {
    // Pull mode: store patches and signal, don't broadcast
    const stored = deps.store.appendPatches(sessionId, patches);
    const seq = stored.length > 0 ? stored[stored.length - 1].seq : deps.store.getSessionSeq(sessionId);
    deps.terminals.signalChanged("session", sessionId, seq);
  }

  // Schedule purge for ended sessions, cancel if session self-heals
  const session = deps.store.get(sessionId);
  if (session && session.status === "ended") {
    deps.schedulePurge(sessionId);
  } else if (session && session.status === "active") {
    deps.store.cancelPurge(sessionId);
  }

  const hasStatusPatch = patches.some(p =>
    p.op === "set_claude_status" || p.op === "set_status"
  );
  if (OVERVIEW_EVENTS.has(eventName) || hasStatusPatch) {
    deps.terminals.signalChanged("overview");
  }

  if (eventName === "SessionStart") {
    const ss = deps.store.get(sessionId);
    if (ss) {
      deps.terminals.signalChanged("session", sessionId, deps.store.getSessionSeq(sessionId));
    }
  }

  if (eventName === "PermissionRequest" || eventName === "AskUserQuestionIntercept") {
    const items = deps.inbox.all();
    if (items.length > 0) {
      const item = items[0];
      logMsg(`Inbox broadcast: type=${item.type} tool_name=${(item.data as Record<string, unknown>)?.tool_name} id=${item.id}`);
      deps.terminals.signalChanged("inbox");
    }
  }
};

// ── Health-check liveness decision (extracted for direct unit testing) ─
//
// The health-check setInterval is an inline closure inside `program` and
// is not callable in isolation. The PID-reaping-before-pi-source-skip
// ordering, the staleness-vs-pid-alive priority rules, and the exact
// log messages that distinguish each outcome are too easy to get wrong
// by hand-rolled re-implementations. Extracted as a pure function that
// takes only what it needs (pid, source, lastEventTime — NOT a Session)
// and an injectable `killFn` so tests can simulate dead/live pids
// without ever touching a real process. Caller retains all logging and
// scheduling; this function only decides.

export interface SessionHealthInput {
  pid: number | null;
  source: string | null;
  lastEventTime: number;
}

export type SessionHealthResult =
  | { dead: false }
  | { dead: true; reason: "pid-unreachable" | "stale" };

export function evaluateSessionHealth(
  session: SessionHealthInput,
  now: number,
  opts: { staleThresholdMs: number; killFn?: (pid: number, signal: 0) => void },
): SessionHealthResult {
  const killFn: (pid: number, signal: 0) => void =
    opts.killFn ?? ((pid, signal) => { process.kill(pid, signal); });
  // PID branch first: a session with a real pid is alive iff the OS
  // reports so; the staleness fallback must NOT override a live pid.
  if (session.pid && session.pid > 0) {
    try {
      killFn(session.pid, 0);
      return { dead: false };
    } catch {
      return { dead: true, reason: "pid-unreachable" };
    }
  }
  // Pid-less pi session: RPC-driven pi processes own their own exit
  // handler (via piDrivers map entry + child.on("exit")). Skip the
  // staleness fallback — pi sessions idle indefinitely waiting for the
  // next prompt, which is normal, not a dead one. Termination is
  // reported via the SessionEnd event the spawn handler emits.
  if (session.source === "pi") {
    return { dead: false };
  }
  // Plain non-pi session with no pid: fall back to the staleness clock.
  if (now - session.lastEventTime > opts.staleThresholdMs) {
    return { dead: true, reason: "stale" };
  }
  return { dead: false };
}

// ── Pull-mode terminal helpers (extracted for direct unit testing) ───
//
// The `request.session` / `request.resync` / `poll` cases of
// handleTerminalMessage each mutate conn.subscribedSessions /
// conn.sessionSeqCursor and route through terminals.sendTo. Extracted
// into pure functions so the incremental-pull cursor-seeding wiring
// (snapshot sends the full session; poll sends only patches strictly
// newer than the cursor; cursor advances only on non-empty sends) is
// behaviorally testable through the real production code path.

export interface TerminalPollDeps {
  readonly store: SessionStoreService;
  readonly terminals: TerminalService;
}

/**
 * Subscribe `conn` to `sessionId` and deliver the full session snapshot
 * when one exists. Seeds the incremental-pull cursor at the session's
 * current seq so the next `poll` begins at "everything after the
 * snapshot we just sent" — not at seq 0, which would replay the entire
 * patch history on top of the snapshot.
 */
export function subscribeAndSnapshot(
  deps: TerminalPollDeps,
  conn: TerminalConnection,
  sessionId: string,
): void {
  conn.subscribedSessions.add(sessionId);
  conn.sessionSeqCursor.set(sessionId, deps.store.getSessionSeq(sessionId));
  const session = deps.store.get(sessionId);
  if (session) {
    deps.terminals.sendTo(conn, { type: "session.snapshot", sessionId, session });
  }
}

/**
 * For every session `conn` is subscribed to, deliver any patches the
 * cursor has not yet Acked. A no-patch poll sends nothing and leaves
 * the cursor alone — the cursor is "what we have Acked", not "what we
 * have read".
 */
export function pollSubscribedSessions(
  deps: TerminalPollDeps,
  conn: TerminalConnection,
): void {
  for (const sessionId of conn.subscribedSessions) {
    const session = deps.store.get(sessionId);
    if (!session) continue;
    const since = conn.sessionSeqCursor.get(sessionId) ?? 0;
    const patches = deps.store.getPatchesSince(sessionId, since);
    const seq = deps.store.getSessionSeq(sessionId);
    if (patches.length > 0) {
      deps.terminals.sendTo(conn, {
        type: "session-patches",
        sessionId,
        seq,
        patches: patches.map(p => p.patch),
      });
      conn.sessionSeqCursor.set(sessionId, seq);
    }
  }
}

// ── Purge cleanup (extracted for direct unit testing) ────────────────
//
// `schedulePurge`'s body was a single closed-over callback that
// deleted the session, cleared its patches, removed inbox items for
// the session, broadcast `session.removed`, unsubscribed terminals,
// signaled the overview change, and dropped the piEnvelopeAccStates
// entry. Pulled out into a callable function so the wiring (and the
// absence of logging inside) is independently verifiable — the caller
// retains the `Purged ended session ...` log exactly as before.

export interface SchedulePurgeDeps {
  readonly store: SessionStoreService;
  readonly inbox: InboxService;
  readonly terminals: TerminalService;
  readonly piEnvelopeAccStates: Map<string, AccState>;
}

export function purgeSession(deps: SchedulePurgeDeps, sessionId: string): void {
  deps.store.delete(sessionId);
  deps.store.clearPatches(sessionId);
  deps.inbox.removeForSession(sessionId);
  deps.terminals.broadcast({ type: "session.removed", sessionId });
  deps.terminals.unsubscribeAll(sessionId);
  deps.terminals.signalChanged("overview");
  deps.piEnvelopeAccStates.delete(sessionId);
}

// ── Program ──────────────────────────────────────────────────────────

const program = Effect.gen(function* () {
  const config = yield* Effect.service(ServerConfig);
  const fs = yield* Effect.service(Fs);
  const store = yield* Effect.service(SessionStore);
  const inbox = yield* Effect.service(Inbox);
  const terminals = yield* Effect.service(Terminal);

  // The layer for handleEvent (it needs SessionStore, Inbox, Fs)
  const eventLayer = Layer.mergeAll(
    Layer.succeed(SessionStore, store),
    Layer.succeed(Inbox, inbox),
    FsLive,
  );

  /** Run handleEvent synchronously with services. */
  const runEvent = (
    eventName: HookEventName, sessionId: string, cwd: string,
    data: HookData, pid: number | null, hookSocket?: Socket,
  ): Patch[] =>
    Effect.runSync(Effect.provide(
      handleEvent(eventName, sessionId, cwd, data, pid, hookSocket),
      eventLayer,
    ));

  /** Process a translation result from the pi driver. */
  const handlePiTranslation = (
    outerSessionId: string,
    result: { hookEvent: HookEventName; hookData: HookData },
  ): void => {
    // Use the outer session ID from startPiSession — do NOT read from hookData.session_id
    // (that would be the inner ID generated by startPiDriver's own generateSessionId)
    const sessionId = outerSessionId;
    const cwd = result.hookData.cwd as string || config.piCwd || process.cwd();

    logMsg(`Pi driver event: ${result.hookEvent} session=${sessionId}`);
    const patches = runEvent(result.hookEvent, sessionId, cwd, result.hookData, null);

    if (patches.length > 0) {
      const stored = store.appendPatches(sessionId, patches);
      const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
      terminals.signalChanged("session", sessionId, seq);
      logMsg(`Pi driver: ${patches.length} patches for ${result.hookEvent}, signaled session seq=${seq}`);
    }

    // Handle session lifecycle for overview updates
    const hasStatusPatch = patches.some(p =>
      p.op === "set_claude_status" || p.op === "set_status"
    );
    if (OVERVIEW_EVENTS.has(result.hookEvent) || hasStatusPatch) {
      terminals.signalChanged("overview");
    }

    // Schedule purge for ended sessions
    const session = store.get(sessionId);
    if (session && session.status === "ended") {
      schedulePurge(sessionId);
    } else if (session && session.status === "active") {
      store.cancelPurge(sessionId);
    }
  };

  /**
   * Consume one `PiEventEnvelope` from the hook socket's pi-envelope path.
   *
   * The envelope carries `event` (the raw pi extension event), `pid`,
   * `cwd`, and optional `attribution` (worktree / branch / role). This
   * function:
   *   1. Lazily creates the per-session `AccState` on first envelope.
   *   2. Translates `event` through `translatePiEvent` and routes each
   *      emitted `TranslationResult` through the existing `handlePiTranslation`
   *      so patch-store/signal behavior is identical to the RPC path.
   *   3. Threads `readOnly: true` (always), the envelope's `attribution`
   *      fields (when present), and `pid` (when a positive number) into
   *      `updateMeta` calls on the session — same patch-store/signal
   *      shape used by `handlePiTranslation`.
   *
   * The path is fire-and-forget — envelopes do NOT carry a `needs_response`
   * and do not engage the bidirectional capability-wait. `processHookMessage`
   * is bypassed entirely; the hook socket's per-line dispatcher routes
   * envelopes here and Claude-Code hook messages to `handleHookMessage`.
   */
  const processPiEnvelope = (envelope: PiEventEnvelope): void => {
    const sessionId = envelope.session_id;
    if (!sessionId) {
      logMsg(`Pi envelope missing session_id — dropping`, "warn");
      return;
    }

    // Lazy-create the AccState on first envelope.
    let accState = piEnvelopeAccStates.get(sessionId);
    const isFirstEnvelope = !accState;
    if (!accState) {
      accState = createAccState(sessionId, envelope.cwd);
      piEnvelopeAccStates.set(sessionId, accState);
    }

    logMsg(`Pi envelope: type=${(envelope.event as { type?: string }).type ?? "?"} session=${sessionId}`);

    // Translate and route emitted results through the existing RPC-path
    // dispatcher so patch-store/signal behavior matches byte-for-byte.
    const translated = translatePiEvent(envelope.event as unknown as Parameters<typeof translatePiEvent>[0], accState);
    if (translated.kind === "emit") {
      for (const result of translated.results) {
        handlePiTranslation(sessionId, result);
      }
    }

    // After the first translated event has run (which will have called
    // ensureSession/createSession for us), thread attribution + pid +
    // readOnly through `updateMeta` exactly like `captureBranch` does
    // for late-arriving branch info.
    const session = store.get(sessionId);
    if (session) {
      const attr = envelope.attribution;
      const metaOpts: Parameters<typeof updateMeta>[1] = { readOnly: true };
      if (attr) {
        if (typeof attr.worktree === "string" && attr.worktree) {
          metaOpts.worktree = attr.worktree;
        }
        if (typeof attr.branch === "string" && attr.branch) {
          metaOpts.branch = attr.branch;
        }
        if (typeof attr.role === "string" && attr.role) {
          metaOpts.role = attr.role as Parameters<typeof updateMeta>[1]["role"];
        }
      }
      if (typeof envelope.pid === "number" && envelope.pid > 0) {
        metaOpts.pid = envelope.pid;
      }

      const firstTouch = isFirstEnvelope || metaOpts.pid !== undefined
        || metaOpts.worktree !== undefined
        || metaOpts.branch !== undefined
        || metaOpts.role !== undefined;
      // Always emit a readOnly patch on first envelope (so terminals see
      // the session go non-actionable ASAP) and on any later envelope
      // that carries new attribution/pid fields.
      if (firstTouch) {
        const metaPatches = updateMeta(session, metaOpts);
        if (metaPatches.length > 0) {
          const stored = store.appendPatches(sessionId, metaPatches);
          const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
          terminals.signalChanged("session", sessionId, seq);
          logMsg(`Pi envelope: ${metaPatches.length} meta patches for session=${sessionId}, signaled seq=${seq}`);
        }
      }
    }
  };

  /**
   * Handle a pi `extension_ui_request`. Dialog methods create an inbox
   * item and stash the pi request id in `pendingPiUIResponses` so the
   * subsequent `action.permission` / `action.question` from a terminal
   * can be routed back to pi. Fire-and-forget methods are logged and
   * dropped (UI integration TBD).
   *
   * Method mapping:
   * - confirm → permission inbox; allow → {confirmed:true}, deny → {confirmed:false}
   * - select  → question inbox; answer[0] → {value:<choice>}
   * - input / editor → auto-cancel (no matching inbox surface yet)
   * - notify / setStatus / setWidget / setTitle / set_editor_text → log only
   */
  const handlePiExtensionUIRequest = (
    sessionId: string,
    request: ExtensionUIRequestEvent,
  ): void => {
    const session = store.get(sessionId);
    if (!session) {
      logMsg(`pi extension_ui_request for unknown session ${sessionId}`, "warn");
      return;
    }
    const method = request.method;
    const sendResponse = (payload: { value?: string; confirmed?: boolean; cancelled?: boolean }): void => {
      const driver = piDrivers.get(sessionId);
      if (!driver) {
        logMsg(`pi extension_ui_response: no driver for session ${sessionId} (request id=${request.id})`, "warn");
        return;
      }
      driver.sendExtensionUIResponse({ id: request.id, ...payload });
    };

    switch (method) {
      case "confirm": {
        const summary = request.title ?? request.message ?? "Pi confirm";
        const item = inbox.add(
          "permission",
          sessionId,
          session.project,
          session.slug || sessionId.substring(0, 8),
          summary,
          {
            // Shape the action.permission handler expects: tool_name +
            // tool_input. We use a synthetic tool name so the UI label
            // makes sense.
            tool_name: "pi:confirm",
            tool_input: { title: request.title, message: request.message },
            // Pi-specific fields for renderers that want them:
            pi_ui: { method, id: request.id, options: ["Allow", "Block"] },
          },
        );
        pendingPiUIResponses.set(item.id, { sessionId, piRequestId: request.id, method });
        terminals.signalChanged("inbox");
        break;
      }
      case "select": {
        const options = request.options ?? [];
        const summary = request.title ?? "Pi select";
        const item = inbox.add(
          "question",
          sessionId,
          session.project,
          session.slug || sessionId.substring(0, 8),
          summary,
          {
            tool_name: "pi:select",
            tool_input: {
              // action.question's handler reads tool_input.questions[] —
              // synthesize a single question entry with the option list.
              questions: [{ question: summary, options }],
            },
            pi_ui: { method, id: request.id, options },
          },
        );
        pendingPiUIResponses.set(item.id, { sessionId, piRequestId: request.id, method });
        terminals.signalChanged("inbox");
        break;
      }
      case "input":
      case "editor": {
        // Route free-text input/editor like `select` (into the question
        // inbox) so any connected terminal can answer. The discriminator
        // `pi_ui.kind: "text"` + empty options[] tells the terminal to
        // render a text-entry surface instead of pick-one. The existing
        // `action.question` pi-branch already turns answers[0] into
        // {value} (and an absent answer into {cancelled:true}), so no
        // change to the response handler is needed.
        const summary =
          request.title ?? request.message ??
          (method === "editor" ? "Pi editor" : "Pi input");
        const item = inbox.add(
          "question",
          sessionId,
          session.project,
          session.slug || sessionId.substring(0, 8),
          summary,
          {
            tool_name: "pi:input",
            tool_input: {
              // Empty options[] — action.question's handler still finds a
              // question entry, but the terminal branches on pi_ui.kind.
              questions: [{ question: summary, options: [] }],
            },
            pi_ui: {
              method,
              id: request.id,
              kind: "text",
              prefill: request.prefill,
              placeholder: request.placeholder,
              title: request.title,
              message: request.message,
              multiline: method === "editor",
            },
          },
        );
        pendingPiUIResponses.set(item.id, { sessionId, piRequestId: request.id, method });
        terminals.signalChanged("inbox");
        break;
      }
      case "notify": {
        // Surface pi-extension notifications as gravity notices (the
        // `notice` ServerPushMessage already exists and Emacs already
        // renders it). Fire-and-forget — pi does not expect a response.
        const level: "info" | "warn" | "error" =
          request.notifyType === "error"
            ? "error"
            : request.notifyType === "warning"
              ? "warn"
              : "info";
        terminals.broadcast({ type: "notice", level, text: request.message ?? "" });
        break;
      }
      // Fire-and-forget: pi doesn't expect a response. Log for visibility;
      // wiring these into the UI (status bar, window title) is a follow-up.
      case "setStatus":
      case "setWidget":
      case "setTitle":
      case "set_editor_text":
        logMsg(`pi UI notice ${method}: ${JSON.stringify({
          text: request.text, message: request.message, statusText: request.statusText, title: request.title,
        })}`);
        break;
      default:
        logMsg(`pi extension_ui_request unknown method ${method} — cancelling`, "warn");
        sendResponse({ cancelled: true });
    }
  };

  /**
   * Apply a `get_session_stats` response from pi to the session. Emits
   * `set_cost` and `set_context_usage` patches as needed. Token usage is
   * already covered by Stop's hookData.token_usage; we don't overwrite it
   * here to avoid double-counting.
   */
  /**
   * Extract git branch from pi's session file (first JSONL line has gitBranch)
   * and update both the session and the driver's accumulator state.
   * Retries until the session file has content or max attempts reached.
   */
  const captureBranch = (sessionId: string, sessionFile: string, driver: PiDriver): void => {
    const attemptRead = (attempt = 0): void => {
      if (piDrivers.get(sessionId) !== driver) return;
      try {
        // Read just the first line — that's where gitBranch lives
        const { readFileSync } = require("fs") as { readFileSync: (path: string, enc: string) => string };
        let branch: string | null = null;
        try {
          const fd = require("fs").openSync(sessionFile, "r");
          const buf = Buffer.alloc(64 * 1024);
          const n = require("fs").readSync(fd, buf, 0, buf.length, 0);
          require("fs").closeSync(fd);
          if (n > 0) {
            const firstLine = buf.toString("utf-8", 0, n).split("\n")[0];
            if (firstLine) {
              const obj = JSON.parse(firstLine) as { gitBranch?: string | null };
              branch = obj.gitBranch ?? null;
            }
          }
        } catch {
          // File not ready yet — retry
        }
        if (branch === null && attempt < 5) {
          setTimeout(() => attemptRead(attempt + 1), 200 * (attempt + 1));
          return;
        }
        // Update accumulator so subsequent SessionStarts carry branch
        if (branch !== null) {
          const accState = driver.getAccState();
          accState.branch = branch;
        }
        // Update session metadata
        const session = store.get(sessionId);
        if (session && branch !== null) {
          const patches = updateMeta(session, { branch });
          if (patches.length > 0) {
            const stored = store.appendPatches(sessionId, patches);
            const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
            terminals.signalChanged("session", sessionId, seq);
          }
        }
      } catch (err) {
        if (attempt < 3) {
          setTimeout(() => attemptRead(attempt + 1), 200 * (attempt + 1));
        }
      }
    };
    setImmediate(() => attemptRead());
  };

  const applyPiSessionStats = (sessionId: string, stats: PiSessionStats): void => {
    const session = store.get(sessionId);
    if (!session) return;
    const patches: Patch[] = [];
    if (typeof stats.cost === "number") {
      patches.push(...setCost(session, stats.cost));
    }
    if (stats.contextUsage) {
      patches.push(...setContextUsage(session, {
        tokens: stats.contextUsage.tokens,
        contextWindow: stats.contextUsage.contextWindow,
        percent: stats.contextUsage.percent,
      }));
    }
    if (patches.length === 0) return;
    const stored = store.appendPatches(sessionId, patches);
    const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
    terminals.signalChanged("session", sessionId, seq);
  };

  // Define helper functions before pi driver check

  /** Poll until a terminal with `capability` connects, or timeout. */
  const waitForCapableTerminal = (capability: string, timeoutMs: number): Promise<boolean> =>
    new Promise((resolve) => {
      if (terminals.hasCapableTerminal(capability)) { resolve(true); return; }
      const start = Date.now();
      const interval = setInterval(() => {
        if (terminals.hasCapableTerminal(capability)) {
          clearInterval(interval);
          resolve(true);
        } else if (Date.now() - start >= timeoutMs) {
          clearInterval(interval);
          resolve(false);
        }
      }, CAPABILITY_POLL_MS);
    });

  /** Schedule purge of an ended session. */
  const schedulePurge = (sessionId: string): void => {
    store.schedulePurge(sessionId, PURGE_DELAY_MS, () => {
      purgeSession({ store, inbox, terminals, piEnvelopeAccStates }, sessionId);
      logMsg(`Purged ended session ${sessionId}`);
    });
  };

  // ── Pi driver session management ───────────────────────────────

  // Map of sessionId → pi driver. N concurrent pi processes are supported,
  // one per gravity session. Each entry is owned exclusively by the lifecycle
  // callback for that driver (which adds on start and removes on stop/error).
  type PiDriver = ReturnType<typeof startPiDriver>;
  const piDrivers = new Map<string, PiDriver>();

  // Map of sessionId → AccState for sessions fed by pi ambient-event
  // envelopes on the hook socket (vs. RPC-spawned drivers). One AccState
  // per session is created lazily on the FIRST envelope seen for that
  // session, so envelopes arriving without an upfront SessionStart (the
  // normal emitter case — there is no eager server-side synthesis for
  // this path) can still exercise the translator's stateful accumulation.
  // The entry is deleted in `schedulePurge`'s `onPurge` callback.
  const piEnvelopeAccStates = new Map<string, AccState>();

  /**
   * Maps gravity inbox item id → originating pi session id + extension_ui
   * request id + method. Populated when pi emits an `extension_ui_request`;
   * consumed by the `action.permission` / `action.question` handlers to
   * format and send the `extension_ui_response` back to the *correct* pi
   * driver (instead of writing to a hook socket like Claude Code's
   * bidirectional flow does).
   */
  const pendingPiUIResponses = new Map<number, {
    sessionId: string;
    piRequestId: string;
    method: string;
  }>();

  /** Start a new pi session (called via terminal message). */
  const startPiSession = (options: { cwd?: string; thinkingLevel?: string; resumeSession?: string }): string | null => {
    const sessionId = generateSessionId();
    const cwd = options.cwd ?? process.cwd();
    const thinking = options.thinkingLevel ?? "medium";
    logMsg(`Starting pi session ${sessionId} (cwd=${cwd}, thinking=${thinking})`);

    // Broadcast pi.session.started for Emacs (control event; sets pi-session-id)
    terminals.broadcast({
      type: "pi.session",
      sessionId,
      event: "started",
      cwd,
    } as ServerMessage);

    // Eagerly synthesize a SessionStart so the session appears in the overview
    // immediately, instead of waiting for pi's own agent_start (which only
    // fires after the user sends a prompt). Pi's later agent_start will be a
    // no-op for an already-existing session.
    handlePiTranslation(sessionId, {
      hookEvent: "SessionStart",
      hookData: {
        session_id: sessionId,
        cwd,
        source: "pi",
        effort_level: thinking,
      } as HookData,
    });

    const driver = startPiDriver({
      cwd: options.cwd ?? process.cwd(),
      thinkingLevel: (options.thinkingLevel as any) ?? "medium",
      sessionId,
      resumeSession: options.resumeSession,
      onExtensionUIRequest: (request) => {
        handlePiExtensionUIRequest(sessionId, request);
      },
      onTranslation: (result) => {
        // Route using the outer sessionId from startPiSession
        handlePiTranslation(sessionId, result);
        // After Stop (pi's agent_end), poll get_session_stats to refresh
        // cost and contextUsage. Fire-and-forget — errors are logged but
        // don't block other event processing.
        if (result.hookEvent === "Stop") {
          // Defer to next tick so the Stop patch lands first.
          setImmediate(() => {
            const d = piDrivers.get(sessionId);
            if (!d) return;
            d.getSessionStats().then(
              (stats) => applyPiSessionStats(sessionId, stats),
              (err) => logMsg(`pi get_session_stats failed: ${err.message}`, "warn"),
            );
          });
        }
      },
      onLifecycle: (event, _metadata) => {
        if (event === "start") {
          logMsg(`Pi session started: ${sessionId}`);
        } else if (event === "stop" || event === "error") {
          if (event === "error") logMsg(`Pi session error`, "error");
          else logMsg(`Pi session ended: ${sessionId}`);
          // Cancel only THIS driver's pending pi UI dialogs — other pi
          // sessions' dialogs must remain intact. Iterate by entry so we
          // can match on sessionId. confirm/select AND input/editor are
          // all registered the same way in pendingPiUIResponses, so this
          // loop drops every pending dialog type on exit.
          for (const [itemId, entry] of pendingPiUIResponses) {
            if (entry.sessionId !== sessionId) continue;
            inbox.remove(itemId);
            terminals.signalChanged("inbox");
            pendingPiUIResponses.delete(itemId);
          }
          terminals.broadcast({
            type: "pi.session",
            sessionId,
            event: "stopped",
          } as ServerMessage);
          piDrivers.delete(sessionId);
        }
      },
    });

    piDrivers.set(sessionId, driver);

    // Capture pi's on-disk session file once pi is ready. Retry a few
    // times because get_state may race with pi's session-init (it returns
    // sessionFile=null until the session is loaded). Fire-and-forget.
    const captureSessionFile = (attempt = 0): void => {
      if (piDrivers.get(sessionId) !== driver) return;
      driver.getState().then((state) => {
        const f = (state as { sessionFile?: unknown }).sessionFile;
        const name = (state as { sessionName?: unknown }).sessionName;
        const session = store.get(sessionId);
        if (session) {
          // Fold both pi's session-file path and its own session name (if
          // present) into one set_meta patch. displayName drives the overview
          // label; piSessionFile drives resume. Either may be absent on any
          // given poll — only emit when at least one is present.
          const opts = {
            ...(typeof f === "string" && f ? { piSessionFile: f } : {}),
            ...(typeof name === "string" && name ? { displayName: name } : {}),
          };
          if (Object.keys(opts).length > 0) {
            const patches = updateMeta(session, opts);
            const stored = store.appendPatches(sessionId, patches);
            const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
            terminals.signalChanged("session", sessionId, seq);
          }
        }
        if (typeof f === "string" && f.length > 0) {
          // Also extract gitBranch from the session file and update session + accumulator.
          captureBranch(sessionId, f, driver);
        } else if (attempt < 3) {
          setTimeout(() => captureSessionFile(attempt + 1), 500 * (attempt + 1));
        }
      }, (err) => {
        if (attempt < 3) {
          setTimeout(() => captureSessionFile(attempt + 1), 500 * (attempt + 1));
        } else {
          logMsg(`pi get_state failed after retries: ${err.message}`, "warn");
        }
      });
    };
    setImmediate(() => captureSessionFile());

    // Fetch pi's command inventory (extension commands, prompt templates,
    // skills). Used by Emacs to drive slash-command autocomplete in the
    // compose buffer. Fire-and-forget — failures are logged but don't gate
    // session startup.
    setImmediate(() => refreshPiCommands(sessionId));

    // Fetch pi's available-model list. Used by Emacs to back the model
    // picker with real models instead of hand-typed strings. Fire-and-
    // forget — failures are logged but don't gate session startup.
    setImmediate(() => refreshPiModels(sessionId));

    return sessionId;
  };

  /**
   * Fetch `get_commands` from the pi process backing SESSION-ID and broadcast
   * a `set_pi_commands` patch. Idempotent: safe to call repeatedly. Drops
   * silently if the session has already been replaced (a `pi.stop` racing
   * with `pi.refresh-commands`).
   */
  const refreshPiCommands = (sessionId: string): void => {
    const driver = piDrivers.get(sessionId);
    if (!driver) return;
    driver.getCommands().then(
      (commands) => {
        // Bail if the driver was swapped/stopped while the RPC was in flight.
        if (piDrivers.get(sessionId) !== driver) return;
        const session = store.get(sessionId);
        if (!session) return;
        const patches = setPiCommands(session, commands);
        if (patches.length === 0) return;
        const stored = store.appendPatches(sessionId, patches);
        const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
        terminals.signalChanged("session", sessionId, seq);
        logMsg(`Pi[${sessionId}]: ${commands.length} commands available`);
      },
      (err) => logMsg(`pi get_commands failed for ${sessionId}: ${err.message}`, "warn"),
    );
  };

  /**
   * Fetch `get_available_models` from the pi process backing SESSION-ID and
   * broadcast a `set_pi_models` patch. Idempotent: safe to call repeatedly.
   * Drops silently if the session has already been replaced (a `pi.stop`
   * racing with `pi.refresh-models`).
   */
  const refreshPiModels = (sessionId: string): void => {
    const driver = piDrivers.get(sessionId);
    if (!driver) return;
    driver.getAvailableModels().then(
      (models) => {
        // Bail if the driver was swapped/stopped while the RPC was in flight.
        if (piDrivers.get(sessionId) !== driver) return;
        const session = store.get(sessionId);
        if (!session) return;
        const patches = setPiModels(session, models);
        if (patches.length === 0) return;
        const stored = store.appendPatches(sessionId, patches);
        const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
        terminals.signalChanged("session", sessionId, seq);
        logMsg(`Pi[${sessionId}]: ${models.length} models available`);
      },
      (err) => logMsg(`pi get_available_models failed for ${sessionId}: ${err.message}`, "warn"),
    );
  };

  /** Look up a pi driver by sessionId, logging a warning if not found. */
  const getPiDriver = (sessionId: string, op: string): PiDriver | null => {
    const d = piDrivers.get(sessionId);
    if (!d) {
      logMsg(`${op}: no pi driver for session ${sessionId}`, "warn");
      return null;
    }
    return d;
  };

  /** Send a prompt to a specific pi session. */
  const piSessionPrompt = (sessionId: string, text: string, images?: string[]): void => {
    const d = getPiDriver(sessionId, "pi.prompt");
    if (!d) return;
    logMsg(`pi.prompt: forwarding to driver session=${sessionId} text_len=${text.length}`);
    d.prompt(text, images)
      .then(() => logMsg(`pi.prompt: sent to pi session=${sessionId}`))
      .catch((err) => {
        logMsg(`pi.prompt error: ${err.message}`, "error");
      });
  };

  /** Send steering message to a specific pi session. */
  const piSessionSteer = (sessionId: string, text: string): void => {
    const d = getPiDriver(sessionId, "pi.steer");
    if (!d) return;
    d.steer(text);
  };

  /** Abort the current turn of a specific pi session. */
  const piSessionAbort = (sessionId: string): void => {
    const d = getPiDriver(sessionId, "pi.abort");
    if (!d) return;
    d.abort();
  };

  /** Set thinking level for a specific pi session. */
  const piSessionSetThinking = (sessionId: string, level: string): void => {
    const d = getPiDriver(sessionId, "pi.set-thinking");
    if (!d) return;
    d.setEffortLevel(level);
  };

  /**
   * Set pi's session name for a specific session (`set_session_name` RPC).
   * Optimistically reflects the name in the overview via `set_meta`; pi
   * persists it and the next `get_state` poll confirms it.
   */
  const piSessionSetSessionName = (sessionId: string, name: string): void => {
    const d = getPiDriver(sessionId, "pi.set-session-name");
    if (!d) return;
    d.setSessionName(name);
    const session = store.get(sessionId);
    if (session) {
      const patches = updateMeta(session, { displayName: name });
      const stored = store.appendPatches(sessionId, patches);
      const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
      terminals.signalChanged("session", sessionId, seq);
    }
  };

  /** Switch model for a specific pi session (pi `set_model` RPC). */
  const piSessionSetModel = (sessionId: string, provider: string, modelId: string): void => {
    const d = getPiDriver(sessionId, "pi.set-model");
    if (!d) return;
    d.setModel(provider, modelId);
  };

  /** Manually compact a specific pi session's context (`compact` RPC). */
  const piSessionCompact = (sessionId: string, customInstructions?: string): void => {
    const d = getPiDriver(sessionId, "pi.compact");
    if (!d) return;
    d.compact(customInstructions).then(
      (data) => {
        const summary = data.summary ? ` — ${data.summary.substring(0, 80)}…` : "";
        logMsg(`pi compact done (tokensBefore=${data.tokensBefore ?? "?"})${summary}`);
      },
      (err) => logMsg(`pi compact failed: ${err.message}`, "error"),
    );
  };

  /** Start a fresh session inside a specific pi process (`new_session` RPC). */
  const piSessionNewSession = (sessionId: string): void => {
    const d = getPiDriver(sessionId, "pi.new-session");
    if (!d) return;
    d.newSession().then(
      (ok) => {
        if (!ok) logMsg(`pi new_session cancelled by extension`, "warn");
      },
      (err) => logMsg(`pi new_session failed: ${err.message}`, "error"),
    );
  };

  /**
   * Resume a pi session. If SESSION-ID identifies a live pi driver, swap its
   * working session via `switch_session` to SESSION-PATH. Otherwise spawn a
   * brand-new pi process resuming SESSION-PATH (`--session <path>`).
   */
  const piSessionResume = (sessionPath: string, sessionId?: string): void => {
    if (!sessionPath) {
      logMsg(`pi.resume called without sessionPath`, "warn");
      return;
    }
    const driver = sessionId ? piDrivers.get(sessionId) : undefined;
    if (driver) {
      driver.switchSession(sessionPath).then(
        (ok) => {
          if (!ok) logMsg(`pi switch_session cancelled by extension`, "warn");
        },
        (err) => logMsg(`pi switch_session failed: ${err.message}`, "error"),
      );
    } else {
      // No live driver for the given id (or no id given) — spawn a new pi
      // process resuming the path.
      const newId = startPiSession({ resumeSession: sessionPath } as { cwd?: string; thinkingLevel?: string; resumeSession?: string });
      if (newId) {
        logMsg(`Pi session ${newId} spawned resuming ${sessionPath}`);
      }
    }
  };

  /** Stop a specific pi session — terminates the pi process. */
  const stopPiSession = async (sessionId: string): Promise<void> => {
    const d = piDrivers.get(sessionId);
    if (!d) return;
    await d.stop();
    // The driver's onLifecycle("stop") callback removes from piDrivers and
    // cleans up that session's UI requests. No need to do it here.
  };

  // ── Pi session control ────────────────────────────────────────────
  //
  // The terminal message handler below references startPiSession,
  // piSessionPrompt, etc. directly by their closure-captured names — no
  // dispatch table needed. (An older draft stashed these on `program` via
  // `as any` casts; that indirection was unused and has been removed.)

  // Generate session ID (needed for pi mode)
  const generateSessionId = (): string => {
    const now = Date.now().toString(36);
    const random = Math.random().toString(36).substring(2, 10);
    return `pi-${now}-${random}`;
  };

  // ── Pi driver mode (--pi flag starts pi instead of hook socket) ──

  if (config.piEnabled) {
    logMsg(`Pi driver mode enabled (cwd=${config.piCwd ?? process.cwd()}, thinking=${config.piThinkingLevel ?? "medium"})`);

    // Auto-start a pi session when --pi flag is passed
    const sessionId = startPiSession({
      cwd: config.piCwd,
      thinkingLevel: config.piThinkingLevel,
    });

    if (sessionId) {
      logMsg(`Auto-started pi session: ${sessionId}`);
    }

    // Note: We still want terminal connections, so don't return early
    // Instead, fall through to start the terminal server below
    // But skip the hook server since pi drives the session
  }

  // ── Hook socket mode (default) ───────────────────────────────────

  // ── Hook message handler ─────────────────────────────────────────

  const handleHookMessage = (msg: Record<string, unknown>, socket: Socket): Promise<void> =>
    processHookMessage(
      {
        store, inbox, terminals, runEvent, waitForCapableTerminal, schedulePurge,
        markHookReceived: () => { hookEventReceived = true; },
      },
      msg,
      socket,
    );

  /** Send overview data to a connection (used by both push and pull modes). */
  const sendOverview = (conn: TerminalConnection): void => {
    terminals.sendTo(conn, {
      type: "overview.snapshot",
      projects: store.getProjectSummaries(),
    });
  };

  // ── Terminal message handler ─────────────────────────────────────

  const handleTerminalMessage = (conn: TerminalConnection, msg: ReturnType<typeof parseTerminalMessage>): void => {
    if (!msg) return;

    switch (msg.type) {
      case "hello": {
        const caps = (msg as Record<string, unknown>).capabilities;
        if (Array.isArray(caps)) {
          conn.capabilities = new Set(caps.filter((c): c is string => typeof c === "string"));
        }
        const clientVersion = helloProtocolVersion(msg);
        logMsg(`Terminal hello: capabilities=[${[...conn.capabilities].join(",")}] protocol=v${clientVersion}`);
        const mismatch = protocolMismatch(clientVersion);
        if (mismatch) {
          logMsg(`Protocol mismatch: client=v${mismatch.clientVersion} server=v${mismatch.serverVersion} caps=[${[...conn.capabilities].join(",")}] — ${mismatch.text}`, "warn");
          terminals.sendTo(conn, { type: "protocol.mismatch", ...mismatch });
        }
        break;
      }

      case "request.overview": {
        terminals.sendTo(conn, {
          type: "overview.snapshot",
          projects: store.getProjectSummaries(),
        });
        break;
      }

      case "poll": {
        // Pull mode: client requests current state
        sendOverview(conn);
        const items = inbox.all();
        // Send inbox-items while non-empty, AND once when it transitions to
        // empty, so read-only clients (menu bar) clear their attention
        // indicator promptly. Without the transition send, an emptied inbox
        // is never communicated via poll and the indicator lingers until the
        // next reconnect/resync. Avoid re-sending empty arrays every poll.
        if (shouldSendInboxOnPoll(items.length, conn.inboxWasNonEmpty)) {
          terminals.sendTo(conn, { type: "inbox-items", items });
        }
        conn.inboxWasNonEmpty = items.length > 0;
        pollSubscribedSessions({ store, terminals }, conn);
        logMsg(`Terminal poll: overview sent, ${inbox.all().length} inbox items, ${conn.subscribedSessions.size} subscribed sessions`);
        break;
      }

      case "request.session": {
        subscribeAndSnapshot({ store, terminals }, conn, msg.sessionId);
        break;
      }

      case "request.resync": {
        terminals.sendTo(conn, {
          type: "overview.snapshot",
          projects: store.getProjectSummaries(),
        });
        for (const sessionId of conn.subscribedSessions) {
          subscribeAndSnapshot({ store, terminals }, conn, sessionId);
        }
        terminals.sendTo(conn, { type: "inbox.snapshot", items: inbox.all() });
        logMsg(`Terminal resync: ${conn.subscribedSessions.size} sessions`);
        break;
      }

      case "request.unsubscribe": {
        // Fire-and-forget: drop both the subscription and the
        // incremental-pull cursor so the client stops receiving
        // signals for this session entirely. No reply is sent —
        // mirrors the other unidirectional `case` blocks that
        // don't echo a confirmation (e.g. `hint.session-dead`).
        conn.subscribedSessions.delete(msg.sessionId);
        conn.sessionSeqCursor.delete(msg.sessionId);
        break;
      }

      case "action.permission": {
        const { itemId, decision, message, updatedPermissions } = msg;
        // Pi extension_ui_request items: route the decision back to pi as
        // an extension_ui_response instead of writing to a hook socket.
        const piUi = pendingPiUIResponses.get(itemId);
        if (piUi) {
          const driver = piDrivers.get(piUi.sessionId);
          if (!driver) {
            logMsg(`action.permission for pi UI ${piUi.piRequestId}: no driver for session ${piUi.sessionId}`, "warn");
          } else {
            driver.sendExtensionUIResponse({
              id: piUi.piRequestId,
              confirmed: decision === "allow",
            });
          }
          pendingPiUIResponses.delete(itemId);
          inbox.remove(itemId);
          terminals.signalChanged("inbox");
          break;
        }
        Effect.runSync(inbox.respond(itemId, {
          hookSpecificOutput: {
            hookEventName: "PermissionRequest",
            decision: { behavior: decision, message, updatedPermissions },
          },
        }));
        terminals.signalChanged("inbox");
        break;
      }

      case "action.question": {
        const { itemId, answers } = msg;
        // Pi extension_ui_request items: send the first answer as
        // {value: <choice>} (pi's select dialog uses string values).
        const piUi = pendingPiUIResponses.get(itemId);
        if (piUi) {
          const driver = piDrivers.get(piUi.sessionId);
          if (!driver) {
            logMsg(`action.question for pi UI ${piUi.piRequestId}: no driver for session ${piUi.sessionId}`, "warn");
          } else {
            const value = answers[0];
            driver.sendExtensionUIResponse({
              id: piUi.piRequestId,
              ...(value !== undefined ? { value } : { cancelled: true }),
            });
          }
          pendingPiUIResponses.delete(itemId);
          inbox.remove(itemId);
          terminals.signalChanged("inbox");
          break;
        }
        const pending = inbox.getPending(itemId);
        const toolInput = (pending?.inboxItem.data?.tool_input as Record<string, unknown>) || {};
        const questions = (toolInput.questions as Array<Record<string, unknown>>) || [];

        const answersMap: Record<string, string> = {};
        questions.forEach((q, i) => {
          const qText = (q.question as string) || `question_${i}`;
          answersMap[qText] = answers[i] || answers[0] || "";
        });

        Effect.runSync(inbox.respond(itemId, {
          hookSpecificOutput: {
            hookEventName: "PreToolUse",
            permissionDecision: "allow",
            updatedInput: { ...toolInput, answers: answersMap },
          },
        }));
        terminals.signalChanged("inbox");
        break;
      }

      case "action.plan-review": {
        const { itemId, decision, feedback } = msg;
        let message: string | undefined;

        let normalizedFeedback: PlanFeedback | undefined;
        if (feedback) {
          if (typeof feedback === "string") {
            normalizedFeedback = {
              inlineComments: [],
              claudeMarkers: [],
              diff: null,
              generalComment: feedback,
            };
          } else {
            normalizedFeedback = feedback as PlanFeedback;
          }
        }

        if (normalizedFeedback) {
          const parts: string[] = ["# Plan Feedback\n"];
          if (normalizedFeedback.inlineComments?.length > 0) {
            parts.push("## Inline comments");
            normalizedFeedback.inlineComments.forEach((c) => {
              parts.push(`- Line ${c.line} (near "${c.nearText}"): ${c.comment}`);
            });
            parts.push("");
          }
          if (normalizedFeedback.claudeMarkers?.length > 0) {
            parts.push("## @claude markers");
            normalizedFeedback.claudeMarkers.forEach((m) => {
              parts.push(`- Line ${m.line} (near "${m.nearText}"): ${m.text}`);
            });
            parts.push("");
          }
          if (normalizedFeedback.diff) {
            parts.push("## Changes requested");
            parts.push(normalizedFeedback.diff);
            parts.push("");
          }
          if (normalizedFeedback.generalComment) {
            parts.push("## General comment");
            parts.push(normalizedFeedback.generalComment);
          }
          message = parts.join("\n");
        }

        Effect.runSync(inbox.respond(itemId, {
          hookSpecificOutput: {
            hookEventName: "PermissionRequest",
            decision: { behavior: decision, message },
          },
        }));
        terminals.signalChanged("inbox");
        break;
      }

      case "action.turn-auto-approve": {
        // TODO: implement turn-scoped auto-approve
        break;
      }

      case "hint.session-dead": {
        const { sessionId } = msg;
        const session = store.get(sessionId);
        if (session && session.status === "active") {
          const age = Date.now() - session.lastEventTime;
          if (age < HINT_RECENCY_GUARD_MS) {
            logMsg(`Terminal hint: session ${sessionId} ignored — last event ${Math.round(age / 1000)}s ago (< ${HINT_RECENCY_GUARD_MS / 1000}s)`, "warn");
            break;
          }
          logMsg(`Terminal hint: session ${sessionId} is dead — marking ended`);
          emitSessionPatches(store, terminals, sessionId, sessionEnd(session));
          schedulePurge(sessionId);
          terminals.signalChanged("overview");
        }
        break;
      }

      // ── Pi driver terminal messages ────────────────────────────────

      case "pi.start": {
        const m = msg as { cwd?: string; thinkingLevel?: string };
        const sessionId = startPiSession({
          cwd: m.cwd,
          thinkingLevel: m.thinkingLevel,
        });
        if (sessionId) {
          logMsg(`Pi session started via terminal: ${sessionId}`);
        } else {
          // startPiSession only fails for non-singleton reasons now (it no
          // longer rejects on an existing driver). Surface a generic error.
          logMsg(`Pi session start failed`, "warn");
          terminals.sendTo(conn, {
            type: "pi.session",
            sessionId: "",
            event: "rejected",
            reason: "Pi session start failed (see server log).",
          } as ServerMessage);
        }
        break;
      }

      case "pi.prompt": {
        const m = msg as { sessionId: string; text: string; images?: string[] };
        piSessionPrompt(m.sessionId, m.text, m.images);
        break;
      }

      case "pi.steer": {
        const m = msg as { sessionId: string; text: string };
        piSessionSteer(m.sessionId, m.text);
        break;
      }

      case "pi.abort": {
        const m = msg as { sessionId: string };
        piSessionAbort(m.sessionId);
        break;
      }

      case "pi.set-thinking": {
        const m = msg as { sessionId: string; level: string };
        piSessionSetThinking(m.sessionId, m.level);
        break;
      }

      case "pi.set-session-name": {
        const m = msg as { sessionId: string; name: string };
        piSessionSetSessionName(m.sessionId, m.name);
        break;
      }

      case "pi.set-model": {
        const m = msg as { sessionId: string; provider: string; modelId: string };
        piSessionSetModel(m.sessionId, m.provider, m.modelId);
        break;
      }

      case "pi.resume": {
        const m = msg as { sessionId?: string; sessionPath: string };
        piSessionResume(m.sessionPath, m.sessionId);
        break;
      }

      case "pi.compact": {
        const m = msg as { sessionId: string; customInstructions?: string };
        piSessionCompact(m.sessionId, m.customInstructions);
        break;
      }

      case "pi.new-session": {
        const m = msg as { sessionId: string };
        piSessionNewSession(m.sessionId);
        break;
      }

      case "pi.stop": {
        // Kill the targeted pi process entirely (vs pi.abort which only
        // interrupts the current LLM turn). The driver's onLifecycle("stop")
        // path removes it from piDrivers and broadcasts pi.session "stopped".
        const m = msg as { sessionId: string };
        stopPiSession(m.sessionId).catch((err) => logMsg(`pi.stop failed: ${err.message}`, "error"));
        break;
      }

      case "pi.refresh-commands": {
        const m = msg as { sessionId: string };
        refreshPiCommands(m.sessionId);
        break;
      }

      case "pi.refresh-models": {
        const m = msg as { sessionId: string };
        refreshPiModels(m.sessionId);
        break;
      }
    }
  };

  // ─��� Start hook server ────────────────────────────────────────────

  yield* fs.unlinkIfExists(config.hookSocketPath);
  yield* fs.mkdirp(dirname(config.hookSocketPath));

  const hookServer: Server = createServer((socket: Socket) => {
    let buffer = "";

    socket.on("data", (chunk: Buffer) => {
      buffer += chunk.toString();
      let newlineIdx: number;
      while ((newlineIdx = buffer.indexOf("\n")) !== -1) {
        const line = buffer.substring(0, newlineIdx).trim();
        buffer = buffer.substring(newlineIdx + 1);
        if (line.length === 0) continue;
        try {
          const msg = JSON.parse(line);
          // Pi envelope discriminator: lines with `src: "pi"` carry a raw
          // pi extension-event payload + attribution and bypass the
          // Claude-Code-hook message path entirely (no `needs_response`,
          // no bidirectional capability-wait, no socket reply). Every
          // other parsed line falls through to the existing hook handler.
          if (
            msg
            && typeof msg === "object"
            && (msg as Record<string, unknown>).src === "pi"
          ) {
            processPiEnvelope(msg as PiEventEnvelope);
            continue;
          }
          handleHookMessage(msg, socket).catch((e) =>
            logMsg(`Hook message handler error: ${e}`, "error"),
          );
        } catch (e) {
          logMsg(`Hook socket parse error: ${e}`, "error");
        }
      }
    });

    socket.on("error", (err) => {
      logMsg(`Hook socket connection error: ${err.message}`, "error");
    });

    socket.on("close", () => {
      const removed = inbox.removeBySocket(socket);
      for (const item of removed) {
        logMsg(`Inbox item ${item.id} (${item.type}) auto-removed: hook socket closed`);
        terminals.signalChanged("inbox");
      }
    });
  });

  hookServer.listen(config.hookSocketPath, () => {
    logMsg(`Hook socket listening on ${config.hookSocketPath}`);
  });

  // ── Start mermaid RPC server (TCP JSON-RPC on port 9876) ────────

  const mermaidServer = new MermaidRpcServer({ port: 9876, host: "127.0.0.1" });
  mermaidServer.start().catch((e: unknown) => {
    logMsg(`Mermaid RPC server failed to start: ${e}`, "warn");
  });

  // ── Start browser gateway (HTTP + WebSocket, in-process) ─────────
  //
  // An in-process HTTP+WS listener respawns on the same lifecycle as the
  // existing terminal Unix socket: `_ensure-server` tearing down
  // gravity-server (e.g. on plugin update) recreates both together, so
  // no orphan-listener risk and no extra ship path. The gateway holds
  // zero session state — it only proxies bytes to/from the terminal
  // socket — so the "no state, no isolation benefit" trade-off rules
  // out a sidecar for this class of server.
  const gatewayToken = randomBytes(24).toString("hex");
  let gateway: WsGateway | null = null;
  if (config.gatewayEnabled) {
    const clientHtml = CLIENT_HTML_TEMPLATE.replace("%%GRAVITY_TOKEN%%", gatewayToken);
    gateway = new WsGateway({
      host: config.gatewayHost,
      port: config.gatewayPort,
      terminalSocketPath: config.terminalSocketPath,
      token: gatewayToken,
      clientHtml,
      logFn: logMsg,
    });
    startGatewayNonFatal(gateway, config.gatewayHost, logMsg);
  }

  // ── Start terminal server ────────────────────────────────────────


  yield* fs.unlinkIfExists(config.terminalSocketPath);
  yield* fs.mkdirp(dirname(config.terminalSocketPath));

  const terminalServer: Server = createServer((socket: Socket) => {
    const conn = terminals.addConnection(socket);
    logMsg(`Terminal connected (total: ${terminals.connectionCount()})`);

    terminals.sendTo(conn, {
      type: "overview.snapshot",
      projects: store.getProjectSummaries(),
    });

    let buffer = "";

    socket.on("data", (chunk: Buffer) => {
      buffer += chunk.toString();
      let newlineIdx: number;
      while ((newlineIdx = buffer.indexOf("\n")) !== -1) {
        const line = buffer.substring(0, newlineIdx).trim();
        buffer = buffer.substring(newlineIdx + 1);
        if (line.length === 0) continue;

        try {
          const parsed = JSON.parse(line);
          if (typeof parsed === "object" && parsed !== null && isHookMessage(parsed)) {
            logMsg(`Hook event received on terminal socket — bridge may have wrong socket path (event=${parsed.event}, session=${parsed.session_id})`, "error");
            socket.destroy();
            return;
          }
        } catch {
          // JSON parse will also fail in parseTerminalMessage below
        }

        const msg = parseTerminalMessage(line);
        if (!msg) {
          logMsg(`Terminal: invalid message: ${line.substring(0, 100)}`, "warn");
          continue;
        }

        handleTerminalMessage(conn, msg);
      }
    });

    socket.on("close", () => {
      logMsg(`Terminal disconnected (total: ${terminals.connectionCount()})`);
    });

    socket.on("error", (err) => {
      logMsg(`Terminal socket error: ${err.message}`, "error");
    });
  });

  terminalServer.listen(config.terminalSocketPath, () => {
    logMsg(`Terminal socket listening on ${config.terminalSocketPath}`);
  });

  // ── Session health monitor ───────────────────────────────────────

  const serverStartedAt = Date.now();
  let lastHooksSilenceWarn = 0;
  let hookEventReceived = false;

  const healthCheckInterval = setInterval(() => {
    const now = Date.now();
    for (const session of store.all()) {
      if (session.status !== "active") continue;
      const sessionId = session.sessionId;
      const decision = evaluateSessionHealth(session, now, {
        staleThresholdMs: STALENESS_THRESHOLD_MS,
      });
      if (!decision.dead) continue;
      if (decision.reason === "pid-unreachable") {
        logMsg(`Health check: session ${sessionId} PID ${session.pid} is dead`);
      } else if (decision.reason === "stale") {
        logMsg(`Health check: session ${sessionId} stale (no events for ${Math.round((now - session.lastEventTime) / 1000)}s)`);
      }
      const patches = sessionEnd(session);
      if (patches.length > 0) {
        const stored = store.appendPatches(sessionId, patches);
        const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
        terminals.signalChanged("session", sessionId, seq);
      }
      schedulePurge(sessionId);
      terminals.signalChanged("overview");
    }

    // Hooks-silence warning: if terminals are connected but no hook
    // events have ever arrived, warn that the plugin may be disabled.
    if (
      !hookEventReceived &&
      store.all().length === 0 &&
      terminals.connectionCount() > 0 &&
      now - serverStartedAt > HOOKS_SILENCE_WARN_MS &&
      now - lastHooksSilenceWarn > HOOKS_SILENCE_REARM_MS
    ) {
      lastHooksSilenceWarn = now;
      const elapsed = Math.round((now - serverStartedAt) / 1000);
      const text = `No hook events received in ${elapsed}s — is the emacs-bridge plugin enabled? Check project .claude/settings.json for enabledPlugins overrides.`;
      logMsg(text, "warn");
      terminals.broadcast({ type: "notice", level: "warn", text });
    }
  }, HEALTH_CHECK_INTERVAL_MS);

  // ── PID file ─────────────────────────────────────────────────────

  yield* fs.mkdirp(dirname(config.pidFilePath));
  yield* fs.writeFile(config.pidFilePath, process.pid.toString());
  logMsg(`gravity-server ready (pid=${process.pid}, pidfile=${config.pidFilePath})`);

  // ── Install pi extension bundle ───────────────────
  //
  // Self-install the inlined emitter source into the configured
  // extensions directory so a `pi` process running alongside the
  // server can pick it up via its standard extension-loader path.
  // `installEmitter` is documented to never throw; the Effect
  // wrapper here exists purely to belt-and-suspender a hypothetical
  // future regression and to log the outcome. A failed install is a
  // warn-level event — the server keeps running.
  yield* Effect.tryPromise({
    try: () =>
      installEmitter({
        installDir: config.piExtensionInstallDir,
        source: PI_EMITTER_SOURCE,
      }),
    catch: (e) => ({
      action: "failed" as const,
      path: `${config.piExtensionInstallDir}/gravity-pi.js`,
      error: e instanceof Error ? e.message : String(e),
    }),
  }).pipe(
    Effect.tap((result) => {
      if (result.action === "failed") {
        logMsg(
          `pi extension install failed: ${result.error ?? "unknown"} (${result.path})`,
          "warn",
        );
      } else {
        logMsg(`pi extension ${result.action} at ${result.path}`);
      }
      return Effect.void;
    }),
  );

  // ── Shutdown ─────────────────────────────────────────────────────

  const shutdown = (): void => {
    logMsg("gravity-server shutting down...");
    clearInterval(healthCheckInterval);
    store.clearAllPurgeTimers();
    hookServer.close();
    terminalServer.close();
    if (gateway) gateway.stop();
    try { unlinkSync(config.hookSocketPath); } catch {}
    try { unlinkSync(config.terminalSocketPath); } catch {}
    try { unlinkSync(config.pidFilePath); } catch {}
  };

  process.on("SIGINT", () => { shutdown(); process.exit(0); });
  process.on("SIGTERM", () => { shutdown(); process.exit(0); });
});

// ── PID guard (runs before services start) ───────────────────────────

const pidGuard = Effect.gen(function* () {
  const config = yield* Effect.service(ServerConfig);
  const fs = yield* Effect.service(Fs);
  logMsg("gravity-server starting...");

  const pidExists = yield* fs.exists(config.pidFilePath);
  if (pidExists) {
    const content = yield* fs.readFile(config.pidFilePath).pipe(
      Effect.catch(() => Effect.succeed("")),
    );
    const existingPid = parseInt(content.trim(), 10);
    if (existingPid > 0 && existingPid !== process.pid) {
      try {
        process.kill(existingPid, 0);
        logMsg(`Another gravity-server is running (pid=${existingPid}). Exiting.`, "warn");
        process.exit(0);
      } catch {
        logMsg(`Stale PID file (pid=${existingPid} dead). Taking over.`);
      }
    }
  }
});

// ── Main ─────────────────────────────────────────────────────────────

const MainLive = Layer.mergeAll(
  ServerConfigLive,
  FsLive,
  SessionStoreLive,
  InboxLive,
  TerminalLive,
);

const main = Effect.gen(function* () {
  yield* pidGuard;
  yield* program;
});

// Only bootstrap the server when run as the entrypoint. Importing this
// module (e.g. the socket-free test harness for processHookMessage) must
// NOT start sockets / call process.exit.
const isEntrypoint =
  !!process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href;

if (isEntrypoint) {
  Effect.runPromise(Effect.provide(main, MainLive)).catch((e) => {
    logMsg(`Fatal error: ${e}`, "error");
    process.exit(1);
  });
}
