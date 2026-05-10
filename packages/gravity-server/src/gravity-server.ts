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
import { Effect, Layer } from "effect";

import type { HookEventName, HookData, Patch, ServerMessage, PlanFeedback } from "@gravity/shared";
import { parseTerminalMessage, isHookMessage } from "./protocol/messages.js";
import { handleEvent } from "./handlers/event-handler.js";
import { sessionEnd } from "./state/session.js";

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
import { startPiDriver, type StartPiDriverOptions } from "./pi-driver/index.js";

// ── Constants ────────────────────────────────────────────────────────

const CAPABILITY_WAIT_MS = 10_000;
const CAPABILITY_POLL_MS = 500;
const PURGE_DELAY_MS = 2 * 60 * 1000;
const HEALTH_CHECK_INTERVAL_MS = 30_000;
const STALENESS_THRESHOLD_MS = 5 * 60 * 1000;
const HINT_RECENCY_GUARD_MS = 30_000;
const HOOKS_SILENCE_WARN_MS = 90_000;
const HOOKS_SILENCE_REARM_MS = 600_000;

const BIDIRECTIONAL_EVENTS: ReadonlySet<HookEventName> = new Set(["PermissionRequest", "AskUserQuestionIntercept"]);
const OVERVIEW_EVENTS: ReadonlySet<HookEventName> = new Set(["SessionStart", "SessionEnd", "UserPromptSubmit", "Stop", "PermissionRequest", "AskUserQuestionIntercept"]);

// Pull mode: server sends lightweight signals instead of full payloads
// Default: true. Set to false via GRAVITY_PUSH_MODE=true to use push (legacy).
const PULL_MODE = process.env.GRAVITY_PUSH_MODE !== "true";

// ── Logging helper (simple, no service dependency for socket callbacks) ──

function logMsg(message: string, level: string = "info"): void {
  const ts = new Date().toISOString();
  try {
    process.stderr.write(`[${ts}] [${level}] ${message}\n`);
  } catch { /* best effort */ }
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
  const handlePiTranslation = (result: { hookEvent: HookEventName; hookData: HookData }): void => {
    const sessionId = result.hookData.session_id as string || "pi-session";
    const cwd = result.hookData.cwd as string || config.piCwd || process.cwd();

    logMsg(`Pi driver event: ${result.hookEvent} session=${sessionId}`);
    const patches = runEvent(result.hookEvent, sessionId, cwd, result.hookData, null);

    if (patches.length > 0) {
      if (PULL_MODE) {
        const stored = store.appendPatches(sessionId, patches);
        const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
        terminals.signalChanged("session", sessionId, seq);
      } else {
        terminals.broadcast({ type: "session.update", sessionId, patches } as ServerMessage);
      }
    }

    // Handle session lifecycle for overview updates
    const hasStatusPatch = patches.some(p =>
      p.op === "set_claude_status" || p.op === "set_status"
    );
    if (OVERVIEW_EVENTS.has(result.hookEvent) || hasStatusPatch) {
      if (PULL_MODE) {
        terminals.signalChanged("overview");
      } else {
        terminals.broadcast({
          type: "overview.snapshot",
          projects: store.getProjectSummaries(),
        });
      }
    }

    // Schedule purge for ended sessions
    const session = store.get(sessionId);
    if (session && session.status === "ended") {
      schedulePurge(sessionId);
    } else if (session && session.status === "active") {
      store.cancelPurge(sessionId);
    }
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
      store.delete(sessionId);
      inbox.removeForSession(sessionId);
      terminals.broadcast({ type: "session.removed", sessionId });
      terminals.unsubscribeAll(sessionId);
      terminals.broadcast({
        type: "overview.snapshot",
        projects: store.getProjectSummaries(),
      });
      logMsg(`Purged ended session ${sessionId}`);
    });
  };

  // ── Pi driver session management ───────────────────────────────

  // Mutable reference to the active pi driver (null if not running)
  let activePiDriver: ReturnType<typeof startPiDriver> | null = null;

  /** Start a new pi session (called via terminal message). */
  const startPiSession = (options: { cwd?: string; thinkingLevel?: string }): string | null => {
    if (activePiDriver) {
      logMsg(`Pi session already running — use pi.abort first`, "warn");
      return null;
    }

    const sessionId = generateSessionId();
    logMsg(`Starting pi session ${sessionId} (cwd=${options.cwd ?? process.cwd()}, thinking=${options.thinkingLevel ?? "medium"})`);

    // Broadcast pi.session.started for Emacs
    terminals.broadcast({
      type: "pi.session",
      sessionId,
      event: "started",
      cwd: options.cwd ?? process.cwd(),
    } as ServerMessage);


    const driver = startPiDriver({
      cwd: options.cwd ?? process.cwd(),
      thinkingLevel: (options.thinkingLevel as any) ?? "medium",
      onTranslation: (result) => {
        // Route translation to handlePiTranslation
        handlePiTranslation(result);
      },
      onLifecycle: (event, metadata) => {
        if (event === "start") {
          logMsg(`Pi session started: ${metadata?.sessionId}`);
        } else if (event === "stop") {
          logMsg(`Pi session ended: ${metadata?.sessionId}`);
          // Broadcast pi.session.stopped for Emacs
          terminals.broadcast({
            type: "pi.session",
            sessionId: metadata?.sessionId ?? sessionId,
            event: "stopped",
          } as ServerMessage);
          activePiDriver = null;
        } else if (event === "error") {
          logMsg(`Pi session error`, "error");
          // Broadcast pi.session.stopped on error too
          terminals.broadcast({
            type: "pi.session",
            sessionId: metadata?.sessionId ?? sessionId,
            event: "stopped",
          } as ServerMessage);
          activePiDriver = null;
        }
      },
    });

    activePiDriver = driver;
    return sessionId;
  };

  /** Send a prompt to the active pi session. */
  const piSessionPrompt = (text: string, images?: string[]): void => {
    if (!activePiDriver) {
      logMsg(`No active pi session`, "warn");
      return;
    }
    activePiDriver.prompt(text, images).catch((err) => {
      logMsg(`pi.prompt error: ${err.message}`, "error");
    });
  };

  /** Send steering message to active pi session. */
  const piSessionSteer = (text: string): void => {
    if (!activePiDriver) {
      logMsg(`No active pi session`, "warn");
      return;
    }
    activePiDriver.steer(text);
  };

  /** Abort the active pi session. */
  const piSessionAbort = (): void => {
    if (!activePiDriver) {
      logMsg(`No active pi session to abort`, "warn");
      return;
    }
    activePiDriver.abort();
  };

  /** Set thinking level for active pi session. */
  const piSessionSetThinking = (level: string): void => {
    if (!activePiDriver) {
      logMsg(`No active pi session`, "warn");
      return;
    }
    activePiDriver.setEffortLevel(level);
  };

  /** Stop the active pi session. */
  const stopPiSession = async (): Promise<void> => {
    if (!activePiDriver) {
      return;
    }
    await activePiDriver.stop();
    activePiDriver = null;
  };

  // ── Terminal message handlers for pi ──────────────────────────────

  // Expose pi session functions to terminal message handler
  (program as any)._startPiSession = startPiSession;
  (program as any)._piSessionPrompt = piSessionPrompt;
  (program as any)._piSessionSteer = piSessionSteer;
  (program as any)._piSessionAbort = piSessionAbort;
  (program as any)._piSessionSetThinking = piSessionSetThinking;
  (program as any)._stopPiSession = stopPiSession;

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

  const handleHookMessage = async (msg: Record<string, unknown>, socket: Socket): Promise<void> => {
    const eventName = msg.event as HookEventName;
    const sessionId = (msg.session_id as string) || "unknown";
    const cwd = (msg.cwd as string) || "";
    const pid = (msg.pid as number) || null;
    const data = (msg.data as HookData) || {};
    const needsResponse = msg.needs_response === true;

    logMsg(`Hook event: ${eventName} session=${sessionId}`);
    hookEventReceived = true;

    // Reject bidirectional events if no capable terminal is connected
    if (needsResponse && BIDIRECTIONAL_EVENTS.has(eventName)) {
      if (!terminals.hasCapableTerminal("action.permission")) {
        logMsg(`No capable terminal connected — waiting up to ${CAPABILITY_WAIT_MS}ms for reconnect`, "warn");
        const arrived = await waitForCapableTerminal("action.permission", CAPABILITY_WAIT_MS);
        if (!arrived) {
          logMsg(`No capable terminal after ${CAPABILITY_WAIT_MS}ms — rejecting ${eventName}`, "warn");
          try {
            socket.write(JSON.stringify({ reason: "no_capable_terminal" }) + "\n");
            socket.end();
          } catch { /* socket may already be closed */ }
          return;
        }
        logMsg(`Capable terminal connected during wait — proceeding with ${eventName}`);
      }
    }

    // Clean up stale bidirectional inbox items before processing
    if (!BIDIRECTIONAL_EVENTS.has(eventName)) {
      const staleRemoved = inbox.removeStaleForSession(sessionId);
      for (const item of staleRemoved) {
        logMsg(`Inbox item ${item.id} (${item.type}) auto-removed: superseded by ${eventName}`);
        terminals.broadcast({ type: "inbox.removed", itemId: item.id });
      }

      if (eventName !== "Notification") {
        const forceClosed = inbox.forceCloseStaleForSession(sessionId);
        for (const item of forceClosed) {
          logMsg(`Inbox item ${item.id} (${item.type}) force-closed: superseded by ${eventName}`);
          terminals.broadcast({ type: "inbox.removed", itemId: item.id });
        }
      }
    }

    const patches = runEvent(eventName, sessionId, cwd, data, pid, needsResponse ? socket : undefined);

    if (patches.length > 0) {
      if (PULL_MODE) {
        // Pull mode: store patches and signal, don't broadcast
        const stored = store.appendPatches(sessionId, patches);
        const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
        terminals.signalChanged("session", sessionId, seq);
      } else {
        // Push mode (default): broadcast full patches
        terminals.broadcast({ type: "session.update", sessionId, patches } as ServerMessage);
      }
    }

    // Schedule purge for ended sessions, cancel if session self-heals
    const session = store.get(sessionId);
    if (session && session.status === "ended") {
      schedulePurge(sessionId);
    } else if (session && session.status === "active") {
      store.cancelPurge(sessionId);
    }

    const hasStatusPatch = patches.some(p =>
      p.op === "set_claude_status" || p.op === "set_status"
    );
    if (OVERVIEW_EVENTS.has(eventName) || hasStatusPatch) {
      if (PULL_MODE) {
        terminals.signalChanged("overview");
      } else {
        terminals.broadcast({
          type: "overview.snapshot",
          projects: store.getProjectSummaries(),
        });
      }
    }

    if (eventName === "SessionStart") {
      const session = store.get(sessionId);
      if (session) {
        if (PULL_MODE) {
          terminals.signalChanged("session", sessionId, store.getSessionSeq(sessionId));
        } else {
          terminals.broadcast({ type: "session.snapshot", sessionId, session });
        }
      }
    }

    if (eventName === "PermissionRequest" || eventName === "AskUserQuestionIntercept") {
      const items = inbox.all();
      if (items.length > 0) {
        const item = items[0];
        logMsg(`Inbox broadcast: type=${item.type} tool_name=${(item.data as Record<string, unknown>)?.tool_name} id=${item.id}`);
        if (PULL_MODE) {
          terminals.signalChanged("inbox");
        } else {
          terminals.broadcast({ type: "inbox.added", item });
        }
      }
    }
  };

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
        logMsg(`Terminal hello: capabilities=[${[...conn.capabilities].join(",")}]`);
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
        if (items.length > 0) {
          terminals.sendTo(conn, { type: "inbox-items", items });
        }
        for (const sessionId of conn.subscribedSessions) {
          const session = store.get(sessionId);
          if (session) {
            const patches = store.getPatchesSince(sessionId, 0);
            const seq = store.getSessionSeq(sessionId);
            if (patches.length > 0) {
              terminals.sendTo(conn, {
                type: "session-patches",
                sessionId,
                seq,
                patches: patches.map(p => p.patch),
              });
            }
          }
        }
        logMsg(`Terminal poll: overview sent, ${inbox.all().length} inbox items, ${conn.subscribedSessions.size} subscribed sessions`);
        break;
      }

      case "request.session": {
        const session = store.get(msg.sessionId);
        conn.subscribedSessions.add(msg.sessionId);
        if (session) {
          terminals.sendTo(conn, { type: "session.snapshot", sessionId: msg.sessionId, session });
        }
        break;
      }

      case "request.resync": {
        terminals.sendTo(conn, {
          type: "overview.snapshot",
          projects: store.getProjectSummaries(),
        });
        for (const sessionId of conn.subscribedSessions) {
          const session = store.get(sessionId);
          if (session) {
            terminals.sendTo(conn, { type: "session.snapshot", sessionId, session });
          }
        }
        terminals.sendTo(conn, { type: "inbox.snapshot", items: inbox.all() });
        logMsg(`Terminal resync: ${conn.subscribedSessions.size} sessions`);
        break;
      }

      case "action.permission": {
        const { itemId, decision, message, updatedPermissions } = msg;
        Effect.runSync(inbox.respond(itemId, {
          hookSpecificOutput: {
            hookEventName: "PermissionRequest",
            decision: { behavior: decision, message, updatedPermissions },
          },
        }));
        terminals.broadcast({ type: "inbox.removed", itemId });
        break;
      }

      case "action.question": {
        const { itemId, answers } = msg;
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
        terminals.broadcast({ type: "inbox.removed", itemId });
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
        terminals.broadcast({ type: "inbox.removed", itemId });
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
          const patches = sessionEnd(session);
          if (patches.length > 0) {
            terminals.broadcast({ type: "session.update", sessionId, patches });
          }
          schedulePurge(sessionId);
          terminals.broadcast({
            type: "overview.snapshot",
            projects: store.getProjectSummaries(),
          });
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
          logMsg(`Pi session start failed — already running?`, "warn");
        }
        break;
      }

      case "pi.prompt": {
        const m = msg as { text: string; images?: string[] };
        piSessionPrompt(m.text, m.images);
        break;
      }

      case "pi.steer": {
        const m = msg as { text: string };
        piSessionSteer(m.text);
        break;
      }

      case "pi.abort": {
        piSessionAbort();
        break;
      }

      case "pi.set-thinking": {
        const m = msg as { level: string };
        piSessionSetThinking(m.level);
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
        terminals.broadcast({ type: "inbox.removed", itemId: item.id });
      }
    });
  });

  hookServer.listen(config.hookSocketPath, () => {
    logMsg(`Hook socket listening on ${config.hookSocketPath}`);
  });

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
      let isDead = false;

      if (session.pid && session.pid > 0) {
        try {
          process.kill(session.pid, 0);
        } catch {
          isDead = true;
          logMsg(`Health check: session ${sessionId} PID ${session.pid} is dead`);
        }
      } else if (now - session.lastEventTime > STALENESS_THRESHOLD_MS) {
        isDead = true;
        logMsg(`Health check: session ${sessionId} stale (no events for ${Math.round((now - session.lastEventTime) / 1000)}s)`);
      }

      if (isDead) {
        const patches = sessionEnd(session);
        if (patches.length > 0) {
          if (PULL_MODE) {
            const stored = store.appendPatches(sessionId, patches);
            const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
            terminals.signalChanged("session", sessionId, seq);
          } else {
            terminals.broadcast({ type: "session.update", sessionId, patches });
          }
        }
        schedulePurge(sessionId);
        if (PULL_MODE) {
          terminals.signalChanged("overview");
        } else {
          terminals.broadcast({
            type: "overview.snapshot",
            projects: store.getProjectSummaries(),
          });
        }
      }
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

  // ── Shutdown ─────────────────────────────────────────────────────

  const shutdown = (): void => {
    logMsg("gravity-server shutting down...");
    clearInterval(healthCheckInterval);
    store.clearAllPurgeTimers();
    hookServer.close();
    terminalServer.close();
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

Effect.runPromise(Effect.provide(main, MainLive)).catch((e) => {
  logMsg(`Fatal error: ${e}`, "error");
  process.exit(1);
});
