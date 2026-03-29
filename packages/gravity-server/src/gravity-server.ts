// gravity-server — Stateful backend for emacs-gravity
//
// Long-running process that:
// 1. Accepts hook events from bridge shims (hook socket)
// 2. Manages session state (turn tree, indexes, inbox)
// 3. Pushes view model updates to connected terminals (terminal socket)

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

// ── Constants ────────────────────────────────────────────────────────

const CAPABILITY_WAIT_MS = 10_000;
const CAPABILITY_POLL_MS = 500;
const PURGE_DELAY_MS = 2 * 60 * 1000;

const BIDIRECTIONAL_EVENTS: ReadonlySet<HookEventName> = new Set(["PermissionRequest", "AskUserQuestionIntercept"]);
const OVERVIEW_EVENTS: ReadonlySet<HookEventName> = new Set(["SessionStart", "SessionEnd", "UserPromptSubmit", "Stop", "PermissionRequest", "AskUserQuestionIntercept"]);

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

  // ── Hook message handler ─────────────────────────────────────────

  const handleHookMessage = async (msg: Record<string, unknown>, socket: Socket): Promise<void> => {
    const eventName = msg.event as HookEventName;
    const sessionId = (msg.session_id as string) || "unknown";
    const cwd = (msg.cwd as string) || "";
    const pid = (msg.pid as number) || null;
    const data = (msg.data as HookData) || {};
    const needsResponse = msg.needs_response === true;

    logMsg(`Hook event: ${eventName} session=${sessionId}`);

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
      terminals.broadcast({ type: "session.update", sessionId, patches } as ServerMessage);
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
      terminals.broadcast({
        type: "overview.snapshot",
        projects: store.getProjectSummaries(),
      });
    }

    if (eventName === "SessionStart") {
      const session = store.get(sessionId);
      if (session) {
        terminals.broadcast({ type: "session.snapshot", sessionId, session });
      }
    }

    if (eventName === "PermissionRequest" || eventName === "AskUserQuestionIntercept") {
      const items = inbox.all();
      if (items.length > 0) {
        const item = items[0];
        logMsg(`Inbox broadcast: type=${item.type} tool_name=${(item.data as Record<string, unknown>)?.tool_name} id=${item.id}`);
        terminals.broadcast({ type: "inbox.added", item });
      }
    }
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

  // ── PID file ─────────────────────────────────────────────────────

  yield* fs.mkdirp(dirname(config.pidFilePath));
  yield* fs.writeFile(config.pidFilePath, process.pid.toString());
  logMsg(`gravity-server ready (pid=${process.pid}, pidfile=${config.pidFilePath})`);

  // ── Shutdown ─────────────────────────────────────────────────────

  const shutdown = (): void => {
    logMsg("gravity-server shutting down...");
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
