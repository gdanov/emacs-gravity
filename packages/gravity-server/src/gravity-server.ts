// gravity-server — Stateful backend for emacs-gravity
//
// Long-running process that:
// 1. Accepts hook events from bridge shims (hook socket)
// 2. Manages session state (turn tree, indexes, inbox)
// 3. Pushes view model updates to connected terminals (terminal socket)

import { createServer } from "net";
import type { Server, Socket } from "net";
import { existsSync, unlinkSync, mkdirSync, writeFileSync, readFileSync } from "fs";
import { dirname, join } from "path";

import type { HookEventName, HookData, Patch, ServerMessage, PlanFeedback } from "@gravity/shared";
import { SessionStore } from "./state/session-store.js";
import { InboxManager } from "./state/inbox.js";
import { TerminalServer } from "./protocol/terminal-server.js";
import { parseTerminalMessage, isHookMessage } from "./protocol/messages.js";
import { handleEvent } from "./handlers/event-handler.js";
import { sessionEnd } from "./state/session.js";
import { log } from "./util/log.js";

// ── Configuration ────────────────────────────────────────────────────

const HOOK_SOCKET = process.env.GRAVITY_HOOK_SOCK
  ?? join(process.env.HOME || "/tmp", ".local", "state", "gravity-hooks.sock");

const TERMINAL_SOCKET = process.env.GRAVITY_TERMINAL_SOCK
  ?? join(process.env.HOME || "/tmp", ".local", "state", "gravity-terminal.sock");

const PID_FILE = process.env.GRAVITY_PID_FILE
  ?? join(process.env.HOME || "/tmp", ".local", "state", "gravity-server.pid");

// ── State ────────────────────────────────────────────────────────────

const store = new SessionStore();
const inbox = new InboxManager();
const terminals = new TerminalServer();

// ── Hook Socket (bridge shims connect here) ──────────────────────────

function startHookServer(): Server {
  // Clean up stale socket
  if (existsSync(HOOK_SOCKET)) {
    try { unlinkSync(HOOK_SOCKET); } catch {}
  }
  mkdirSync(dirname(HOOK_SOCKET), { recursive: true });

  const server = createServer((socket: Socket) => {
    let buffer = "";

    socket.on("data", (chunk: Buffer) => {
      buffer += chunk.toString();

      // Process complete lines (newline-delimited JSON)
      let newlineIdx: number;
      while ((newlineIdx = buffer.indexOf("\n")) !== -1) {
        const line = buffer.substring(0, newlineIdx).trim();
        buffer = buffer.substring(newlineIdx + 1);

        if (line.length === 0) continue;

        try {
          const msg = JSON.parse(line);
          handleHookMessage(msg, socket);
        } catch (e) {
          log(`Hook socket parse error: ${e}`, "error");
        }
      }
    });

    socket.on("error", (err) => {
      log(`Hook socket connection error: ${err.message}`, "error");
    });

    socket.on("close", () => {
      // Bridge shim exited — permission was handled in TUI or bridge crashed.
      // Remove any inbox items that were waiting on this socket.
      const removed = inbox.removeBySocket(socket);
      for (const item of removed) {
        log(`Inbox item ${item.id} (${item.type}) auto-removed: hook socket closed`, "info");
        terminals.broadcast({ type: "inbox.removed", itemId: item.id });
      }
    });
  });

  server.listen(HOOK_SOCKET, () => {
    log(`Hook socket listening on ${HOOK_SOCKET}`, "info");
  });

  return server;
}

function handleHookMessage(msg: Record<string, unknown>, socket: Socket): void {
  const eventName = msg.event as HookEventName;
  const sessionId = (msg.session_id as string) || "unknown";
  const cwd = (msg.cwd as string) || "";
  const pid = (msg.pid as number) || null;
  const data = (msg.data as HookData) || {};
  const needsResponse = msg.needs_response === true;

  log(`Hook event: ${eventName} session=${sessionId}`, "info");

  // Reject bidirectional events immediately if no capable terminal is connected.
  // Without a terminal that can approve, the bridge would hang forever.
  const bidirectionalEvents = new Set(["PermissionRequest", "AskUserQuestionIntercept"]);
  if (needsResponse && bidirectionalEvents.has(eventName)) {
    if (!terminals.hasCapableTerminal("action.permission")) {
      log(`No capable terminal connected — rejecting ${eventName} immediately`, "warn");
      try {
        socket.write(JSON.stringify({ reason: "no_capable_terminal" }) + "\n");
        socket.end();
      } catch { /* socket may already be closed */ }
      return;
    }
  }

  // Clean up stale bidirectional inbox items before processing.
  // Any non-bidirectional event means Claude Code has moved past any pending
  // permission/question (e.g. user approved in TUI, then PostToolUse fires).
  if (!bidirectionalEvents.has(eventName)) {
    const staleRemoved = inbox.removeStaleForSession(sessionId);
    for (const item of staleRemoved) {
      log(`Inbox item ${item.id} (${item.type}) auto-removed: superseded by ${eventName}`, "info");
      terminals.broadcast({ type: "inbox.removed", itemId: item.id });
    }
  }

  const patches = handleEvent(
    eventName,
    sessionId,
    cwd,
    data,
    pid,
    { store, inbox },
    needsResponse ? socket : undefined,
  );

  // Send patches to subscribed terminals
  if (patches.length > 0) {
    const updateMsg: ServerMessage = {
      type: "session.update",
      sessionId,
      patches,
    };
    terminals.broadcast(updateMsg);
  }

  // Schedule purge for ended sessions, cancel if session self-heals
  const session = store.get(sessionId);
  if (session && session.status === "ended") {
    store.schedulePurge(sessionId, 2 * 60 * 1000, () => {
      store.delete(sessionId);
      inbox.removeForSession(sessionId);
      terminals.broadcast({ type: "session.removed", sessionId });
      terminals.unsubscribeAll(sessionId);
      terminals.broadcast({
        type: "overview.snapshot",
        projects: store.getProjectSummaries(),
      });
      log(`Purged ended session ${sessionId}`, "info");
    });
  } else if (session && session.status === "active") {
    store.cancelPurge(sessionId);
  }

  // Broadcast overview on status-changing events or when patches contain status ops
  const overviewEvents = new Set(["SessionStart", "SessionEnd", "UserPromptSubmit", "Stop", "PermissionRequest", "AskUserQuestionIntercept"]);
  const hasStatusPatch = patches.some(p =>
    p.op === "set_claude_status" || p.op === "set_status"
  );
  if (overviewEvents.has(eventName) || hasStatusPatch) {
    terminals.broadcast({
      type: "overview.snapshot",
      projects: store.getProjectSummaries(),
    });
  }

  // For new sessions, send snapshot to all terminals
  if (eventName === "SessionStart") {
    const session = store.get(sessionId);
    if (session) {
      terminals.broadcast({
        type: "session.snapshot",
        sessionId,
        session,
      });
    }
  }

  // For inbox events, broadcast to all terminals
  if (eventName === "PermissionRequest" || eventName === "AskUserQuestionIntercept") {
    const items = inbox.all();
    if (items.length > 0) {
      const item = items[0];
      log(`Inbox broadcast: type=${item.type} tool_name=${(item.data as Record<string, unknown>)?.tool_name} id=${item.id}`, "info");
      terminals.broadcast({
        type: "inbox.added",
        item,
      });
    }
  }

  // Fire-and-forget events: close the socket if no response needed
  if (!needsResponse) {
    // Don't close — the bridge manages its own socket lifecycle
  }
}

// ── Terminal Socket (Emacs/web/native connect here) ──────────────────

function startTerminalServer(): Server {
  if (existsSync(TERMINAL_SOCKET)) {
    try { unlinkSync(TERMINAL_SOCKET); } catch {}
  }
  mkdirSync(dirname(TERMINAL_SOCKET), { recursive: true });

  const server = createServer((socket: Socket) => {
    const conn = terminals.addConnection(socket);
    log(`Terminal connected (total: ${terminals.connectionCount})`, "info");

    // Send initial overview snapshot
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

        // Detect misrouted hook messages (bridge shim connected to terminal socket)
        try {
          const parsed = JSON.parse(line);
          if (typeof parsed === "object" && parsed !== null && isHookMessage(parsed)) {
            log(`Hook event received on terminal socket — bridge may have wrong socket path (event=${parsed.event}, session=${parsed.session_id})`, "error");
            socket.destroy();
            return;
          }
        } catch {
          // JSON parse will also fail in parseTerminalMessage below — let it handle
        }

        const msg = parseTerminalMessage(line);
        if (!msg) {
          log(`Terminal: invalid message: ${line.substring(0, 100)}`, "warn");
          continue;
        }

        handleTerminalMessage(conn, msg);
      }
    });

    socket.on("close", () => {
      log(`Terminal disconnected (total: ${terminals.connectionCount})`, "info");
    });

    socket.on("error", (err) => {
      log(`Terminal socket error: ${err.message}`, "error");
    });
  });

  server.listen(TERMINAL_SOCKET, () => {
    log(`Terminal socket listening on ${TERMINAL_SOCKET}`, "info");
  });

  return server;
}

function handleTerminalMessage(
  conn: ReturnType<TerminalServer["addConnection"]>,
  msg: ReturnType<typeof parseTerminalMessage>,
): void {
  if (!msg) return;

  switch (msg.type) {
    case "hello": {
      const caps = (msg as Record<string, unknown>).capabilities;
      if (Array.isArray(caps)) {
        conn.capabilities = new Set(caps.filter((c): c is string => typeof c === "string"));
      }
      log(`Terminal hello: capabilities=[${[...conn.capabilities].join(",")}]`, "info");
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
        terminals.sendTo(conn, {
          type: "session.snapshot",
          sessionId: msg.sessionId,
          session,
        });
      }
      break;
    }

    case "request.resync": {
      // Overview
      terminals.sendTo(conn, {
        type: "overview.snapshot",
        projects: store.getProjectSummaries(),
      });
      // Snapshots for all subscribed sessions
      for (const sessionId of conn.subscribedSessions) {
        const session = store.get(sessionId);
        if (session) {
          terminals.sendTo(conn, {
            type: "session.snapshot",
            sessionId,
            session,
          });
        }
      }
      // Inbox snapshot
      terminals.sendTo(conn, {
        type: "inbox.snapshot",
        items: inbox.all(),
      });
      log(`Terminal resync: ${conn.subscribedSessions.size} sessions`, "info");
      break;
    }

    case "action.permission": {
      const { itemId, decision, message, updatedPermissions } = msg;
      // Write the full hookSpecificOutput format — the bridge writes it directly to stdout
      inbox.respond(itemId, {
        hookSpecificOutput: {
          hookEventName: "PermissionRequest",
          decision: { behavior: decision, message, updatedPermissions },
        },
      });
      terminals.broadcast({ type: "inbox.removed", itemId });
      break;
    }

    case "action.question": {
      const { itemId, answers } = msg;
      // Write the full hookSpecificOutput format — the bridge writes it directly to stdout
      // AskUserQuestionIntercept is sent as PreToolUse event to Claude Code
      inbox.respond(itemId, {
        hookSpecificOutput: {
          hookEventName: "PreToolUse",
          permissionDecision: "deny",
          permissionDecisionReason: `User answered: ${answers[0] || ""}`,
        },
        answer: answers[0] || "",
        answers,
      });
      terminals.broadcast({ type: "inbox.removed", itemId });
      break;
    }

    case "action.plan-review": {
      const { itemId, decision, feedback } = msg;
      let message: string | undefined;

      // Normalize feedback to structured format
      let normalizedFeedback: PlanFeedback | undefined;
      if (feedback) {
        if (typeof feedback === 'string') {
          // Legacy format: feedback is a pre-formatted string (from Emacs deny)
          normalizedFeedback = {
            inlineComments: [],
            claudeMarkers: [],
            diff: null,
            generalComment: feedback,
          };
        } else {
          // New format: structured feedback object
          normalizedFeedback = feedback as PlanFeedback;
        }
      }

      // Build message from normalized feedback
      if (normalizedFeedback) {
        // Build structured feedback message (matches Emacs plan review format)
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

      // Write the full hookSpecificOutput format — the bridge writes it directly to stdout
      inbox.respond(itemId, {
        hookSpecificOutput: {
          hookEventName: "PermissionRequest",
          decision: { behavior: decision, message },
        },
      });
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
        log(`Terminal hint: session ${sessionId} is dead — marking ended`, "info");
        const patches = sessionEnd(session);
        if (patches.length > 0) {
          terminals.broadcast({ type: "session.update", sessionId, patches });
        }
        // Schedule purge (same as SessionEnd hook flow)
        store.schedulePurge(sessionId, 2 * 60 * 1000, () => {
          store.delete(sessionId);
          inbox.removeForSession(sessionId);
          terminals.broadcast({ type: "session.removed", sessionId });
          terminals.unsubscribeAll(sessionId);
          terminals.broadcast({
            type: "overview.snapshot",
            projects: store.getProjectSummaries(),
          });
          log(`Purged ended session ${sessionId}`, "info");
        });
        terminals.broadcast({
          type: "overview.snapshot",
          projects: store.getProjectSummaries(),
        });
      }
      break;
    }
  }
}

// ── Lifecycle ────────────────────────────────────────────────────────

let hookServer: Server;
let terminalServer: Server;

function start(): void {
  log("gravity-server starting...", "info");

  // Guard: refuse to start if another instance is already running
  if (existsSync(PID_FILE)) {
    try {
      const existingPid = parseInt(readFileSync(PID_FILE, "utf-8").trim(), 10);
      if (existingPid > 0 && existingPid !== process.pid) {
        try {
          process.kill(existingPid, 0); // test if alive (throws if dead)
          log(`Another gravity-server is running (pid=${existingPid}). Exiting.`, "warn");
          process.exit(0);
        } catch {
          // Process dead — stale PID file, proceed with cleanup
          log(`Stale PID file (pid=${existingPid} dead). Taking over.`, "info");
        }
      }
    } catch {
      // PID file unreadable/corrupt — ignore, proceed
    }
  }

  hookServer = startHookServer();
  terminalServer = startTerminalServer();
  mkdirSync(dirname(PID_FILE), { recursive: true });
  writeFileSync(PID_FILE, process.pid.toString());
  log(`gravity-server ready (pid=${process.pid}, pidfile=${PID_FILE})`, "info");
}

function shutdown(): void {
  log("gravity-server shutting down...", "info");
  store.clearAllPurgeTimers();
  hookServer?.close();
  terminalServer?.close();
  try { unlinkSync(HOOK_SOCKET); } catch {}
  try { unlinkSync(TERMINAL_SOCKET); } catch {}
  try { unlinkSync(PID_FILE); } catch {}
}

process.on("SIGINT", () => { shutdown(); process.exit(0); });
process.on("SIGTERM", () => { shutdown(); process.exit(0); });

start();
