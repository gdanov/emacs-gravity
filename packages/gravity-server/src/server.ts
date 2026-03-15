// gravity-server — Stateful backend for emacs-gravity
//
// Long-running process that:
// 1. Accepts hook events from bridge shims (hook socket)
// 2. Manages session state (turn tree, indexes, inbox)
// 3. Pushes view model updates to connected terminals (terminal socket)

import { createServer } from "net";
import type { Server, Socket } from "net";
import { existsSync, unlinkSync, mkdirSync } from "fs";
import { dirname, join } from "path";

import type { HookEventName, HookData, Patch, ServerMessage } from "@gravity/shared";
import { SessionStore } from "./state/session-store.js";
import { InboxManager } from "./state/inbox.js";
import { TerminalServer } from "./protocol/terminal-server.js";
import { parseTerminalMessage } from "./protocol/messages.js";
import { handleEvent } from "./handlers/event-handler.js";
import { log } from "./util/log.js";

// ── Configuration ────────────────────────────────────────────────────

const HOOK_SOCKET = process.env.GRAVITY_HOOK_SOCK
  ?? join(process.env.HOME || "/tmp", ".local", "state", "gravity-hooks.sock");

const TERMINAL_SOCKET = process.env.GRAVITY_TERMINAL_SOCK
  ?? join(process.env.HOME || "/tmp", ".local", "state", "gravity-terminal.sock");

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
    terminals.sendToSubscribers(sessionId, updateMsg);
  }

  // Broadcast overview on status-changing events only
  const overviewEvents = new Set(["SessionStart", "SessionEnd", "UserPromptSubmit", "Stop"]);
  if (overviewEvents.has(eventName)) {
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
  if (eventName === "PermissionRequest") {
    const items = inbox.all();
    if (items.length > 0) {
      terminals.broadcast({
        type: "inbox.added",
        item: items[0],
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

    case "action.permission": {
      const { itemId, decision, message } = msg;
      inbox.respond(itemId, {
        decision: { behavior: decision, message },
      });
      terminals.broadcast({ type: "inbox.removed", itemId });
      break;
    }

    case "action.question": {
      const { itemId, answers } = msg;
      inbox.respond(itemId, {
        answer: answers[0] || "",
      });
      terminals.broadcast({ type: "inbox.removed", itemId });
      break;
    }

    case "action.plan-review": {
      const { itemId, decision, feedback } = msg;
      let message: string | undefined;
      if (feedback) {
        // Build structured feedback message (matches Emacs plan review format)
        const parts: string[] = ["# Plan Feedback\n"];
        if (feedback.inlineComments.length > 0) {
          parts.push("## Inline comments");
          for (const c of feedback.inlineComments) {
            parts.push(`- Line ${c.line} (near "${c.nearText}"): ${c.comment}`);
          }
          parts.push("");
        }
        if (feedback.claudeMarkers.length > 0) {
          parts.push("## @claude markers");
          for (const m of feedback.claudeMarkers) {
            parts.push(`- Line ${m.line} (near "${m.nearText}"): ${m.text}`);
          }
          parts.push("");
        }
        if (feedback.diff) {
          parts.push("## Changes requested");
          parts.push(feedback.diff);
          parts.push("");
        }
        if (feedback.generalComment) {
          parts.push("## General comment");
          parts.push(feedback.generalComment);
        }
        message = parts.join("\n");
      }

      // DENY-AS-APPROVE workaround: Claude Code ignores ExitPlanMode "allow"
      // from PermissionRequest hooks (#15755). Convert allow → deny with message.
      const pending = inbox.getPending(itemId);
      const toolName = pending?.inboxItem.data?.tool_name;
      let finalDecision = decision;
      if (toolName === "ExitPlanMode" && decision === "allow") {
        finalDecision = "deny";
        message = message || "User approved the plan. Proceed with implementation.";
        log("Plan review: converting ExitPlanMode allow → deny-as-approve", "info");
      }

      inbox.respond(itemId, {
        decision: { behavior: finalDecision, message },
      });
      terminals.broadcast({ type: "inbox.removed", itemId });
      break;
    }

    case "action.turn-auto-approve": {
      // TODO: implement turn-scoped auto-approve
      break;
    }
  }
}

// ── Lifecycle ────────────────────────────────────────────────────────

let hookServer: Server;
let terminalServer: Server;

function start(): void {
  log("gravity-server starting...", "info");
  hookServer = startHookServer();
  terminalServer = startTerminalServer();
  log("gravity-server ready", "info");
}

function shutdown(): void {
  log("gravity-server shutting down...", "info");
  hookServer?.close();
  terminalServer?.close();
  try { unlinkSync(HOOK_SOCKET); } catch {}
  try { unlinkSync(TERMINAL_SOCKET); } catch {}
}

process.on("SIGINT", () => { shutdown(); process.exit(0); });
process.on("SIGTERM", () => { shutdown(); process.exit(0); });

start();
