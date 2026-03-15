import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { createServer, createConnection } from "net";
import type { Server, Socket } from "net";
import { existsSync, unlinkSync, mkdirSync } from "fs";
import { join } from "path";
import type { HookSocketMessage, ServerMessage, TerminalMessage } from "@gravity/shared";

// ── Helpers ──────────────────────────────────────────────────────────

const TMP = join(process.env.TMPDIR || "/tmp", `gravity-test-${process.pid}`);
const HOOK_SOCK = join(TMP, "hooks.sock");
const TERMINAL_SOCK = join(TMP, "terminal.sock");

/** Read newline-delimited JSON messages from a socket */
function readMessages(socket: Socket): Promise<ServerMessage[]> {
  return new Promise((resolve) => {
    const messages: ServerMessage[] = [];
    let buffer = "";
    socket.on("data", (chunk) => {
      buffer += chunk.toString();
      let idx: number;
      while ((idx = buffer.indexOf("\n")) !== -1) {
        const line = buffer.substring(0, idx).trim();
        buffer = buffer.substring(idx + 1);
        if (line.length > 0) {
          messages.push(JSON.parse(line));
        }
      }
    });
    socket.on("close", () => resolve(messages));
    socket.on("error", () => resolve(messages));
  });
}

/** Send a hook message and disconnect */
function sendHookEvent(msg: HookSocketMessage): Promise<void> {
  return new Promise((resolve, reject) => {
    const client = createConnection(HOOK_SOCK, () => {
      client.write(JSON.stringify(msg) + "\n");
      client.end();
    });
    client.on("close", () => resolve());
    client.on("error", (err) => reject(err));
  });
}

/** Connect as a terminal and collect messages for a duration */
function connectTerminal(durationMs = 200): Promise<{ messages: ServerMessage[]; socket: Socket }> {
  return new Promise((resolve, reject) => {
    const client = createConnection(TERMINAL_SOCK, () => {
      const msgPromise = readMessages(client);
      setTimeout(() => {
        client.end();
        msgPromise.then((messages) => resolve({ messages, socket: client }));
      }, durationMs);
    });
    client.on("error", (err) => reject(err));
  });
}

/** Send a terminal message */
function sendTerminalMessage(socket: Socket, msg: TerminalMessage): void {
  socket.write(JSON.stringify(msg) + "\n");
}

// ── Test suite ───────────────────────────────────────────────────────

describe("Integration: Server end-to-end", () => {
  let hookServer: Server;
  let terminalServer: Server;
  let cleanup: () => void;

  beforeEach(async () => {
    // Set up temp directory
    mkdirSync(TMP, { recursive: true });
    for (const sock of [HOOK_SOCK, TERMINAL_SOCK]) {
      if (existsSync(sock)) unlinkSync(sock);
    }

    // Import and start server components inline (avoid global state issues)
    // We'll create a minimal server setup here
    const { SessionStore } = await import("../src/state/session-store.js");
    const { InboxManager } = await import("../src/state/inbox.js");
    const { TerminalServer } = await import("../src/protocol/terminal-server.js");
    const { parseTerminalMessage } = await import("../src/protocol/messages.js");
    const { handleEvent } = await import("../src/handlers/event-handler.js");

    const store = new SessionStore();
    const inbox = new InboxManager();
    const terminals = new TerminalServer();

    hookServer = createServer((socket: Socket) => {
      let buf = "";
      socket.on("data", (chunk: Buffer) => {
        buf += chunk.toString();
        let idx: number;
        while ((idx = buf.indexOf("\n")) !== -1) {
          const line = buf.substring(0, idx).trim();
          buf = buf.substring(idx + 1);
          if (line.length === 0) continue;
          const msg = JSON.parse(line) as HookSocketMessage;
          const patches = handleEvent(
            msg.event, msg.session_id, msg.cwd, msg.data, msg.pid,
            { store, inbox }, msg.needs_response ? socket : undefined,
          );
          if (patches.length > 0) {
            terminals.sendToSubscribers(msg.session_id, {
              type: "session.update", sessionId: msg.session_id, patches,
            });
            terminals.broadcast({
              type: "overview.snapshot", projects: store.getProjectSummaries(),
            });
          }
          if (msg.event === "SessionStart") {
            const session = store.get(msg.session_id);
            if (session) {
              terminals.broadcast({
                type: "session.snapshot", sessionId: msg.session_id, session,
              });
            }
          }
        }
      });
    });

    terminalServer = createServer((socket: Socket) => {
      const conn = terminals.addConnection(socket);
      terminals.sendTo(conn, {
        type: "overview.snapshot", projects: store.getProjectSummaries(),
      });
      let buf = "";
      socket.on("data", (chunk: Buffer) => {
        buf += chunk.toString();
        let idx: number;
        while ((idx = buf.indexOf("\n")) !== -1) {
          const line = buf.substring(0, idx).trim();
          buf = buf.substring(idx + 1);
          if (line.length === 0) continue;
          const msg = parseTerminalMessage(line);
          if (!msg) continue;
          if (msg.type === "request.session") {
            const session = store.get(msg.sessionId);
            conn.subscribedSessions.add(msg.sessionId);
            if (session) {
              terminals.sendTo(conn, {
                type: "session.snapshot", sessionId: msg.sessionId, session,
              });
            }
          } else if (msg.type === "request.overview") {
            terminals.sendTo(conn, {
              type: "overview.snapshot", projects: store.getProjectSummaries(),
            });
          }
        }
      });
    });

    await new Promise<void>((resolve) => hookServer.listen(HOOK_SOCK, resolve));
    await new Promise<void>((resolve) => terminalServer.listen(TERMINAL_SOCK, resolve));

    cleanup = () => {
      hookServer?.close();
      terminalServer?.close();
      try { unlinkSync(HOOK_SOCK); } catch {}
      try { unlinkSync(TERMINAL_SOCK); } catch {}
    };
  });

  afterEach(() => {
    cleanup?.();
  });

  it("terminal receives overview on connect", async () => {
    const { messages } = await connectTerminal(100);
    expect(messages.length).toBeGreaterThan(0);
    expect(messages[0].type).toBe("overview.snapshot");
  });

  it("hook event creates session visible to terminal", async () => {
    // Send SessionStart via hook socket
    await sendHookEvent({
      event: "SessionStart",
      session_id: "test-s1",
      cwd: "/test/project",
      pid: 42,
      source: "bridge",
      data: { model: "claude-opus-4-6-20260301" },
      needs_response: false,
    });

    // Brief delay for processing
    await new Promise((r) => setTimeout(r, 50));

    // Connect as terminal — should see the session
    const { messages } = await connectTerminal(100);
    const overview = messages.find((m) => m.type === "overview.snapshot") as Extract<ServerMessage, { type: "overview.snapshot" }>;
    expect(overview).toBeDefined();
    expect(overview.projects.length).toBe(1);
    expect(overview.projects[0].sessions[0].sessionId).toBe("test-s1");
  });

  it("subscribed terminal receives patches for hook events", async () => {
    // Start session
    await sendHookEvent({
      event: "SessionStart",
      session_id: "test-s2",
      cwd: "/test/project",
      pid: 42,
      source: "bridge",
      data: {},
      needs_response: false,
    });
    await new Promise((r) => setTimeout(r, 50));

    // Connect terminal and subscribe to session
    const termSocket = createConnection(TERMINAL_SOCK);
    await new Promise<void>((resolve) => termSocket.on("connect", resolve));

    // Subscribe to session
    sendTerminalMessage(termSocket, {
      type: "request.session",
      sessionId: "test-s2",
    });
    await new Promise((r) => setTimeout(r, 50));

    // Collect messages from now on
    const collectedMessages: ServerMessage[] = [];
    let buf = "";
    termSocket.on("data", (chunk) => {
      buf += chunk.toString();
      let idx: number;
      while ((idx = buf.indexOf("\n")) !== -1) {
        const line = buf.substring(0, idx).trim();
        buf = buf.substring(idx + 1);
        if (line.length > 0) collectedMessages.push(JSON.parse(line));
      }
    });

    // Drain initial messages (overview + snapshot)
    await new Promise((r) => setTimeout(r, 100));
    collectedMessages.length = 0;

    // Send a tool event
    await sendHookEvent({
      event: "PreToolUse",
      session_id: "test-s2",
      cwd: "/test/project",
      pid: 42,
      source: "bridge",
      data: {
        tool_name: "Read",
        tool_use_id: "tool-1",
        tool_input: { file_path: "/src/index.ts" },
      },
      needs_response: false,
    });
    await new Promise((r) => setTimeout(r, 100));

    // Should have received session.update with add_tool patch
    const updates = collectedMessages.filter((m) => m.type === "session.update") as Extract<ServerMessage, { type: "session.update" }>[];
    expect(updates.length).toBeGreaterThan(0);
    const addToolPatch = updates.flatMap((u) => u.patches).find((p) => p.op === "add_tool");
    expect(addToolPatch).toBeDefined();

    termSocket.end();
  });

  it("full turn lifecycle produces correct patch sequence", async () => {
    // SessionStart
    await sendHookEvent({
      event: "SessionStart",
      session_id: "test-s3",
      cwd: "/test/project",
      pid: 42,
      source: "bridge",
      data: {},
      needs_response: false,
    });
    await new Promise((r) => setTimeout(r, 50));

    // Connect and subscribe
    const termSocket = createConnection(TERMINAL_SOCK);
    await new Promise<void>((resolve) => termSocket.on("connect", resolve));
    sendTerminalMessage(termSocket, { type: "request.session", sessionId: "test-s3" });
    await new Promise((r) => setTimeout(r, 100));

    const allPatches: string[] = [];
    let buf = "";
    termSocket.on("data", (chunk) => {
      buf += chunk.toString();
      let idx: number;
      while ((idx = buf.indexOf("\n")) !== -1) {
        const line = buf.substring(0, idx).trim();
        buf = buf.substring(idx + 1);
        if (line.length > 0) {
          const msg = JSON.parse(line) as ServerMessage;
          if (msg.type === "session.update") {
            for (const p of msg.patches) {
              allPatches.push(p.op);
            }
          }
        }
      }
    });
    // Drain
    await new Promise((r) => setTimeout(r, 100));
    allPatches.length = 0;

    // UserPromptSubmit
    await sendHookEvent({
      event: "UserPromptSubmit",
      session_id: "test-s3",
      cwd: "/test/project",
      pid: 42,
      source: "bridge",
      data: { prompt: "Fix the bug" },
      needs_response: false,
    });
    await new Promise((r) => setTimeout(r, 50));

    // PreToolUse
    await sendHookEvent({
      event: "PreToolUse",
      session_id: "test-s3",
      cwd: "/test/project",
      pid: 42,
      source: "bridge",
      data: { tool_name: "Read", tool_use_id: "t1", tool_input: { file_path: "/src/app.ts" } },
      needs_response: false,
    });
    await new Promise((r) => setTimeout(r, 50));

    // PostToolUse
    await sendHookEvent({
      event: "PostToolUse",
      session_id: "test-s3",
      cwd: "/test/project",
      pid: 42,
      source: "bridge",
      data: { tool_name: "Read", tool_use_id: "t1", tool_response: "file contents" },
      needs_response: false,
    });
    await new Promise((r) => setTimeout(r, 50));

    // Stop
    await sendHookEvent({
      event: "Stop",
      session_id: "test-s3",
      cwd: "/test/project",
      pid: 42,
      source: "bridge",
      data: {
        stop_text: "Fixed it",
        token_usage: { input_tokens: 100, output_tokens: 50, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 },
      },
      needs_response: false,
    });
    await new Promise((r) => setTimeout(r, 100));

    // Verify patch sequence
    expect(allPatches).toContain("freeze_turn");
    expect(allPatches).toContain("add_turn");
    expect(allPatches).toContain("add_prompt");
    expect(allPatches).toContain("set_claude_status");
    expect(allPatches).toContain("add_tool");
    expect(allPatches).toContain("complete_tool");
    expect(allPatches).toContain("set_turn_stop");
    expect(allPatches).toContain("set_token_usage");

    termSocket.end();
  });
});
