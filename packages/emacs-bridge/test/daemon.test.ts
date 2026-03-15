import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { createServer, Server, Socket, createConnection } from "net";
import { unlinkSync, existsSync } from "fs";
import { join } from "path";
import { PromptQueue, DaemonSession, SendEventFn, SendAndWaitFn } from "../src/daemon-session";
import { createDaemonHooks, createCanUseTool, clearAgents } from "../src/daemon-hooks";

// ─── Part 1: PromptQueue ────────────────────────────────────────────────────

describe("PromptQueue", () => {
  it("yields pushed messages immediately", async () => {
    const q = new PromptQueue();
    q.push("hello", "sess-1");

    const iter = q[Symbol.asyncIterator]();
    const { value, done } = await iter.next();
    expect(done).toBe(false);
    expect(value.type).toBe("user");
    expect(value.session_id).toBe("sess-1");
    expect(value.message.content[0].text).toBe("hello");
    q.close();
  });

  it("waits for push when queue is empty", async () => {
    const q = new PromptQueue();
    const iter = q[Symbol.asyncIterator]();

    // Push after a short delay
    setTimeout(() => q.push("delayed", "sess-1"), 10);

    const { value, done } = await iter.next();
    expect(done).toBe(false);
    expect(value.message.content[0].text).toBe("delayed");
    q.close();
  });

  it("yields multiple messages in order", async () => {
    const q = new PromptQueue();
    q.push("first", "sess-1");
    q.push("second", "sess-1");

    const iter = q[Symbol.asyncIterator]();
    const r1 = await iter.next();
    const r2 = await iter.next();
    expect(r1.value.message.content[0].text).toBe("first");
    expect(r2.value.message.content[0].text).toBe("second");
    q.close();
  });

  it("terminates iteration on close()", async () => {
    const q = new PromptQueue();
    const iter = q[Symbol.asyncIterator]();

    // Close after short delay
    setTimeout(() => q.close(), 10);

    const { done } = await iter.next();
    // The sentinel message causes the generator to return
    expect(done).toBe(true);
  });

  it("does not yield after close", async () => {
    const q = new PromptQueue();
    q.close();

    const results: any[] = [];
    for await (const msg of q) {
      results.push(msg);
    }
    expect(results).toHaveLength(0);
  });
});

// ─── Part 2: createDaemonHooks ──────────────────────────────────────────────

describe("createDaemonHooks", () => {
  let sentEvents: Array<{ event: string; payload: any }>;
  let sendEvent: SendEventFn;
  let sendAndWait: SendAndWaitFn;

  beforeEach(() => {
    sentEvents = [];
    sendEvent = async (eventName, _sid, _cwd, _pid, payload) => {
      sentEvents.push({ event: eventName, payload });
    };
    sendAndWait = async (eventName, _sid, _cwd, _pid, payload) => {
      sentEvents.push({ event: eventName, payload });
      return { hookSpecificOutput: { decision: { behavior: "allow" } } };
    };
  });

  it("creates hooks for all expected events", () => {
    const hooks = createDaemonHooks(sendEvent, sendAndWait, () => "sess-1", () => "/cwd");

    // SessionStart and SessionEnd are handled natively by daemon-session.ts
    // (not forwarded via hooks) to avoid duplicates with re-keying
    const expectedEvents = [
      "PreToolUse", "PostToolUse",
      "PostToolUseFailure", "SubagentStart", "SubagentStop",
      "UserPromptSubmit", "Stop", "Notification", "PermissionRequest",
    ];

    for (const event of expectedEvents) {
      expect(hooks[event as keyof typeof hooks]).toBeDefined();
      expect(hooks[event as keyof typeof hooks]![0].hooks).toHaveLength(1);
    }
  });

  it("fire-and-forget hooks send events and return empty object", async () => {
    const hooks = createDaemonHooks(sendEvent, sendAndWait, () => "sess-1", () => "/cwd");
    const cb = hooks["PreToolUse"]![0].hooks[0];

    const result = await cb(
      { tool_name: "Read", tool_input: { file_path: "/foo" } } as any,
      "tool-123",
      { signal: new AbortController().signal } as any,
    );

    expect(result).toEqual({});
    expect(sentEvents).toHaveLength(1);
    expect(sentEvents[0].event).toBe("PreToolUse");
    expect(sentEvents[0].payload.tool_name).toBe("Read");
    expect(sentEvents[0].payload.tool_use_id).toBe("tool-123");
    expect(sentEvents[0].payload.session_id).toBe("sess-1");
  });

  it("PermissionRequest is bidirectional", async () => {
    const hooks = createDaemonHooks(sendEvent, sendAndWait, () => "sess-1", () => "/cwd");
    const cb = hooks["PermissionRequest"]![0].hooks[0];

    const result = await cb(
      { tool_name: "Bash" } as any,
      "tool-456",
      { signal: new AbortController().signal } as any,
    );

    // sendAndWait returns our mock response
    expect(result).toEqual({ hookSpecificOutput: { decision: { behavior: "allow" } } });
    expect(sentEvents).toHaveLength(1);
    expect(sentEvents[0].event).toBe("PermissionRequest");
  });

  it("tracks agents on SubagentStart/SubagentStop", async () => {
    const sessionId = "sess-agent-test";
    const hooks = createDaemonHooks(sendEvent, sendAndWait, () => sessionId, () => "/cwd");

    const startCb = hooks["SubagentStart"]![0].hooks[0];
    const stopCb = hooks["SubagentStop"]![0].hooks[0];
    const ctx = { signal: new AbortController().signal } as any;

    await startCb({ agent_id: "agent-1" } as any, undefined, ctx);
    await startCb({ agent_id: "agent-2" } as any, undefined, ctx);

    expect(sentEvents).toHaveLength(2);

    await stopCb({ agent_id: "agent-1" } as any, undefined, ctx);

    expect(sentEvents).toHaveLength(3);

    // Clean up
    clearAgents(sessionId);
  });
});

// ─── Part 3: createCanUseTool ───────────────────────────────────────────────

describe("createCanUseTool", () => {
  it("returns allow when Emacs approves", async () => {
    const sendAndWait: SendAndWaitFn = async () => ({
      hookSpecificOutput: {
        decision: {
          behavior: "allow",
          updatedPermissions: [{ pattern: "Bash(*)" }],
        },
      },
    });

    const canUseTool = createCanUseTool(sendAndWait, () => "sess-1", () => "/cwd");

    const result = await canUseTool("Bash", { command: "ls" }, {
      signal: new AbortController().signal,
      suggestions: [],
      toolUseID: "tool-1",
      agentID: undefined,
      decisionReason: "user",
    } as any);

    expect(result.behavior).toBe("allow");
    if (result.behavior === "allow") {
      expect(result.updatedPermissions).toEqual([{ pattern: "Bash(*)" }]);
    }
  });

  it("returns deny when Emacs denies", async () => {
    const sendAndWait: SendAndWaitFn = async () => ({
      hookSpecificOutput: {
        decision: {
          behavior: "deny",
          message: "User said no",
        },
      },
    });

    const canUseTool = createCanUseTool(sendAndWait, () => "sess-1", () => "/cwd");

    const result = await canUseTool("Bash", { command: "rm -rf /" }, {
      signal: new AbortController().signal,
      suggestions: [],
      toolUseID: "tool-2",
      agentID: undefined,
      decisionReason: "user",
    } as any);

    expect(result.behavior).toBe("deny");
    if (result.behavior === "deny") {
      expect(result.message).toBe("User said no");
    }
  });

  it("returns deny when no response from Emacs", async () => {
    const sendAndWait: SendAndWaitFn = async () => null;

    const canUseTool = createCanUseTool(sendAndWait, () => "sess-1", () => "/cwd");

    const result = await canUseTool("Read", { file_path: "/etc/passwd" }, {
      signal: new AbortController().signal,
      suggestions: [],
      toolUseID: "tool-3",
      agentID: undefined,
      decisionReason: "user",
    } as any);

    expect(result.behavior).toBe("deny");
    if (result.behavior === "deny") {
      expect(result.message).toBe("No response from Emacs");
    }
  });
});

// ─── Part 4: Daemon command socket integration ──────────────────────────────

describe("Daemon command socket", () => {
  const SOCK_PATH = join(__dirname, "test-daemon-cmd.sock");
  let server: Server;

  // A minimal mock command handler
  function handleCommand(cmd: any): any {
    switch (cmd.cmd) {
      case "status":
        return { ok: true, data: [] };
      case "echo":
        return { ok: true, data: cmd };
      default:
        return { ok: false, error: `Unknown: ${cmd.cmd}` };
    }
  }

  beforeEach((ctx) => {
    if (existsSync(SOCK_PATH)) unlinkSync(SOCK_PATH);

    return new Promise<void>((resolve) => {
      server = createServer((client: Socket) => {
        let buffer = "";
        client.on("data", (chunk) => {
          buffer += chunk.toString();
          let idx: number;
          while ((idx = buffer.indexOf("\n")) >= 0) {
            const line = buffer.substring(0, idx).trim();
            buffer = buffer.substring(idx + 1);
            if (!line) continue;
            try {
              const cmd = JSON.parse(line);
              const response = handleCommand(cmd);
              client.write(JSON.stringify(response) + "\n");
            } catch (e: any) {
              client.write(JSON.stringify({ ok: false, error: e.message }) + "\n");
            }
          }
        });
      });
      server.listen(SOCK_PATH, resolve);
    });
  });

  afterEach(() => {
    return new Promise<void>((resolve) => {
      server.close(() => {
        if (existsSync(SOCK_PATH)) unlinkSync(SOCK_PATH);
        resolve();
      });
    });
  });

  function sendCommand(cmd: any): Promise<any> {
    return new Promise((resolve, reject) => {
      const client = createConnection(SOCK_PATH);
      let buffer = "";

      client.on("connect", () => {
        client.write(JSON.stringify(cmd) + "\n");
      });

      client.on("data", (chunk) => {
        buffer += chunk.toString();
        const idx = buffer.indexOf("\n");
        if (idx >= 0) {
          try {
            resolve(JSON.parse(buffer.substring(0, idx)));
          } catch (e) {
            reject(e);
          }
          client.end();
        }
      });

      client.on("error", reject);
    });
  }

  it("handles status command", async () => {
    const response = await sendCommand({ cmd: "status" });
    expect(response.ok).toBe(true);
    expect(response.data).toEqual([]);
  });

  it("handles echo command", async () => {
    const response = await sendCommand({ cmd: "echo", text: "hello" });
    expect(response.ok).toBe(true);
    expect(response.data.text).toBe("hello");
  });

  it("handles unknown command", async () => {
    const response = await sendCommand({ cmd: "bogus" });
    expect(response.ok).toBe(false);
    expect(response.error).toContain("Unknown");
  });

  it("handles multiple sequential commands", async () => {
    const r1 = await sendCommand({ cmd: "status" });
    const r2 = await sendCommand({ cmd: "echo", n: 1 });
    const r3 = await sendCommand({ cmd: "echo", n: 2 });

    expect(r1.ok).toBe(true);
    expect(r2.data.n).toBe(1);
    expect(r3.data.n).toBe(2);
  });

  it("handles invalid JSON gracefully", async () => {
    const response = await new Promise<any>((resolve, reject) => {
      const client = createConnection(SOCK_PATH);
      let buffer = "";

      client.on("connect", () => {
        client.write("not valid json\n");
      });

      client.on("data", (chunk) => {
        buffer += chunk.toString();
        const idx = buffer.indexOf("\n");
        if (idx >= 0) {
          try {
            resolve(JSON.parse(buffer.substring(0, idx)));
          } catch (e) {
            reject(e);
          }
          client.end();
        }
      });

      client.on("error", reject);
    });

    expect(response.ok).toBe(false);
    expect(response.error).toBeDefined();
  });
});

// ─── Part 5: DaemonSession unit tests ───────────────────────────────────────

describe("DaemonSession", () => {
  it("constructs with correct temp ID and session ID", () => {
    const session = new DaemonSession({
      id: "temp-123",
      cwd: "/project",
      sendEvent: async () => {},
      sendAndWait: async () => ({}),
    });

    expect(session.tempId).toBe("temp-123");
    expect(session.sessionId).toBe("temp-123"); // before real ID known
    expect(session.isRunning).toBe(false);
  });

  it("sendPrompt does nothing when not running", () => {
    const session = new DaemonSession({
      id: "temp-456",
      cwd: "/project",
      sendEvent: async () => {},
      sendAndWait: async () => ({}),
    });

    // Should not throw
    session.sendPrompt("hello");
    expect(session.isRunning).toBe(false);
  });

  it("stop() closes the session", () => {
    const session = new DaemonSession({
      id: "temp-789",
      cwd: "/project",
      sendEvent: async () => {},
      sendAndWait: async () => ({}),
    });

    // Should not throw
    session.stop();
    expect(session.isRunning).toBe(false);
  });
});
