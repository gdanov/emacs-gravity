import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { createServer, createConnection } from "net";
import type { Server, Socket } from "net";
import { existsSync, unlinkSync, mkdirSync } from "fs";
import { join } from "path";
import type { HookSocketMessage, ServerMessage, Patch, HookEventName, HookData } from "@gravity/shared";

// ── Helpers ──────────────────────────────────────────────────────────

const TMP = join(process.env.TMPDIR || "/tmp", `gravity-scenarios-${process.pid}`);
const HOOK_SOCK = join(TMP, "hooks.sock");
const TERMINAL_SOCK = join(TMP, "terminal.sock");

const wait = (ms: number) => new Promise((r) => setTimeout(r, ms));

/** Build a HookSocketMessage with defaults */
function hook(
  event: HookEventName,
  sessionId: string,
  data: HookData = {},
  opts: { needsResponse?: boolean; pid?: number; cwd?: string } = {},
): HookSocketMessage {
  return {
    event,
    session_id: sessionId,
    cwd: opts.cwd ?? "/test/project",
    pid: opts.pid ?? 42,
    source: "bridge",
    data,
    needs_response: opts.needsResponse ?? false,
  };
}

/** Send a hook message and disconnect (fire-and-forget) */
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

/** Send a bidirectional hook message: keeps socket open, returns response */
function sendBidirectionalHookEvent(msg: HookSocketMessage): Promise<{ socket: Socket; response: Promise<Record<string, unknown>> }> {
  return new Promise((resolve, reject) => {
    const client = createConnection(HOOK_SOCK, () => {
      client.write(JSON.stringify(msg) + "\n");
      const response = new Promise<Record<string, unknown>>((res) => {
        let buf = "";
        client.on("data", (chunk) => {
          buf += chunk.toString();
          const idx = buf.indexOf("\n");
          if (idx !== -1) {
            const line = buf.substring(0, idx).trim();
            if (line.length > 0) res(JSON.parse(line));
          }
        });
        client.on("close", () => {
          if (buf.trim().length > 0) res(JSON.parse(buf.trim()));
        });
      });
      resolve({ socket: client, response });
    });
    client.on("error", (err) => reject(err));
  });
}

/** Send a terminal message */
function sendTerminalMessage(socket: Socket, msg: Record<string, unknown>): void {
  socket.write(JSON.stringify(msg) + "\n");
}

/**
 * Subscribe to a session and return live-updating patches/messages arrays.
 * Drains initial snapshot messages before returning.
 */
async function subscribeAndCollect(sessionId: string): Promise<{
  patches: Array<Patch>;
  messages: ServerMessage[];
  socket: Socket;
}> {
  const patches: Array<Patch> = [];
  const messages: ServerMessage[] = [];
  const termSocket = createConnection(TERMINAL_SOCK);
  await new Promise<void>((resolve) => termSocket.on("connect", resolve));
  sendTerminalMessage(termSocket, { type: "request.session", sessionId });

  let buf = "";
  termSocket.on("data", (chunk) => {
    buf += chunk.toString();
    let idx: number;
    while ((idx = buf.indexOf("\n")) !== -1) {
      const line = buf.substring(0, idx).trim();
      buf = buf.substring(idx + 1);
      if (line.length > 0) {
        const msg = JSON.parse(line) as ServerMessage;
        messages.push(msg);
        if (msg.type === "session.update") {
          for (const p of msg.patches) patches.push(p);
        }
      }
    }
  });

  // Drain initial messages (overview.snapshot + session.snapshot)
  await wait(150);
  messages.length = 0;
  patches.length = 0;

  return { patches, messages, socket: termSocket };
}

/** Request a session snapshot from the terminal socket */
async function requestSnapshot(sessionId: string): Promise<Record<string, unknown> | null> {
  const termSocket = createConnection(TERMINAL_SOCK);
  await new Promise<void>((resolve) => termSocket.on("connect", resolve));
  sendTerminalMessage(termSocket, { type: "request.session", sessionId });

  return new Promise((resolve) => {
    let buf = "";
    termSocket.on("data", (chunk) => {
      buf += chunk.toString();
      let idx: number;
      while ((idx = buf.indexOf("\n")) !== -1) {
        const line = buf.substring(0, idx).trim();
        buf = buf.substring(idx + 1);
        if (line.length > 0) {
          const msg = JSON.parse(line);
          if (msg.type === "session.snapshot") {
            termSocket.end();
            resolve(msg.session);
          }
        }
      }
    });
    setTimeout(() => { termSocket.end(); resolve(null); }, 500);
  });
}

// ── Test suite ───────────────────────────────────────────────────────

describe("Integration Scenarios", () => {
  let hookServer: Server;
  let terminalServer: Server;
  let cleanup: () => void;

  beforeEach(async () => {
    mkdirSync(TMP, { recursive: true });
    for (const sock of [HOOK_SOCK, TERMINAL_SOCK]) {
      if (existsSync(sock)) unlinkSync(sock);
    }

    const { Effect, Layer } = await import("effect");
    const { SessionStore: SessionStoreTag, makeSessionStore } = await import("../src/services/session-store.js");
    const { Inbox: InboxTag, makeInbox } = await import("../src/services/inbox.js");
    const { FsLive } = await import("@gravity/shared");
    const { makeTerminal } = await import("../src/services/terminal.js");
    const { parseTerminalMessage } = await import("../src/protocol/messages.js");
    const { handleEvent } = await import("../src/handlers/event-handler.js");

    const store = makeSessionStore();
    const inbox = makeInbox();
    const terminals = makeTerminal();

    const eventLayer = Layer.mergeAll(
      Layer.succeed(SessionStoreTag, store),
      Layer.succeed(InboxTag, inbox),
      FsLive,
    );

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
          const patches = Effect.runSync(Effect.provide(
            handleEvent(
              msg.event, msg.session_id, msg.cwd, msg.data, msg.pid,
              msg.needs_response ? socket : undefined,
            ),
            eventLayer,
          ));
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
          if (msg.event === "PermissionRequest" || msg.event === "AskUserQuestionIntercept") {
            const items = inbox.all();
            if (items.length > 0) {
              terminals.broadcast({ type: "inbox.added", item: items[items.length - 1] });
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
          } else if (msg.type === "action.permission") {
            Effect.runSync(inbox.respond(msg.itemId, {
              hookSpecificOutput: {
                hookEventName: "PermissionRequest",
                decision: { behavior: msg.decision, message: msg.message, updatedPermissions: msg.updatedPermissions },
              },
            }));
            terminals.broadcast({ type: "inbox.removed", itemId: msg.itemId });
          } else if (msg.type === "action.plan-review") {
            let message: string | undefined = undefined;
            if (msg.feedback) {
              const parts: string[] = ["# Plan Feedback\n"];
              if (msg.feedback.generalComment) {
                parts.push("## General comment");
                parts.push(msg.feedback.generalComment);
              }
              message = parts.join("\n");
            }
            Effect.runSync(inbox.respond(msg.itemId, {
              hookSpecificOutput: {
                hookEventName: "PermissionRequest",
                decision: { behavior: msg.decision, message },
              },
            }));
            terminals.broadcast({ type: "inbox.removed", itemId: msg.itemId });
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

  // ── 1. Multi-turn session ──────────────────────────────────────────

  describe("Multi-turn session with state accumulation", () => {
    it("3 turns accumulate prompts, tools, freeze correctly, and token usage grows", async () => {
      const SID = "multi-turn-1";
      await sendHookEvent(hook("SessionStart", SID, { model: "claude-opus-4-6-20260301" }));
      await wait(50);

      const { patches, socket: term } = await subscribeAndCollect(SID);

      // Turn 1
      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "Explain the codebase" }));
      await wait(50);
      await sendHookEvent(hook("PreToolUse", SID, { tool_name: "Read", tool_use_id: "t1", tool_input: { file_path: "/src/index.ts" } }));
      await wait(50);
      await sendHookEvent(hook("PostToolUse", SID, { tool_name: "Read", tool_use_id: "t1", tool_response: "contents" }));
      await wait(50);
      await sendHookEvent(hook("Stop", SID, {
        stop_text: "Here is the explanation",
        token_usage: { input_tokens: 100, output_tokens: 50, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 },
      }));
      await wait(50);

      // Turn 2 — two tools
      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "Now fix the bug" }));
      await wait(50);
      await sendHookEvent(hook("PreToolUse", SID, { tool_name: "Edit", tool_use_id: "t2", tool_input: { file_path: "/src/auth.ts" } }));
      await wait(50);
      await sendHookEvent(hook("PostToolUse", SID, { tool_name: "Edit", tool_use_id: "t2", tool_response: "edited" }));
      await wait(50);
      await sendHookEvent(hook("PreToolUse", SID, { tool_name: "Write", tool_use_id: "t3", tool_input: { file_path: "/src/new.ts" } }));
      await wait(50);
      await sendHookEvent(hook("PostToolUse", SID, { tool_name: "Write", tool_use_id: "t3", tool_response: "written" }));
      await wait(50);
      await sendHookEvent(hook("Stop", SID, {
        stop_text: "Bug fixed",
        token_usage: { input_tokens: 300, output_tokens: 150, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 },
      }));
      await wait(50);

      // Turn 3
      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "Run the tests" }));
      await wait(50);
      await sendHookEvent(hook("PreToolUse", SID, { tool_name: "Bash", tool_use_id: "t4", tool_input: { command: "npm test" } }));
      await wait(50);
      await sendHookEvent(hook("PostToolUse", SID, { tool_name: "Bash", tool_use_id: "t4", tool_response: "all pass" }));
      await wait(50);
      await sendHookEvent(hook("Stop", SID, {
        stop_text: "All tests pass",
        token_usage: { input_tokens: 500, output_tokens: 250, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 },
      }));
      await wait(100);

      // Verify patch ops sequence
      const ops = patches.map((p) => p.op);

      // Turn freezing: 3 freeze_turn ops (freeze turn 0 before turn 1, freeze turn 1 before turn 2, freeze turn 2 before turn 3)
      const freezes = patches.filter((p) => p.op === "freeze_turn");
      expect(freezes.length).toBe(3);

      // 3 add_turn ops (turns 1, 2, 3)
      const addTurns = patches.filter((p) => p.op === "add_turn");
      expect(addTurns.length).toBe(3);

      // 3 add_prompt ops
      const addPrompts = patches.filter((p) => p.op === "add_prompt");
      expect(addPrompts.length).toBe(3);

      // 4 add_tool ops
      const addTools = patches.filter((p) => p.op === "add_tool");
      expect(addTools.length).toBe(4);

      // 4 complete_tool ops
      const completeTools = patches.filter((p) => p.op === "complete_tool");
      expect(completeTools.length).toBe(4);

      // 3 set_turn_stop ops
      const turnStops = patches.filter((p) => p.op === "set_turn_stop");
      expect(turnStops.length).toBe(3);

      // 3 set_token_usage ops
      const tokenUsages = patches.filter((p) => p.op === "set_token_usage");
      expect(tokenUsages.length).toBe(3);

      // Verify state via snapshot
      const snap = await requestSnapshot(SID) as any;
      expect(snap).not.toBeNull();
      expect(snap.turns.length).toBe(4); // turn 0 + 3 user turns
      expect(snap.turns[0].frozen).toBe(true);
      expect(snap.turns[1].frozen).toBe(true);
      expect(snap.turns[2].frozen).toBe(true);
      expect(snap.turns[3].frozen).toBe(false); // last turn not frozen
      expect(snap.currentTurn).toBe(3);
      expect(snap.totalToolCount).toBe(4);
      expect(snap.tokenUsage.input_tokens).toBe(500);
      expect(snap.tokenUsage.output_tokens).toBe(250);
      expect(snap.claudeStatus).toBe("idle");

      term.end();
    });
  });

  // ── 2. SubagentStart/SubagentStop ──────────────────────────────────

  describe("SubagentStart/SubagentStop with nested tools", () => {
    it("agent spawns, tools route via parent_agent_id, agent completes with linked Task", async () => {
      const SID = "agent-scenario";
      await sendHookEvent(hook("SessionStart", SID));
      await wait(50);

      const { patches, socket: term } = await subscribeAndCollect(SID);

      // User prompt
      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "Refactor the module" }));
      await wait(50);

      // Task tool that spawns agent
      await sendHookEvent(hook("PreToolUse", SID, {
        tool_name: "Task", tool_use_id: "task-1",
        tool_input: { description: "explore codebase", subagent_type: "Explore" },
      }));
      await wait(50);

      // Agent starts
      await sendHookEvent(hook("SubagentStart", SID, {
        agent_id: "agent-a1", agent_type: "Explore",
      } as HookData));
      await wait(50);

      // Agent's tools (parent_agent_id routes to agent)
      await sendHookEvent(hook("PreToolUse", SID, {
        tool_name: "Read", tool_use_id: "at-1", parent_agent_id: "agent-a1",
        tool_input: { file_path: "/src/module.ts" },
      }));
      await wait(50);
      await sendHookEvent(hook("PostToolUse", SID, {
        tool_name: "Read", tool_use_id: "at-1", parent_agent_id: "agent-a1",
        tool_response: "module contents",
      }));
      await wait(50);

      await sendHookEvent(hook("PreToolUse", SID, {
        tool_name: "Edit", tool_use_id: "at-2", parent_agent_id: "agent-a1",
        tool_input: { file_path: "/src/module.ts" },
      }));
      await wait(50);
      await sendHookEvent(hook("PostToolUse", SID, {
        tool_name: "Edit", tool_use_id: "at-2", parent_agent_id: "agent-a1",
        tool_response: "edited ok",
      }));
      await wait(50);

      // Agent stops
      await sendHookEvent(hook("SubagentStop", SID, {
        agent_id: "agent-a1", agent_stop_text: "Refactored successfully",
      } as HookData));
      await wait(50);

      // Task tool completes
      await sendHookEvent(hook("PostToolUse", SID, {
        tool_name: "Task", tool_use_id: "task-1", tool_response: "done",
      }));
      await wait(50);

      // Stop
      await sendHookEvent(hook("Stop", SID, { stop_text: "Done" }));
      await wait(100);

      // Verify agent patches
      const addAgents = patches.filter((p) => p.op === "add_agent");
      expect(addAgents.length).toBe(1);
      expect((addAgents[0] as any).agent.agentId).toBe("agent-a1");
      expect((addAgents[0] as any).agent.type).toBe("Explore");

      // Agent tools have agentId set
      const agentTools = patches.filter((p) => p.op === "add_tool" && (p as any).agentId === "agent-a1");
      expect(agentTools.length).toBe(2);

      // Agent completion
      const completeAgents = patches.filter((p) => p.op === "complete_agent");
      expect(completeAgents.length).toBe(1);
      expect((completeAgents[0] as any).agentId).toBe("agent-a1");
      expect((completeAgents[0] as any).stopText).toBe("Refactored successfully");

      // Verify snapshot
      const snap = await requestSnapshot(SID) as any;
      expect(snap.turns[1].agents.length).toBe(1);
      const agent = snap.turns[1].agents[0];
      expect(agent.status).toBe("done");
      expect(agent.toolCount).toBe(2);
      expect(agent.stopText).toBe("Refactored successfully");

      term.end();
    });
  });

  // ── 3. TaskCreate/TaskUpdate lifecycle ─────────────────────────────

  describe("TaskCreate/TaskUpdate lifecycle", () => {
    it("TaskCreate pending -> re-key on PostToolUse -> TaskUpdate changes status", async () => {
      const SID = "task-lifecycle";
      await sendHookEvent(hook("SessionStart", SID));
      await wait(50);

      const { patches, socket: term } = await subscribeAndCollect(SID);

      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "Create tasks" }));
      await wait(50);

      // TaskCreate PreToolUse — creates pending entry
      await sendHookEvent(hook("PreToolUse", SID, {
        tool_name: "TaskCreate", tool_use_id: "tc-1",
        tool_input: { subject: "Fix login bug", description: "Users cannot log in" },
      }));
      await wait(50);

      // TaskCreate PostToolUse — re-keys to real taskId
      await sendHookEvent(hook("PostToolUse", SID, {
        tool_name: "TaskCreate", tool_use_id: "tc-1",
        tool_response: { taskId: "real-task-42", task: { id: "real-task-42" } },
      }));
      await wait(50);

      // TaskUpdate — changes status
      await sendHookEvent(hook("PreToolUse", SID, {
        tool_name: "TaskUpdate", tool_use_id: "tu-1",
        tool_input: { taskId: "real-task-42", status: "in_progress" },
      }));
      await wait(50);
      await sendHookEvent(hook("PostToolUse", SID, {
        tool_name: "TaskUpdate", tool_use_id: "tu-1", tool_response: "updated",
      }));
      await wait(50);

      await sendHookEvent(hook("Stop", SID, { stop_text: "Tasks created" }));
      await wait(100);

      // Verify update_task patches
      const taskPatches = patches.filter((p) => p.op === "update_task");
      expect(taskPatches.length).toBeGreaterThanOrEqual(2);

      // First: pending task with subject
      const pendingTask = taskPatches.find((p) => (p as any).taskId?.startsWith("_pending_"));
      expect(pendingTask).toBeDefined();
      expect((pendingTask as any).task.subject).toBe("Fix login bug");
      expect((pendingTask as any).task.status).toBe("pending");

      // Re-keyed task should exist
      const realTask = taskPatches.find((p) => (p as any).taskId === "real-task-42");
      expect(realTask).toBeDefined();

      // Status changed to in_progress
      const inProgressTask = taskPatches.filter((p) => (p as any).taskId === "real-task-42");
      const lastUpdate = inProgressTask[inProgressTask.length - 1] as any;
      expect(lastUpdate.task.status).toBe("in_progress");

      // Verify snapshot tasks
      const snap = await requestSnapshot(SID) as any;
      expect(snap.tasks["real-task-42"]).toBeDefined();
      expect(snap.tasks["real-task-42"].status).toBe("in_progress");

      term.end();
    });
  });

  // ── 4. AskUserQuestion flow ────────────────────────────────────────

  describe("AskUserQuestion flow", () => {
    it("question prompt created on PreToolUse, answer extracted on PostToolUse", async () => {
      const SID = "ask-question";
      await sendHookEvent(hook("SessionStart", SID));
      await wait(50);

      const { patches, socket: term } = await subscribeAndCollect(SID);

      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "Help me choose" }));
      await wait(50);

      // AskUserQuestion — creates question prompt
      await sendHookEvent(hook("PreToolUse", SID, {
        tool_name: "AskUserQuestion", tool_use_id: "ask-1",
        tool_input: { questions: [{ question: "Which framework do you prefer?" }] },
      }));
      await wait(50);

      // Answer
      await sendHookEvent(hook("PostToolUse", SID, {
        tool_name: "AskUserQuestion", tool_use_id: "ask-1",
        tool_response: { result: "I prefer React" },
      }));
      await wait(50);

      await sendHookEvent(hook("Stop", SID, { stop_text: "Noted" }));
      await wait(100);

      // Verify question prompt patch
      const promptPatches = patches.filter((p) => p.op === "add_prompt");
      const questionPrompt = promptPatches.find((p) => (p as any).prompt?.type === "question");
      expect(questionPrompt).toBeDefined();
      expect((questionPrompt as any).prompt.text).toContain("Which framework");

      // Verify answer extraction
      const answerPatches = patches.filter((p) => p.op === "set_prompt_answer");
      expect(answerPatches.length).toBeGreaterThanOrEqual(1);
      expect((answerPatches[0] as any).answer).toBe("I prefer React");

      term.end();
    });
  });

  // ── 5. ExitPlanMode full flow ──────────────────────────────────────

  describe("ExitPlanMode plan approval full flow", () => {
    it("plan set, phase-boundary created, inbox round-trip completes", async () => {
      const SID = "plan-full";
      await sendHookEvent(hook("SessionStart", SID));
      await wait(50);

      const { patches, messages, socket: term } = await subscribeAndCollect(SID);

      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "Plan the refactor" }));
      await wait(50);

      // PreToolUse ExitPlanMode
      await sendHookEvent(hook("PreToolUse", SID, {
        tool_name: "ExitPlanMode", tool_use_id: "epm-1",
        tool_input: { plan: "1. Refactor module A\n2. Update tests" },
      }));
      await wait(50);

      // PermissionRequest (bidirectional) — terminal will approve
      const { response: hookResponse } = await sendBidirectionalHookEvent(
        hook("PermissionRequest", SID, {
          tool_name: "ExitPlanMode",
          tool_input: { plan: "1. Refactor module A\n2. Update tests" },
        }, { needsResponse: true }),
      );
      await wait(100);

      // Terminal should see inbox.added
      const inboxAdded = messages.find((m) => m.type === "inbox.added") as Extract<ServerMessage, { type: "inbox.added" }>;
      expect(inboxAdded).toBeDefined();
      expect(inboxAdded.item.type).toBe("plan-review");

      // Terminal approves
      sendTerminalMessage(term, {
        type: "action.plan-review",
        itemId: inboxAdded.item.id,
        decision: "allow",
      });

      // Hook socket gets response
      const resp = await hookResponse;
      expect((resp as any).hookSpecificOutput.decision.behavior).toBe("allow");

      // inbox.removed
      await wait(50);
      const inboxRemoved = messages.find((m) => m.type === "inbox.removed");
      expect(inboxRemoved).toBeDefined();

      // PostToolUse ExitPlanMode — sets plan, creates phase-boundary turn
      await sendHookEvent(hook("PostToolUse", SID, {
        tool_name: "ExitPlanMode", tool_use_id: "epm-1",
        tool_response: { filePath: "/tmp/plan.md" },
        tool_input: { plan: "1. Refactor module A\n2. Update tests", allowedPrompts: ["refactor"] },
      }));
      await wait(50);

      await sendHookEvent(hook("Stop", SID, { stop_text: "Plan approved" }));
      await wait(100);

      // Verify set_plan patch
      const planPatches = patches.filter((p) => p.op === "set_plan");
      expect(planPatches.length).toBeGreaterThanOrEqual(1);
      const plan = (planPatches[planPatches.length - 1] as any).plan;
      expect(plan.content).toContain("Refactor module A");
      expect(plan.filePath).toBe("/tmp/plan.md");

      // Verify phase-boundary prompt
      const prompts = patches.filter((p) => p.op === "add_prompt");
      const phaseBoundary = prompts.find((p) => (p as any).prompt?.type === "phase-boundary");
      expect(phaseBoundary).toBeDefined();

      term.end();
    });
  });

  // ── 6. PostToolUseFailure ──────────────────────────────────────────

  describe("PostToolUseFailure", () => {
    it("error tool completion with error status and message", async () => {
      const SID = "tool-failure";
      await sendHookEvent(hook("SessionStart", SID));
      await wait(50);

      const { patches, socket: term } = await subscribeAndCollect(SID);

      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "Run something" }));
      await wait(50);

      await sendHookEvent(hook("PreToolUse", SID, {
        tool_name: "Bash", tool_use_id: "bash-1",
        tool_input: { command: "npm test" },
      }));
      await wait(50);

      await sendHookEvent(hook("PostToolUseFailure", SID, {
        tool_name: "Bash", tool_use_id: "bash-1",
        error: "Command exited with code 1",
      }));
      await wait(50);

      await sendHookEvent(hook("Stop", SID, { stop_text: "Tests failed" }));
      await wait(100);

      // Verify add_tool followed by complete_tool with error
      const addTools = patches.filter((p) => p.op === "add_tool");
      expect(addTools.length).toBe(1);
      expect((addTools[0] as any).tool.name).toBe("Bash");

      const completeTools = patches.filter((p) => p.op === "complete_tool");
      expect(completeTools.length).toBe(1);
      expect((completeTools[0] as any).status).toBe("error");
      expect((completeTools[0] as any).result).toContain("[ERROR]");
      expect((completeTools[0] as any).result).toContain("Command exited with code 1");

      term.end();
    });
  });

  // ── 7. SessionEnd ──────────────────────────────────────────────────

  describe("SessionEnd", () => {
    it("full termination produces status patches and removes from overview", async () => {
      const SID = "session-end";
      await sendHookEvent(hook("SessionStart", SID));
      await wait(50);

      const { patches, socket: term } = await subscribeAndCollect(SID);

      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "Do work" }));
      await wait(50);
      await sendHookEvent(hook("Stop", SID, { stop_text: "Done" }));
      await wait(50);

      // SessionEnd
      await sendHookEvent(hook("SessionEnd", SID));
      await wait(100);

      // Verify status patches
      const statusPatches = patches.filter((p) => p.op === "set_status");
      expect(statusPatches.length).toBe(1);
      expect((statusPatches[0] as any).status).toBe("ended");

      // Claude status should be idle
      const claudeStatusPatches = patches.filter((p) => p.op === "set_claude_status" && (p as any).claudeStatus === "idle");
      expect(claudeStatusPatches.length).toBeGreaterThanOrEqual(1);

      // Overview should not contain ended session
      const overviewTerm = createConnection(TERMINAL_SOCK);
      await new Promise<void>((resolve) => overviewTerm.on("connect", resolve));
      sendTerminalMessage(overviewTerm, { type: "request.overview" });
      await wait(100);

      const overviewMsgs: ServerMessage[] = [];
      let buf = "";
      await new Promise<void>((resolve) => {
        overviewTerm.on("data", (chunk) => {
          buf += chunk.toString();
          let idx: number;
          while ((idx = buf.indexOf("\n")) !== -1) {
            const line = buf.substring(0, idx).trim();
            buf = buf.substring(idx + 1);
            if (line.length > 0) overviewMsgs.push(JSON.parse(line));
          }
        });
        setTimeout(() => { overviewTerm.end(); resolve(); }, 200);
      });

      // Ended sessions are filtered from getProjectSummaries
      const overviews = overviewMsgs.filter((m) => m.type === "overview.snapshot") as Extract<ServerMessage, { type: "overview.snapshot" }>[];
      const lastOverview = overviews[overviews.length - 1];
      // Either no projects or projects with no sessions containing our SID
      const allSessions = (lastOverview?.projects ?? []).flatMap((p) => p.sessions);
      const ourSession = allSessions.find((s) => s.sessionId === SID);
      expect(ourSession).toBeUndefined();

      term.end();
    });
  });

  // ── 8. Multi-session isolation ─────────────────────────────────────

  describe("Multi-session isolation", () => {
    it("2 sessions produce isolated patches, overview shows both", async () => {
      const S1 = "iso-session-1";
      const S2 = "iso-session-2";

      await sendHookEvent(hook("SessionStart", S1, {}, { cwd: "/project-alpha" }));
      await wait(50);
      await sendHookEvent(hook("SessionStart", S2, {}, { cwd: "/project-beta" }));
      await wait(50);

      // Subscribe terminal to S1 only
      const { patches: s1Patches, messages: s1Messages, socket: term1 } = await subscribeAndCollect(S1);

      // Send events to both sessions
      await sendHookEvent(hook("UserPromptSubmit", S1, { prompt: "Work on alpha" }));
      await wait(50);
      await sendHookEvent(hook("UserPromptSubmit", S2, { prompt: "Work on beta" }));
      await wait(50);
      await sendHookEvent(hook("PreToolUse", S2, {
        tool_name: "Read", tool_use_id: "s2-t1", tool_input: { file_path: "/beta/file.ts" },
      }));
      await wait(50);
      await sendHookEvent(hook("PreToolUse", S1, {
        tool_name: "Edit", tool_use_id: "s1-t1", tool_input: { file_path: "/alpha/file.ts" },
      }));
      await wait(100);

      // S1 terminal should only see S1 session.update patches
      const s1Updates = s1Messages.filter(
        (m) => m.type === "session.update",
      ) as Extract<ServerMessage, { type: "session.update" }>[];
      for (const update of s1Updates) {
        expect(update.sessionId).toBe(S1);
      }

      // S1 patches should only contain S1 tools
      const s1Tools = s1Patches.filter((p) => p.op === "add_tool");
      expect(s1Tools.length).toBe(1);
      expect((s1Tools[0] as any).tool.name).toBe("Edit");

      // Overview should contain both sessions
      const overviews = s1Messages.filter(
        (m) => m.type === "overview.snapshot",
      ) as Extract<ServerMessage, { type: "overview.snapshot" }>[];
      if (overviews.length > 0) {
        const lastOverview = overviews[overviews.length - 1];
        const allSessionIds = lastOverview.projects.flatMap((p) => p.sessions.map((s) => s.sessionId));
        expect(allSessionIds).toContain(S1);
        expect(allSessionIds).toContain(S2);
      }

      term1.end();
    });
  });

  // ── 9. Self-healing after SessionEnd ───────────────────────────────

  describe("Self-healing after SessionEnd", () => {
    it("events after SessionEnd revive session to active", async () => {
      const SID = "revive-session";
      await sendHookEvent(hook("SessionStart", SID));
      await wait(50);

      const { patches, socket: term } = await subscribeAndCollect(SID);

      // End the session
      await sendHookEvent(hook("SessionEnd", SID));
      await wait(100);

      const endedStatus = patches.find((p) => p.op === "set_status" && (p as any).status === "ended");
      expect(endedStatus).toBeDefined();

      // Now send a new event — should revive
      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "I'm back" }));
      await wait(100);

      // Should see set_status active (self-healing)
      const revivedStatus = patches.find((p) => p.op === "set_status" && (p as any).status === "active");
      expect(revivedStatus).toBeDefined();

      // Should also see the prompt
      const prompts = patches.filter((p) => p.op === "add_prompt");
      expect(prompts.length).toBeGreaterThanOrEqual(1);

      // Verify snapshot shows active
      const snap = await requestSnapshot(SID) as any;
      expect(snap.status).toBe("active");

      term.end();
    });
  });

  // ── 10. File tracking ──────────────────────────────────────────────

  describe("File tracking", () => {
    it("Read/Edit/Write tracked in patches with correct fileOp", async () => {
      const SID = "file-tracking";
      await sendHookEvent(hook("SessionStart", SID));
      await wait(50);

      const { patches, socket: term } = await subscribeAndCollect(SID);

      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "Modify files" }));
      await wait(50);

      // Read
      await sendHookEvent(hook("PreToolUse", SID, {
        tool_name: "Read", tool_use_id: "ft-1", tool_input: { file_path: "/src/index.ts" },
      }));
      await wait(50);
      await sendHookEvent(hook("PostToolUse", SID, {
        tool_name: "Read", tool_use_id: "ft-1", tool_response: "contents",
        tool_input: { file_path: "/src/index.ts" },
      }));
      await wait(50);

      // Edit (same file)
      await sendHookEvent(hook("PreToolUse", SID, {
        tool_name: "Edit", tool_use_id: "ft-2", tool_input: { file_path: "/src/index.ts" },
      }));
      await wait(50);
      await sendHookEvent(hook("PostToolUse", SID, {
        tool_name: "Edit", tool_use_id: "ft-2", tool_response: "edited",
        tool_input: { file_path: "/src/index.ts" },
      }));
      await wait(50);

      // Write (different file)
      await sendHookEvent(hook("PreToolUse", SID, {
        tool_name: "Write", tool_use_id: "ft-3", tool_input: { file_path: "/src/new-file.ts" },
      }));
      await wait(50);
      await sendHookEvent(hook("PostToolUse", SID, {
        tool_name: "Write", tool_use_id: "ft-3", tool_response: "written",
        tool_input: { file_path: "/src/new-file.ts" },
      }));
      await wait(50);

      await sendHookEvent(hook("Stop", SID, { stop_text: "Done" }));
      await wait(100);

      // Verify track_file patches
      const filePatches = patches.filter((p) => p.op === "track_file");
      expect(filePatches.length).toBeGreaterThanOrEqual(3);

      // Check file ops present
      const readPatches = filePatches.filter(
        (p) => (p as any).path === "/src/index.ts" && (p as any).fileOp === "read",
      );
      expect(readPatches.length).toBeGreaterThanOrEqual(1);

      const editPatches = filePatches.filter(
        (p) => (p as any).path === "/src/index.ts" && (p as any).fileOp === "edit",
      );
      expect(editPatches.length).toBeGreaterThanOrEqual(1);

      const writePatches = filePatches.filter(
        (p) => (p as any).path === "/src/new-file.ts" && (p as any).fileOp === "write",
      );
      expect(writePatches.length).toBeGreaterThanOrEqual(1);

      // Verify snapshot files
      const snap = await requestSnapshot(SID) as any;
      expect(snap.files["/src/index.ts"]).toBeDefined();
      expect(snap.files["/src/index.ts"].ops).toContain("read");
      expect(snap.files["/src/index.ts"].ops).toContain("edit");
      expect(snap.files["/src/new-file.ts"]).toBeDefined();
      expect(snap.files["/src/new-file.ts"].ops).toContain("write");

      term.end();
    });
  });

  // ── 11. Session reset ──────────────────────────────────────────────

  describe("Session reset (re-start same ID)", () => {
    it("re-start same session_id emits reset patches", async () => {
      const SID = "reset-session";

      // First start + some activity
      await sendHookEvent(hook("SessionStart", SID, { model: "claude-sonnet-4-20250514" }));
      await wait(50);
      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "Do something" }));
      await wait(50);
      await sendHookEvent(hook("Stop", SID, {
        stop_text: "Done",
        token_usage: { input_tokens: 100, output_tokens: 50, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 },
      }));
      await wait(50);

      // Subscribe after first session has state
      const { patches, socket: term } = await subscribeAndCollect(SID);

      // Re-start with same session ID (simulates /clear)
      await sendHookEvent(hook("SessionStart", SID, { model: "claude-opus-4-6-20260301" }));
      await wait(100);

      // Should see reset patches from resetSession():
      // set_status active, set_claude_status idle, set_plan null,
      // set_streaming_text null, set_token_usage zeroed
      const streamingText = patches.filter((p) => p.op === "set_streaming_text");
      expect(streamingText.length).toBeGreaterThanOrEqual(1);
      expect((streamingText[0] as any).text).toBeNull();

      const planReset = patches.filter((p) => p.op === "set_plan");
      expect(planReset.length).toBeGreaterThanOrEqual(1);
      expect((planReset[0] as any).plan).toBeNull();

      const tokenReset = patches.filter((p) => p.op === "set_token_usage");
      expect(tokenReset.length).toBeGreaterThanOrEqual(1);

      const statusReset = patches.filter((p) => p.op === "set_status" && (p as any).status === "active");
      expect(statusReset.length).toBeGreaterThanOrEqual(1);

      // Verify snapshot reflects reset
      const snap = await requestSnapshot(SID) as any;
      expect(snap.turns.length).toBe(1); // only turn 0
      expect(snap.currentTurn).toBe(0);
      expect(snap.plan).toBeNull();
      expect(snap.modelName).toBe("opus"); // updated model

      term.end();
    });
  });

  // ── 12. Session metadata propagation ───────────────────────────────

  describe("Session metadata propagation", () => {
    it("slug, branch, model, tmux_session set on SessionStart and visible in snapshot", async () => {
      const SID = "meta-session";
      await sendHookEvent(hook("SessionStart", SID, {
        model: "claude-sonnet-4-20250514",
        slug: "fix-auth-bug",
        branch: "feature/login",
        tmux_session: "dev-session",
      }));
      await wait(100);

      const snap = await requestSnapshot(SID) as any;
      expect(snap).not.toBeNull();
      expect(snap.modelName).toBe("sonnet");
      expect(snap.slug).toBe("fix-auth-bug");
      expect(snap.branch).toBe("feature/login");
      expect(snap.tmuxSession).toBe("dev-session");
    });

    it("model updated on PreToolUse when model field present", async () => {
      const SID = "meta-model-update";
      await sendHookEvent(hook("SessionStart", SID, { model: "claude-sonnet-4-20250514" }));
      await wait(50);

      const { patches, socket: term } = await subscribeAndCollect(SID);

      await sendHookEvent(hook("UserPromptSubmit", SID, { prompt: "test" }));
      await wait(50);

      // PreToolUse with different model
      await sendHookEvent(hook("PreToolUse", SID, {
        tool_name: "Read", tool_use_id: "mt-1",
        tool_input: { file_path: "/src/a.ts" },
        model: "claude-opus-4-6-20260301",
      }));
      await wait(100);

      // Model should be updated in snapshot (PreToolUse updates in-memory
      // but does not emit a set_meta patch for model — only SessionStart does)
      const snap = await requestSnapshot(SID) as any;
      expect(snap.modelName).toBe("opus");

      term.end();
    });
  });
});
