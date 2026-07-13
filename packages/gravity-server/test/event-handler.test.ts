import { describe, it, expect, beforeEach } from "vitest";
import { Effect, Layer } from "effect";
import { handleEvent } from "../src/handlers/event-handler.js";
import { SessionStore, makeSessionStore, type SessionStoreService } from "../src/services/session-store.js";
import { Inbox, makeInbox, type InboxService } from "../src/services/inbox.js";
import { FsTest } from "@gravity/shared";
import type { HookData, Patch, Session, TokenUsage } from "@gravity/shared";
import type { Socket } from "net";
import { EventEmitter } from "events";

// ── Helpers ──────────────────────────────────────────────────────────

interface TestDeps {
  store: SessionStoreService;
  inbox: InboxService;
  layer: Layer.Layer<SessionStoreService | InboxService>;
}

function makeDeps(): TestDeps {
  const store = makeSessionStore();
  const inbox = makeInbox();
  const layer = Layer.mergeAll(
    Layer.succeed(SessionStore, store),
    Layer.succeed(Inbox, inbox),
  );
  return { store, inbox, layer };
}

function fire(
  deps: TestDeps,
  event: string,
  sessionId: string,
  data: HookData = {},
  pid: number | null = 123,
  hookSocket?: Socket,
): Patch[] {
  const fullLayer = Layer.mergeAll(deps.layer, FsTest({}));
  return Effect.runSync(
    Effect.provide(
      handleEvent(event as any, sessionId, "/test/project", data, pid, hookSocket),
      fullLayer,
    ),
  );
}

/** Shorthand: fire SessionStart and return deps for chaining */
function startSession(deps: TestDeps, sessionId = "s1", data: HookData = {}, pid: number | null = 123): TestDeps {
  fire(deps, "SessionStart", sessionId, data, pid);
  return deps;
}

function makeTokenUsage(overrides: Partial<TokenUsage> = {}): TokenUsage {
  return {
    input_tokens: 100,
    output_tokens: 50,
    cache_read_input_tokens: 0,
    cache_creation_input_tokens: 0,
    ...overrides,
  };
}

/** Create a minimal mock Socket for PermissionRequest tests */
function makeMockSocket(): Socket {
  const emitter = new EventEmitter();
  const written: string[] = [];
  return Object.assign(emitter, {
    write: (data: string) => { written.push(data); return true; },
    end: () => {},
    destroy: () => {},
    _written: written,
  }) as unknown as Socket;
}

// ── Tests ────────────────────────────────────────────────────────────

describe("Event Handler", () => {
  let deps: TestDeps;

  beforeEach(() => {
    deps = makeDeps();
  });

  // ────────────────────────────────────────────────────────────────────
  // SessionStart
  // ────────────────────────────────────────────────────────────────────

  describe("SessionStart", () => {
    it("creates a new session with correct defaults", () => {
      fire(deps, "SessionStart", "s1");
      const session = deps.store.get("s1");
      expect(session).toBeDefined();
      expect(session!.status).toBe("active");
      expect(session!.claudeStatus).toBe("idle");
      expect(session!.project).toBe("project");
      expect(session!.currentTurn).toBe(0);
      expect(session!.turns.length).toBe(1); // turn 0 pre-prompt
      expect(session!.totalToolCount).toBe(0);
      expect(session!.plan).toBeNull();
      expect(session!.tokenUsage).toBeNull();
      expect(session!.streamingText).toBeNull();
    });

    it("resets existing session on re-start (e.g., /clear)", () => {
      fire(deps, "SessionStart", "s1");
      fire(deps, "UserPromptSubmit", "s1", { prompt: "hello" });
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });

      const before = deps.store.get("s1");
      expect(before!.currentTurn).toBe(1);
      expect(before!.totalToolCount).toBe(1);

      fire(deps, "SessionStart", "s1");
      const after = deps.store.get("s1");
      expect(after!.currentTurn).toBe(0);
      expect(after!.turns.length).toBe(1);
      expect(after!.totalToolCount).toBe(0);
      expect(after!.status).toBe("active");
      expect(after!.plan).toBeNull();
      expect(after!.tokenUsage).toBeNull();
    });

    it("sets model name from data.model", () => {
      fire(deps, "SessionStart", "s1", { model: "claude-sonnet-4-6-20261022" });
      expect(deps.store.get("s1")!.modelName).toBe("sonnet");
    });

    it("maps opus model name", () => {
      fire(deps, "SessionStart", "s1", { model: "claude-opus-4-6-20261022" });
      expect(deps.store.get("s1")!.modelName).toBe("opus");
    });

    it("maps haiku model name", () => {
      fire(deps, "SessionStart", "s1", { model: "claude-haiku-3-20251023" });
      expect(deps.store.get("s1")!.modelName).toBe("haiku");
    });

    it("passes through unknown model names", () => {
      fire(deps, "SessionStart", "s1", { model: "gpt-4o" });
      expect(deps.store.get("s1")!.modelName).toBe("gpt-4o");
    });

    it("sets slug from data", () => {
      fire(deps, "SessionStart", "s1", { slug: "my-session" });
      expect(deps.store.get("s1")!.slug).toBe("my-session");
    });

    it("sets branch from data", () => {
      fire(deps, "SessionStart", "s1", { branch: "feature/auth" });
      expect(deps.store.get("s1")!.branch).toBe("feature/auth");
    });

    it("sets pid from argument", () => {
      fire(deps, "SessionStart", "s1", {}, 42);
      expect(deps.store.get("s1")!.pid).toBe(42);
    });

    it("does not set modelName if model is empty string", () => {
      fire(deps, "SessionStart", "s1", { model: "" });
      expect(deps.store.get("s1")!.modelName).toBeNull();
    });

    it("returns set_meta patches", () => {
      const patches = fire(deps, "SessionStart", "s1", { slug: "s", branch: "b" }, 42);
      const metaPatch = patches.find(p => p.op === "set_meta");
      expect(metaPatch).toBeDefined();
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // SessionEnd
  // ────────────────────────────────────────────────────────────────────

  describe("SessionEnd", () => {
    it("marks session ended, claude status idle", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "SessionEnd", "s1");

      const session = deps.store.get("s1");
      expect(session!.status).toBe("ended");
      expect(session!.claudeStatus).toBe("idle");
    });

    it("returns set_status and set_claude_status patches", () => {
      startSession(deps);
      const patches = fire(deps, "SessionEnd", "s1");
      expect(patches).toContainEqual({ op: "set_status", status: "ended" });
      expect(patches).toContainEqual({ op: "set_claude_status", claudeStatus: "idle" });
    });

    it("does not remove inbox items (cleanup is in server.ts)", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "Stop", "s1", { stop_text: "done" }); // creates idle inbox item
      expect(deps.inbox.all().length).toBe(1);

      fire(deps, "SessionEnd", "s1");
      // handleEvent no longer removes inbox items — that's server.ts's responsibility
      expect(deps.inbox.all().length).toBe(1);
    });

    it("is a no-op for unknown session", () => {
      const patches = fire(deps, "SessionEnd", "unknown");
      // Should not crash, returns empty-ish patches (the meta patch for non-existent session)
      expect(patches).toBeDefined();
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // UserPromptSubmit
  // ────────────────────────────────────────────────────────────────────

  describe("UserPromptSubmit", () => {
    it("creates new turn with prompt entry", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "Fix the bug" });

      const session = deps.store.get("s1");
      expect(session!.currentTurn).toBe(1);
      expect(session!.turns.length).toBe(2); // turn 0 + turn 1
      expect(session!.turns[1].prompt).toBeDefined();
      expect(session!.turns[1].prompt!.text).toBe("Fix the bug");
      expect(session!.turns[1].prompt!.type).toBe("user");
      expect(session!.turns[1].prompt!.elapsed).toBeNull();
      expect(session!.turns[1].prompt!.answer).toBeNull();
    });

    it("freezes previous turn", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "Turn 1" });
      expect(deps.store.get("s1")!.turns[0].frozen).toBe(true);

      fire(deps, "UserPromptSubmit", "s1", { prompt: "Turn 2" });
      expect(deps.store.get("s1")!.turns[1].frozen).toBe(true);
      expect(deps.store.get("s1")!.turns[2].frozen).toBe(false);
    });

    it("strips system XML from prompt text", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", {
        prompt: "Hello <system-reminder>ignore this</system-reminder> world",
      });
      expect(deps.store.get("s1")!.turns[1].prompt!.text).toBe("Hello  world");
    });

    it("strips command-name and command-args XML", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", {
        prompt: "Actual text <command-name>review</command-name><command-args>file.ts</command-args> more text",
      });
      expect(deps.store.get("s1")!.turns[1].prompt!.text).toBe("Actual text  more text");
    });

    it("sets claude status to responding", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      expect(deps.store.get("s1")!.claudeStatus).toBe("responding");
    });

    it("extracts slash commands as fallback when prompt is only XML", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", {
        prompt: "<command-name>clear</command-name>",
      });
      expect(deps.store.get("s1")!.turns[1].prompt!.text).toBe("/clear");
    });

    it("extracts slash command with args", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", {
        prompt: "<command-name>review</command-name><command-args>src/auth.ts</command-args>",
      });
      expect(deps.store.get("s1")!.turns[1].prompt!.text).toBe("/review src/auth.ts");
    });

    it("returns add_turn and set_claude_status patches", () => {
      startSession(deps);
      const patches = fire(deps, "UserPromptSubmit", "s1", { prompt: "Go" });
      expect(patches.some(p => p.op === "add_turn")).toBe(true);
      expect(patches.some(p => p.op === "add_prompt")).toBe(true);
      expect(patches.some(p => p.op === "set_claude_status")).toBe(true);
    });

    it("does not remove idle inbox items (cleanup is in server.ts)", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "first" });
      fire(deps, "Stop", "s1", { stop_text: "done" }); // creates idle item
      expect(deps.inbox.all().length).toBe(1);

      fire(deps, "UserPromptSubmit", "s1", { prompt: "second" });
      // handleEvent no longer removes inbox items — server.ts does it before calling handleEvent
      expect(deps.inbox.all().length).toBe(1);
    });

    it("creates session if not exists", () => {
      // No SessionStart fired
      fire(deps, "UserPromptSubmit", "s1", { prompt: "Go" });
      const session = deps.store.get("s1");
      expect(session).toBeDefined();
      expect(session!.currentTurn).toBe(1);
    });

    it("does not create turn for empty prompt", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "" });
      // Empty prompt stripped becomes null, no turn created for prompt
      // but claude status still set to responding
      expect(deps.store.get("s1")!.claudeStatus).toBe("responding");
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // Stop
  // ────────────────────────────────────────────────────────────────────

  describe("Stop", () => {
    it("sets claude status to idle", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "Stop", "s1");
      expect(deps.store.get("s1")!.claudeStatus).toBe("idle");
    });

    it("finalizes prompt elapsed time", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "Stop", "s1");

      const prompt = deps.store.get("s1")!.turns[1].prompt;
      expect(prompt!.elapsed).toBeGreaterThanOrEqual(0);
    });

    it("sets stop text and thinking on turn", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "Stop", "s1", {
        stop_text: "All done!",
        stop_thinking: "I finished the task",
      });

      const turn = deps.store.get("s1")!.turns[1];
      expect(turn.stopText).toBe("All done!");
      expect(turn.stopThinking).toBe("I finished the task");
    });

    it("sets token usage on session", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      const usage = makeTokenUsage({ input_tokens: 500, output_tokens: 200 });
      fire(deps, "Stop", "s1", { token_usage: usage });

      const session = deps.store.get("s1")!;
      expect(session.tokenUsage).toBeDefined();
      expect(session.tokenUsage!.input_tokens).toBe(500);
      expect(session.tokenUsage!.output_tokens).toBe(200);
    });

    it("creates idle inbox item", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "Stop", "s1", { stop_text: "Done!" });

      const items = deps.inbox.all();
      expect(items.length).toBe(1);
      expect(items[0].type).toBe("idle");
      expect(items[0].sessionId).toBe("s1");
    });

    it("adds idle inbox item on Stop (cleanup is in server.ts)", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "Stop", "s1", { stop_text: "First stop" });
      expect(deps.inbox.all().length).toBe(1);

      // Second Stop adds another idle — server.ts removes stale ones before handleEvent
      fire(deps, "Stop", "s1", { stop_text: "Second stop" });
      const items = deps.inbox.all();
      expect(items.length).toBe(2);
      // Both idle items present (cleanup is server.ts's job)
      expect(items.some(i => i.summary.includes("First stop"))).toBe(true);
      expect(items.some(i => i.summary.includes("Second stop"))).toBe(true);
    });

    it("returns set_claude_status, set_turn_stop, and token patches", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      const patches = fire(deps, "Stop", "s1", {
        stop_text: "Done",
        stop_thinking: "Thought",
        token_usage: makeTokenUsage(),
      });

      expect(patches.some(p => p.op === "set_claude_status")).toBe(true);
      expect(patches.some(p => p.op === "set_turn_stop")).toBe(true);
      expect(patches.some(p => p.op === "set_token_usage")).toBe(true);
    });

    it("is a no-op for unknown session", () => {
      const patches = fire(deps, "Stop", "unknown");
      // Should return empty patches, no crash
      expect(patches).toEqual([]);
    });

    it("truncates stop text snippet for inbox to 80 chars", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      const longText = "A".repeat(200);
      fire(deps, "Stop", "s1", { stop_text: longText });

      const items = deps.inbox.all();
      expect(items[0].summary.length).toBeLessThanOrEqual(80);
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // PreToolUse
  // ────────────────────────────────────────────────────────────────────

  describe("PreToolUse", () => {
    it("creates tool with correct fields", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_input: { file_path: "/src/index.ts" },
        assistant_text: "I'll read the file",
        assistant_thinking: "Need to check the code",
      });

      const session = deps.store.get("s1")!;
      const tool = session.turns[1].steps[0].tools[0];
      expect(tool.name).toBe("Read");
      expect(tool.toolUseId).toBe("t1");
      expect(tool.status).toBe("running");
      expect(tool.result).toBeNull();
      expect(tool.duration).toBeNull();
      expect(tool.turn).toBe(1);
      expect(tool.parentAgentId).toBeNull();
      expect(tool.ambiguous).toBe(false);
    });

    it("adds tool to current turn", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });

      const session = deps.store.get("s1")!;
      expect(session.turns[1].steps.length).toBeGreaterThan(0);
      expect(session.turns[1].steps[0].tools.length).toBe(1);
      expect(session.turns[1].toolCount).toBe(1);
      expect(session.totalToolCount).toBe(1);
    });

    it("routes tool to agent when parent_agent_id present", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
      } as HookData);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Glob",
        tool_use_id: "t1",
        parent_agent_id: "a1",
      });

      const session = deps.store.get("s1")!;
      const agent = session.turns[1].agents[0];
      expect(agent.steps.length).toBe(1);
      expect(agent.steps[0].tools.length).toBe(1);
      expect(agent.steps[0].tools[0].name).toBe("Glob");
      expect(agent.steps[0].tools[0].parentAgentId).toBe("a1");
      expect(agent.toolCount).toBe(1);

      // Root turn should NOT have the tool
      expect(session.turns[1].toolCount).toBe(0);
    });

    it("deduplicates tools by tool_use_id", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });

      expect(deps.store.get("s1")!.totalToolCount).toBe(1);
    });

    it("tracks files for Read", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_input: { file_path: "/src/app.ts" },
      });

      const session = deps.store.get("s1")!;
      expect(session.files["/src/app.ts"]).toBeDefined();
      expect(session.files["/src/app.ts"].ops).toContain("read");
    });

    it("tracks files for Edit", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Edit",
        tool_use_id: "t1",
        tool_input: { file_path: "/src/app.ts" },
      });

      expect(deps.store.get("s1")!.files["/src/app.ts"].ops).toContain("edit");
    });

    it("tracks files for Write", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Write",
        tool_use_id: "t1",
        tool_input: { file_path: "/new-file.ts" },
      });

      expect(deps.store.get("s1")!.files["/new-file.ts"].ops).toContain("write");
    });

    it("does not track files for non-file tools", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Bash",
        tool_use_id: "t1",
        tool_input: { command: "npm test" },
      });

      expect(Object.keys(deps.store.get("s1")!.files).length).toBe(0);
    });

    it("creates TaskCreate entry", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "TaskCreate",
        tool_use_id: "tc1",
        tool_input: { subject: "Fix bug", description: "Auth bug" },
      });

      const session = deps.store.get("s1")!;
      expect(session.tasks["_pending_tc1"]).toBeDefined();
      expect(session.tasks["_pending_tc1"].subject).toBe("Fix bug");
      expect(session.tasks["_pending_tc1"].description).toBe("Auth bug");
      expect(session.tasks["_pending_tc1"].status).toBe("pending");
    });

    it("creates question prompt for AskUserQuestion", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "Go" });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "AskUserQuestion",
        tool_use_id: "q1",
        tool_input: {
          questions: [{ question: "Which approach?" }],
        },
      });

      const session = deps.store.get("s1")!;
      // New turn should have been created for the question
      expect(session.currentTurn).toBe(2);
      expect(session.turns[2].prompt!.type).toBe("question");
      expect(session.turns[2].prompt!.text).toBe("Which approach?");
      expect(session.turns[2].prompt!.toolUseId).toBe("q1");
    });

    it("sets claude status to responding", () => {
      startSession(deps);
      fire(deps, "Stop", "s1"); // set to idle first
      expect(deps.store.get("s1")!.claudeStatus).toBe("idle");

      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });
      expect(deps.store.get("s1")!.claudeStatus).toBe("responding");
    });

    it("stores assistant text on step", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        assistant_text: "Let me look at this",
      });

      const step = deps.store.get("s1")!.turns[0].steps[0];
      expect(step.text).toBe("Let me look at this");
    });

    it("stores assistant thinking on step", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        assistant_thinking: "I need to understand the code",
      });

      const step = deps.store.get("s1")!.turns[0].steps[0];
      expect(step.thinking).toBe("I need to understand the code");
    });

    it("creates new step when assistant text changes", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        assistant_text: "First I'll read",
      });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Edit",
        tool_use_id: "t2",
        assistant_text: "Now I'll edit",
      });

      const turn = deps.store.get("s1")!.turns[0];
      expect(turn.steps.length).toBe(2);
      expect(turn.steps[0].text).toBe("First I'll read");
      expect(turn.steps[1].text).toBe("Now I'll edit");
    });

    it("returns add_tool and set_claude_status patches", () => {
      startSession(deps);
      const patches = fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
      });
      expect(patches.some(p => p.op === "add_tool")).toBe(true);
      expect(patches.some(p => p.op === "set_claude_status")).toBe(true);
    });

    it("updates model name from enrichment (non-agent)", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        model: "claude-opus-4-6-20261022",
      });
      expect(deps.store.get("s1")!.modelName).toBe("opus");
    });

    it("does not update model name for agent tools", () => {
      startSession(deps);
      fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
      } as HookData);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        parent_agent_id: "a1",
        model: "claude-haiku-3-20251023",
      });
      // Model should not be set to haiku from agent tool
      expect(deps.store.get("s1")!.modelName).not.toBe("haiku");
    });

    it("handles TaskUpdate on PreToolUse", () => {
      startSession(deps);
      // Create task first
      fire(deps, "PreToolUse", "s1", {
        tool_name: "TaskCreate",
        tool_use_id: "tc1",
        tool_input: { subject: "Original" },
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "TaskCreate",
        tool_use_id: "tc1",
        tool_response: { taskId: "task-1" },
      } as HookData);

      // Now update it
      fire(deps, "PreToolUse", "s1", {
        tool_name: "TaskUpdate",
        tool_use_id: "tu1",
        tool_input: { taskId: "task-1", status: "in_progress", subject: "Updated" },
      });

      const task = deps.store.get("s1")!.tasks["task-1"];
      expect(task.status).toBe("in_progress");
      expect(task.subject).toBe("Updated");
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // PostToolUse
  // ────────────────────────────────────────────────────────────────────

  describe("PostToolUse", () => {
    it("completes tool with result and duration", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_input: { file_path: "/test.ts" },
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_response: "file contents",
      } as HookData);

      const tool = deps.store.get("s1")!.turns[0].steps[0].tools[0];
      expect(tool.status).toBe("done");
      expect(tool.result).toBe("file contents");
      expect(tool.duration).toBeGreaterThanOrEqual(0);
    });

    it("stores post-tool text and thinking", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        post_tool_text: "Now I see the issue",
        post_tool_thinking: "The bug is clear",
      });

      const tool = deps.store.get("s1")!.turns[0].steps[0].tools[0];
      expect(tool.postText).toBe("Now I see the issue");
      expect(tool.postThinking).toBe("The bug is clear");
    });

    it("tracks TaskCreate re-key on PostToolUse", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "TaskCreate",
        tool_use_id: "tc1",
        tool_input: { subject: "Fix bug" },
      });

      expect(deps.store.get("s1")!.tasks["_pending_tc1"]).toBeDefined();

      fire(deps, "PostToolUse", "s1", {
        tool_name: "TaskCreate",
        tool_use_id: "tc1",
        tool_response: { taskId: "real-123" },
      } as HookData);

      const session = deps.store.get("s1")!;
      expect(session.tasks["_pending_tc1"]).toBeUndefined();
      expect(session.tasks["real-123"]).toBeDefined();
      expect(session.tasks["real-123"].subject).toBe("Fix bug");
      expect(session.tasks["real-123"].taskId).toBe("real-123");
    });

    it("handles TaskCreate with nested task.id response", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "TaskCreate",
        tool_use_id: "tc1",
        tool_input: { subject: "Fix bug" },
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "TaskCreate",
        tool_use_id: "tc1",
        tool_response: { task: { id: "nested-id-1" } },
      } as HookData);

      expect(deps.store.get("s1")!.tasks["nested-id-1"]).toBeDefined();
    });

    it("handles TaskList reconciliation", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "TaskList",
        tool_use_id: "tl1",
        tool_input: {},
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "TaskList",
        tool_use_id: "tl1",
        tool_response: {
          tasks: [
            { id: "t1", subject: "Task one", status: "completed" },
            { id: "t2", subject: "Task two", status: "pending" },
          ],
        },
      } as HookData);

      const session = deps.store.get("s1")!;
      expect(session.tasks["t1"]).toBeDefined();
      expect(session.tasks["t1"].status).toBe("completed");
      expect(session.tasks["t2"]).toBeDefined();
      expect(session.tasks["t2"].status).toBe("pending");
    });

    it("extracts AskUserQuestion answer", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "AskUserQuestion",
        tool_use_id: "q1",
        tool_input: { questions: [{ question: "Which?" }] },
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "AskUserQuestion",
        tool_use_id: "q1",
        tool_response: { result: "Option A" },
      } as HookData);

      const session = deps.store.get("s1")!;
      // Find the question turn
      const qTurn = session.turns.find(t => t.prompt?.type === "question" && t.prompt?.toolUseId === "q1");
      expect(qTurn).toBeDefined();
      expect(qTurn!.prompt!.answer).toBe("Option A");
    });

    it("extracts AskUserQuestion answer from 'answer' field", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "AskUserQuestion",
        tool_use_id: "q1",
        tool_input: { questions: [{ question: "Which?" }] },
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "AskUserQuestion",
        tool_use_id: "q1",
        tool_response: { answer: "Option B" },
      } as HookData);

      const qTurn = deps.store.get("s1")!.turns.find(t => t.prompt?.toolUseId === "q1");
      expect(qTurn!.prompt!.answer).toBe("Option B");
    });

    it("detects ExitPlanMode and creates plan + phase boundary", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "Plan this" });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "ExitPlanMode",
        tool_use_id: "t1",
        tool_input: {
          plan: "1. Do X\n2. Do Y",
          allowedPrompts: ["Bash(npm test)"],
        },
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "ExitPlanMode",
        tool_use_id: "t1",
        tool_input: {
          plan: "1. Do X\n2. Do Y",
          allowedPrompts: ["Bash(npm test)"],
        },
        tool_response: { filePath: "/tmp/plan.md" },
      } as HookData);

      const session = deps.store.get("s1")!;
      expect(session.plan).toBeDefined();
      expect(session.plan!.content).toBe("1. Do X\n2. Do Y");
      expect(session.plan!.filePath).toBe("/tmp/plan.md");
      expect(session.plan!.allowedPrompts).toEqual(["Bash(npm test)"]);

      // Phase boundary turn
      const lastTurn = session.turns[session.turns.length - 1];
      expect(lastTurn.prompt!.type).toBe("phase-boundary");
      expect(lastTurn.prompt!.text).toBe("[Plan approved]");
    });

    it("updates turn tokens from token_usage", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        token_usage: makeTokenUsage({ input_tokens: 200, output_tokens: 100 }),
      } as HookData);

      const turn = deps.store.get("s1")!.turns[1];
      // First tool delta: full amounts since baseline is 0
      expect(turn.tokenIn).toBe(200);
      expect(turn.tokenOut).toBe(100);
    });

    it("returns complete_tool patch", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });
      const patches = fire(deps, "PostToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_response: "result",
      } as HookData);

      expect(patches.some(p => p.op === "complete_tool")).toBe(true);
    });

    it("also tracks files on PostToolUse", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Write",
        tool_use_id: "t1",
        tool_input: { file_path: "/new.ts" },
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "Write",
        tool_use_id: "t1",
        tool_input: { file_path: "/new.ts" },
      });

      expect(deps.store.get("s1")!.files["/new.ts"]).toBeDefined();
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // PostToolUseFailure
  // ────────────────────────────────────────────────────────────────────

  describe("PostToolUseFailure", () => {
    it("completes tool with error status", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", { tool_name: "Bash", tool_use_id: "t1" });
      fire(deps, "PostToolUseFailure", "s1", {
        tool_name: "Bash",
        tool_use_id: "t1",
        error: "Command not found",
      } as HookData);

      const tool = deps.store.get("s1")!.turns[0].steps[0].tools[0];
      expect(tool.status).toBe("error");
      expect(tool.result).toBe("[ERROR] Command not found");
    });

    it("stores post-tool text on failure", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", { tool_name: "Bash", tool_use_id: "t1" });
      fire(deps, "PostToolUseFailure", "s1", {
        tool_name: "Bash",
        tool_use_id: "t1",
        error: "fail",
        post_tool_text: "Let me try another approach",
        post_tool_thinking: "That didn't work",
      } as HookData);

      const tool = deps.store.get("s1")!.turns[0].steps[0].tools[0];
      expect(tool.postText).toBe("Let me try another approach");
      expect(tool.postThinking).toBe("That didn't work");
    });

    it("returns complete_tool patch with error status", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", { tool_name: "Bash", tool_use_id: "t1" });
      const patches = fire(deps, "PostToolUseFailure", "s1", {
        tool_name: "Bash",
        tool_use_id: "t1",
        error: "fail",
      } as HookData);

      const completePatch = patches.find(p => p.op === "complete_tool");
      expect(completePatch).toBeDefined();
      expect((completePatch as any).status).toBe("error");
    });

    it("uses default error message when error field missing", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", { tool_name: "Bash", tool_use_id: "t1" });
      fire(deps, "PostToolUseFailure", "s1", {
        tool_name: "Bash",
        tool_use_id: "t1",
      });

      const tool = deps.store.get("s1")!.turns[0].steps[0].tools[0];
      expect(tool.result).toBe("[ERROR] Unknown error");
    });

    it("tracks files on failure too", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Edit",
        tool_use_id: "t1",
        tool_input: { file_path: "/bad.ts" },
      });
      fire(deps, "PostToolUseFailure", "s1", {
        tool_name: "Edit",
        tool_use_id: "t1",
        tool_input: { file_path: "/bad.ts" },
        error: "parse error",
      } as HookData);

      expect(deps.store.get("s1")!.files["/bad.ts"]).toBeDefined();
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // SubagentStart
  // ────────────────────────────────────────────────────────────────────

  describe("SubagentStart", () => {
    it("creates agent in current turn", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
        agent_transcript_path: "/tmp/agent.jsonl",
      } as HookData);

      const session = deps.store.get("s1")!;
      const agent = session.turns[1].agents[0];
      expect(agent).toBeDefined();
      expect(agent.agentId).toBe("a1");
      expect(agent.type).toBe("Explore");
      expect(agent.status).toBe("running");
      expect(agent.steps).toEqual([]);
      expect(agent.toolCount).toBe(0);
      expect(agent.stopText).toBeNull();
      expect(agent.stopThinking).toBeNull();
      expect(agent.duration).toBeNull();
      expect(agent.transcriptPath).toBe("/tmp/agent.jsonl");
      expect(session.turns[1].agentCount).toBe(1);
    });

    it("links agent to Task tool", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Task",
        tool_use_id: "task1",
        tool_input: { subagent_type: "Explore", prompt: "search files" },
      });
      fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
      } as HookData);

      const session = deps.store.get("s1")!;
      const taskTool = session.turns[1].steps[0].tools[0];
      expect(taskTool.agentId).toBe("a1");
      expect(session.turns[1].agents[0].taskToolUseId).toBe("task1");
    });

    it("deduplicates agents by agentId", () => {
      startSession(deps);
      fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
      } as HookData);
      fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
      } as HookData);

      expect(deps.store.get("s1")!.turns[0].agents.length).toBe(1);
    });

    it("returns add_agent patch", () => {
      startSession(deps);
      const patches = fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
      } as HookData);

      expect(patches.some(p => p.op === "add_agent")).toBe(true);
    });

    it("defaults unknown agent_id and type", () => {
      startSession(deps);
      fire(deps, "SubagentStart", "s1", {} as HookData);

      const agent = deps.store.get("s1")!.turns[0].agents[0];
      expect(agent.agentId).toBe("unknown");
      expect(agent.type).toBe("unknown");
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // SubagentStop
  // ────────────────────────────────────────────────────────────────────

  describe("SubagentStop", () => {
    it("completes agent with stop text/thinking/duration", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
      } as HookData);

      fire(deps, "SubagentStop", "s1", {
        agent_id: "a1",
        agent_stop_text: "Found 3 files",
        agent_stop_thinking: "The search revealed patterns",
      });

      const agent = deps.store.get("s1")!.turns[1].agents[0];
      expect(agent.status).toBe("done");
      expect(agent.stopText).toBe("Found 3 files");
      expect(agent.stopThinking).toBe("The search revealed patterns");
      expect(agent.duration).toBeGreaterThanOrEqual(0);
    });

    it("updates transcript path on stop", () => {
      startSession(deps);
      fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
      } as HookData);
      fire(deps, "SubagentStop", "s1", {
        agent_id: "a1",
        agent_transcript_path: "/tmp/agent-complete.jsonl",
      });

      expect(deps.store.get("s1")!.turns[0].agents[0].transcriptPath).toBe("/tmp/agent-complete.jsonl");
    });

    it("returns complete_agent patch", () => {
      startSession(deps);
      fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
      } as HookData);
      const patches = fire(deps, "SubagentStop", "s1", {
        agent_id: "a1",
        agent_stop_text: "done",
      });

      expect(patches.some(p => p.op === "complete_agent")).toBe(true);
    });

    it("is a no-op when agent_id is missing", () => {
      startSession(deps);
      const patches = fire(deps, "SubagentStop", "s1", {});
      // No agent_id means nothing happens
      expect(patches.filter(p => p.op === "complete_agent").length).toBe(0);
    });

    it("is a no-op for unknown agent_id", () => {
      startSession(deps);
      const patches = fire(deps, "SubagentStop", "s1", { agent_id: "unknown-agent" });
      expect(patches.filter(p => p.op === "complete_agent").length).toBe(0);
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // PermissionRequest
  // ────────────────────────────────────────────────────────────────────

  describe("PermissionRequest", () => {
    it("creates plan-review inbox item for ExitPlanMode", () => {
      startSession(deps);
      const socket = makeMockSocket();
      const patches = fire(deps, "PermissionRequest", "s1", {
        tool_name: "ExitPlanMode",
      }, 123, socket);

      const items = deps.inbox.all();
      expect(items.length).toBe(1);
      expect(items[0].type).toBe("plan-review");
      expect(items[0].sessionId).toBe("s1");
      expect(items[0].summary).toBe("ExitPlanMode");

      // Should have a pending response
      const pending = deps.inbox.getPending(items[0].id);
      expect(pending).toBeDefined();
      expect(pending!.hookSocket).toBe(socket);
    });

    it("creates permission inbox item for non-ExitPlanMode tools", () => {
      startSession(deps);
      const socket = makeMockSocket();
      fire(deps, "PermissionRequest", "s1", {
        tool_name: "Bash",
      }, 123, socket);

      const items = deps.inbox.all();
      expect(items.length).toBe(1);
      expect(items[0].type).toBe("permission");
    });

    it("returns claude status idle patch (blocked waiting for user)", () => {
      startSession(deps);
      // Set status to responding (as UserPromptSubmit would)
      fire(deps, "UserPromptSubmit", "s1", { prompt: "do something" });
      const session = deps.store.get("s1")!;
      expect(session.claudeStatus).toBe("responding");

      const socket = makeMockSocket();
      const patches = fire(deps, "PermissionRequest", "s1", {
        tool_name: "Bash",
      }, 123, socket);

      // Should include set_claude_status: idle (Claude is blocked waiting)
      const statusPatch = patches.find((p: any) => p.op === "set_claude_status");
      expect(statusPatch).toBeDefined();
      expect(statusPatch).toMatchObject({ op: "set_claude_status", claudeStatus: "idle" });
      expect(session.claudeStatus).toBe("idle");
    });

    it("does not create inbox item without hook socket", () => {
      startSession(deps);
      const patches = fire(deps, "PermissionRequest", "s1", {
        tool_name: "ExitPlanMode",
      });

      // No socket means no inbox item, just returns empty
      const items = deps.inbox.all().filter(i => i.type === "plan-review" || i.type === "permission");
      expect(items.length).toBe(0);
    });

    // Regression: Claude Code fires PermissionRequest (matcher "") for
    // AskUserQuestion ~150ms after AskUserQuestionIntercept. A generic
    // "permission" item here renders the raw tool_input as a JSON dump and
    // buries the real question UI. handlePermissionRequest must skip it and
    // unblock the redundant hook socket with an allow passthrough.
    it("does not create a permission item for AskUserQuestion (owned by intercept)", () => {
      startSession(deps);
      const socket = makeMockSocket();
      fire(deps, "PermissionRequest", "s1", {
        tool_name: "AskUserQuestion",
        tool_input: { questions: [{ question: "Pick one", options: [] }] },
      }, 123, socket);

      const items = deps.inbox.all().filter(
        i => i.type === "permission" || i.type === "plan-review",
      );
      expect(items.length).toBe(0);
    });

    it("unblocks the redundant AskUserQuestion PermissionRequest socket with allow", () => {
      startSession(deps);
      const socket = makeMockSocket();
      fire(deps, "PermissionRequest", "s1", {
        tool_name: "AskUserQuestion",
        tool_input: { questions: [{ question: "Pick one", options: [] }] },
      }, 123, socket);

      const written = (socket as unknown as { _written: string[] })._written;
      expect(written.length).toBe(1);
      expect(JSON.parse(written[0])).toEqual({
        hookSpecificOutput: {
          hookEventName: "PermissionRequest",
          decision: { behavior: "allow" },
        },
      });
    });

    it("uses session slug as label when available", () => {
      fire(deps, "SessionStart", "s1", { slug: "my-slug" });
      const socket = makeMockSocket();
      fire(deps, "PermissionRequest", "s1", { tool_name: "Bash" }, 123, socket);

      const items = deps.inbox.all();
      expect(items[0].label).toBe("my-slug");
    });

    it("falls back to truncated session ID when no slug", () => {
      fire(deps, "SessionStart", "s1");
      const socket = makeMockSocket();
      fire(deps, "PermissionRequest", "s1", { tool_name: "Bash" }, 123, socket);

      const items = deps.inbox.all();
      expect(items[0].label).toBe("s1");
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // Notification
  // ────────────────────────────────────────────────────────────────────

  describe("Notification", () => {
    it("returns empty patches (no state mutation)", () => {
      startSession(deps);
      const patches = fire(deps, "Notification", "s1", {});
      // Only meta patch from existing session
      expect(patches.filter(p => p.op !== "set_meta").length).toBe(0);
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // Self-healing
  // ────────────────────────────────────────────────────────────────────

  describe("Self-healing", () => {
    it("revives ended session on new event (not SessionEnd)", () => {
      startSession(deps);
      fire(deps, "SessionEnd", "s1");
      expect(deps.store.get("s1")!.status).toBe("ended");

      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });
      expect(deps.store.get("s1")!.status).toBe("active");
    });

    it("does not self-heal on SessionEnd", () => {
      startSession(deps);
      fire(deps, "SessionEnd", "s1");
      fire(deps, "SessionEnd", "s1");
      expect(deps.store.get("s1")!.status).toBe("ended");
    });

    it("self-heals on UserPromptSubmit", () => {
      startSession(deps);
      fire(deps, "SessionEnd", "s1");
      fire(deps, "UserPromptSubmit", "s1", { prompt: "hello" });
      expect(deps.store.get("s1")!.status).toBe("active");
    });

    it("self-heals on SubagentStart", () => {
      startSession(deps);
      fire(deps, "SessionEnd", "s1");
      fire(deps, "SubagentStart", "s1", { agent_id: "a1", agent_type: "X" } as HookData);
      expect(deps.store.get("s1")!.status).toBe("active");
    });

    it("does not self-heal on Notification", () => {
      startSession(deps);
      fire(deps, "SessionEnd", "s1");
      expect(deps.store.get("s1")!.status).toBe("ended");
      fire(deps, "Notification", "s1");
      expect(deps.store.get("s1")!.status).toBe("ended");
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // Cross-event scenarios
  // ────────────────────────────────────────────────────────────────────

  describe("Cross-event scenarios", () => {
    it("full turn lifecycle: UserPromptSubmit -> PreToolUse -> PostToolUse -> Stop", () => {
      startSession(deps, "s1", { slug: "test-slug", model: "claude-sonnet-4-6" });

      fire(deps, "UserPromptSubmit", "s1", { prompt: "Fix the auth bug" });
      expect(deps.store.get("s1")!.claudeStatus).toBe("responding");

      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_input: { file_path: "/src/auth.ts" },
        assistant_text: "Let me read the file",
      });
      expect(deps.store.get("s1")!.totalToolCount).toBe(1);

      fire(deps, "PostToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_response: "function auth() {}",
        post_tool_text: "I see the issue",
      } as HookData);
      const tool = deps.store.get("s1")!.turns[1].steps[0].tools[0];
      expect(tool.status).toBe("done");
      expect(tool.postText).toBe("I see the issue");

      fire(deps, "Stop", "s1", {
        stop_text: "Fixed the auth bug",
        token_usage: makeTokenUsage({ input_tokens: 500, output_tokens: 200 }),
      });

      const session = deps.store.get("s1")!;
      expect(session.claudeStatus).toBe("idle");
      expect(session.turns[1].stopText).toBe("Fixed the auth bug");
      expect(session.tokenUsage).toBeDefined();
      expect(session.files["/src/auth.ts"]).toBeDefined();
    });

    it("multi-turn with tool dedup", () => {
      startSession(deps);

      // Turn 1
      fire(deps, "UserPromptSubmit", "s1", { prompt: "First turn" });
      fire(deps, "PreToolUse", "s1", { tool_name: "Glob", tool_use_id: "t1" });
      fire(deps, "PostToolUse", "s1", { tool_name: "Glob", tool_use_id: "t1", tool_response: ["a.ts"] } as HookData);
      fire(deps, "Stop", "s1", { stop_text: "Done first" });

      // Turn 2
      fire(deps, "UserPromptSubmit", "s1", { prompt: "Second turn" });
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t2" });
      // Attempt duplicate
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t2" });
      fire(deps, "PostToolUse", "s1", { tool_name: "Read", tool_use_id: "t2", tool_response: "content" } as HookData);
      fire(deps, "Stop", "s1", { stop_text: "Done second" });

      const session = deps.store.get("s1")!;
      expect(session.currentTurn).toBe(2);
      expect(session.totalToolCount).toBe(2); // t1 + t2, no dups
      expect(session.turns[1].frozen).toBe(true);
      // Stop now freezes the turn (was previously only frozen by the next
      // UserPromptSubmit — see design/pi-adapter.md "freeze on Stop").
      expect(session.turns[2].frozen).toBe(true);
      expect(session.turns[1].stopText).toBe("Done first");
      expect(session.turns[2].stopText).toBe("Done second");
    });

    it("agent with nested tools: SubagentStart -> PreToolUse(agent) -> PostToolUse(agent) -> SubagentStop", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "Research" });

      // Launch agent via Task tool
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Task",
        tool_use_id: "task1",
        tool_input: { subagent_type: "Explore", prompt: "find patterns" },
      });

      // Agent starts
      fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
        agent_transcript_path: "/tmp/agent.jsonl",
      } as HookData);

      // Verify Task<->Agent linking
      const taskTool = deps.store.get("s1")!.turns[1].steps[0].tools[0];
      expect(taskTool.agentId).toBe("a1");

      // Agent uses tools
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Glob",
        tool_use_id: "at1",
        parent_agent_id: "a1",
        assistant_text: "Searching for patterns",
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "Glob",
        tool_use_id: "at1",
        tool_response: ["a.ts", "b.ts"],
      } as HookData);

      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "at2",
        parent_agent_id: "a1",
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "at2",
        tool_response: "file content",
      } as HookData);

      // Agent stops
      fire(deps, "SubagentStop", "s1", {
        agent_id: "a1",
        agent_stop_text: "Found 3 patterns",
        agent_stop_thinking: "Analysis complete",
      });

      const session = deps.store.get("s1")!;
      const agent = session.turns[1].agents[0];

      expect(agent.status).toBe("done");
      expect(agent.stopText).toBe("Found 3 patterns");
      expect(agent.toolCount).toBe(2);
      expect(agent.steps[0].tools[0].name).toBe("Glob");
      expect(agent.steps[0].tools[0].status).toBe("done");

      // Root turn should only have the Task tool, not agent tools
      expect(session.turns[1].toolCount).toBe(1); // only Task
      expect(session.totalToolCount).toBe(3); // Task + 2 agent tools
    });

    it("pre-prompt activity in turn 0", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", { tool_name: "Glob", tool_use_id: "t0" });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "Glob",
        tool_use_id: "t0",
        tool_response: ["file.ts"],
      } as HookData);

      const session = deps.store.get("s1")!;
      expect(session.currentTurn).toBe(0);
      expect(session.turns[0].steps[0].tools[0].name).toBe("Glob");
    });

    it("multiple tools in same step (same assistant text)", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });

      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        assistant_text: "Let me check these files",
      });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t2",
        assistant_text: "Let me check these files",
      });

      const turn = deps.store.get("s1")!.turns[1];
      expect(turn.steps.length).toBe(1);
      expect(turn.steps[0].tools.length).toBe(2);
    });

    it("multiple file ops aggregate per file", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_input: { file_path: "/src/app.ts" },
      });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Edit",
        tool_use_id: "t2",
        tool_input: { file_path: "/src/app.ts" },
      });

      const fileEntry = deps.store.get("s1")!.files["/src/app.ts"];
      expect(fileEntry.ops).toContain("read");
      expect(fileEntry.ops).toContain("edit");
      expect(fileEntry.ops.length).toBe(2);
    });

    it("does not duplicate file ops", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_input: { file_path: "/src/app.ts" },
      });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t2",
        tool_input: { file_path: "/src/app.ts" },
      });

      const fileEntry = deps.store.get("s1")!.files["/src/app.ts"];
      expect(fileEntry.ops.filter(op => op === "read").length).toBe(1);
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // Patch verification
  // ────────────────────────────────────────────────────────────────────

  describe("Patch verification", () => {
    it("SessionStart returns set_meta and set_meta (model) patches", () => {
      const patches = fire(deps, "SessionStart", "s1", { slug: "slug", model: "claude-opus-4-6" }, 42);

      const metaPatches = patches.filter(p => p.op === "set_meta");
      expect(metaPatches.length).toBeGreaterThanOrEqual(1);

      // Model patch
      const modelPatch = patches.find(p => p.op === "set_meta" && "modelName" in p);
      expect(modelPatch).toBeDefined();
    });

    it("propagates tmux_session through set_meta patch and session state", () => {
      const patches = fire(deps, "SessionStart", "s1", { tmux_session: "my-tmux" }, 42);
      const metaPatch = patches.find(p => p.op === "set_meta" && "tmuxSession" in p);
      expect(metaPatch).toBeDefined();
      expect(metaPatch!.op === "set_meta" && metaPatch!.tmuxSession).toBe("my-tmux");

      const session = deps.store.get("s1");
      expect(session?.tmuxSession).toBe("my-tmux");
    });

    it("propagates tmux_session on non-SessionStart events via ensureSession", () => {
      // First event for this session is PreToolUse (not SessionStart) — e.g. after server restart
      const patches = fire(deps, "PreToolUse", "s1", {
        tmux_session: "tmux-0",
        tool_name: "Read",
        tool_use_id: "tu_1",
      }, 42);
      const session = deps.store.get("s1");
      expect(session?.tmuxSession).toBe("tmux-0");
    });

    it("SessionEnd patches match session state", () => {
      startSession(deps);
      const patches = fire(deps, "SessionEnd", "s1");
      const session = deps.store.get("s1")!;

      expect(patches).toContainEqual({ op: "set_status", status: session.status });
      expect(patches).toContainEqual({ op: "set_claude_status", claudeStatus: session.claudeStatus });
    });

    it("UserPromptSubmit returns freeze_turn, add_turn, add_prompt, set_claude_status", () => {
      startSession(deps);
      const patches = fire(deps, "UserPromptSubmit", "s1", { prompt: "Go" });

      expect(patches.some(p => p.op === "freeze_turn")).toBe(true);
      expect(patches.some(p => p.op === "add_turn")).toBe(true);
      expect(patches.some(p => p.op === "add_prompt")).toBe(true);
      expect(patches.some(p => p.op === "set_claude_status")).toBe(true);
    });

    it("PreToolUse returns add_tool, set_permission_mode, set_claude_status, track_file", () => {
      startSession(deps);
      const patches = fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_input: { file_path: "/src/file.ts" },
      });

      expect(patches.some(p => p.op === "add_tool")).toBe(true);
      expect(patches.some(p => p.op === "set_permission_mode")).toBe(true);
      expect(patches.some(p => p.op === "set_claude_status")).toBe(true);
      expect(patches.some(p => p.op === "track_file")).toBe(true);
    });

    it("PostToolUse returns complete_tool patch with correct fields", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });
      const patches = fire(deps, "PostToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_response: "result data",
        post_tool_text: "post text",
      } as HookData);

      const completePatch = patches.find(p => p.op === "complete_tool") as any;
      expect(completePatch).toBeDefined();
      expect(completePatch.toolUseId).toBe("t1");
      expect(completePatch.result).toBe("result data");
      expect(completePatch.status).toBe("done");
      expect(completePatch.postText).toBe("post text");
      expect(completePatch.duration).toBeGreaterThanOrEqual(0);
    });

    it("Stop returns set_claude_status, set_turn_stop, set_token_usage, set_turn_tokens", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      const patches = fire(deps, "Stop", "s1", {
        stop_text: "Done",
        stop_thinking: "Finished",
        token_usage: makeTokenUsage(),
      });

      expect(patches.some(p => p.op === "set_claude_status")).toBe(true);
      expect(patches.some(p => p.op === "set_turn_stop")).toBe(true);
      expect(patches.some(p => p.op === "set_token_usage")).toBe(true);
      expect(patches.some(p => p.op === "set_turn_tokens")).toBe(true);
    });

    it("SubagentStart returns add_agent patch", () => {
      startSession(deps);
      const patches = fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
      } as HookData);

      const agentPatch = patches.find(p => p.op === "add_agent") as any;
      expect(agentPatch).toBeDefined();
      expect(agentPatch.agent.agentId).toBe("a1");
      expect(agentPatch.agent.type).toBe("Explore");
    });

    it("SubagentStop returns complete_agent patch", () => {
      startSession(deps);
      fire(deps, "SubagentStart", "s1", { agent_id: "a1", agent_type: "X" } as HookData);
      const patches = fire(deps, "SubagentStop", "s1", {
        agent_id: "a1",
        agent_stop_text: "done",
      });

      const completePatch = patches.find(p => p.op === "complete_agent") as any;
      expect(completePatch).toBeDefined();
      expect(completePatch.agentId).toBe("a1");
      expect(completePatch.stopText).toBe("done");
    });

    it("ExitPlanMode returns set_plan, add_turn, add_prompt patches", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "plan" });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "ExitPlanMode",
        tool_use_id: "t1",
        tool_input: { plan: "the plan" },
      });
      const patches = fire(deps, "PostToolUse", "s1", {
        tool_name: "ExitPlanMode",
        tool_use_id: "t1",
        tool_input: { plan: "the plan" },
        tool_response: { filePath: "/plan.md" },
      } as HookData);

      expect(patches.some(p => p.op === "set_plan")).toBe(true);
      expect(patches.some(p => p.op === "add_turn")).toBe(true);
      expect(patches.some(p => p.op === "add_prompt")).toBe(true);
    });

    it("TaskCreate returns update_task patches at both Pre and Post", () => {
      startSession(deps);
      const prePatches = fire(deps, "PreToolUse", "s1", {
        tool_name: "TaskCreate",
        tool_use_id: "tc1",
        tool_input: { subject: "Do thing" },
      });
      expect(prePatches.some(p => p.op === "update_task")).toBe(true);

      const postPatches = fire(deps, "PostToolUse", "s1", {
        tool_name: "TaskCreate",
        tool_use_id: "tc1",
        tool_response: { taskId: "real-1" },
      } as HookData);
      expect(postPatches.some(p => p.op === "update_task")).toBe(true);
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // Meta update on every event
  // ────────────────────────────────────────────────────────────────────

  describe("Meta updates", () => {
    it("updates lastEventTime on every event", () => {
      startSession(deps);
      const timeBefore = deps.store.get("s1")!.lastEventTime;

      // Fire any event
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });
      const timeAfter = deps.store.get("s1")!.lastEventTime;
      expect(timeAfter).toBeGreaterThanOrEqual(timeBefore);
    });

    it("updates PID from event", () => {
      startSession(deps, "s1", {}, 100);
      expect(deps.store.get("s1")!.pid).toBe(100);

      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" }, 200);
      expect(deps.store.get("s1")!.pid).toBe(200);

      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t2" }, 300);
      expect(deps.store.get("s1")!.pid).toBe(300);
    });

    it("does not overwrite slug once set", () => {
      fire(deps, "SessionStart", "s1", { slug: "original" });
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1", slug: "new" });
      expect(deps.store.get("s1")!.slug).toBe("original");
    });

    it("updates branch on subsequent events", () => {
      startSession(deps, "s1", { branch: "main" });
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1", branch: "feature" });
      expect(deps.store.get("s1")!.branch).toBe("feature");
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // Inbox dismiss on boundaries
  // ────────────────────────────────────────────────────────────────────

  describe("Inbox dismissal (moved to server.ts)", () => {
    // NOTE: Inbox cleanup for stale bidirectional items now happens in server.ts's
    // handleHookMessage, before calling handleEvent. These tests verify that
    // handleEvent itself does NOT remove inbox items.

    it("handleEvent does not remove inbox items on UserPromptSubmit", () => {
      startSession(deps);
      deps.inbox.add("idle", "s1", "project", "s1", "idle", {});
      deps.inbox.add("permission", "s1", "project", "s1", "perm", {});
      expect(deps.inbox.all().length).toBe(2);

      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      // Items remain — server.ts removes them before calling handleEvent
      expect(deps.inbox.all().length).toBe(2);
    });

    it("handleEvent does not remove inbox items on Stop (but adds idle)", () => {
      startSession(deps);
      deps.inbox.add("idle", "s1", "project", "s1", "old idle", {});
      fire(deps, "Stop", "s1");
      // Old idle remains, new idle added
      const items = deps.inbox.all();
      expect(items.length).toBe(2);
    });

    it("handleEvent does not remove inbox items on SessionEnd", () => {
      startSession(deps);
      deps.inbox.add("idle", "s1", "project", "s1", "idle", {});
      deps.inbox.add("permission", "s1", "project", "s1", "perm", {});
      fire(deps, "SessionEnd", "s1");
      // Items remain — server.ts removes them before calling handleEvent
      expect(deps.inbox.all().length).toBe(2);
    });

    it("items from other sessions are unaffected", () => {
      startSession(deps, "s1");
      startSession(deps, "s2");
      deps.inbox.add("idle", "s1", "project", "s1", "s1-idle", {});
      deps.inbox.add("idle", "s2", "project", "s2", "s2-idle", {});

      fire(deps, "SessionEnd", "s1");
      const items = deps.inbox.all();
      expect(items.length).toBe(2);
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // ProjectSummary
  // ────────────────────────────────────────────────────────────────────

  describe("ProjectSummary", () => {
    it("groups sessions by project", () => {
      fire(deps, "SessionStart", "s1");
      fire(deps, "SessionStart", "s2");

      const summaries = deps.store.getProjectSummaries();
      expect(summaries.length).toBe(1);
      expect(summaries[0].project).toBe("project");
      expect(summaries[0].sessions.length).toBe(2);
    });

    it("reflects tool count in summary", () => {
      startSession(deps);
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });
      fire(deps, "PreToolUse", "s1", { tool_name: "Edit", tool_use_id: "t2" });

      const summaries = deps.store.getProjectSummaries();
      const sessionSummary = summaries[0].sessions.find(s => s.sessionId === "s1");
      expect(sessionSummary!.toolCount).toBe(2);
    });

    it("reflects session status in summary", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });

      let summaries = deps.store.getProjectSummaries();
      let sessionSummary = summaries[0].sessions[0];
      expect(sessionSummary.claudeStatus).toBe("responding");

      fire(deps, "Stop", "s1");
      summaries = deps.store.getProjectSummaries();
      sessionSummary = summaries[0].sessions[0];
      expect(sessionSummary.claudeStatus).toBe("idle");
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // Realistic multi-turn scenario
  // ────────────────────────────────────────────────────────────────────

  describe("Realistic multi-turn scenario", () => {
    it("handles a full realistic session lifecycle", () => {
      // Session starts
      fire(deps, "SessionStart", "s1", { slug: "my-slug", model: "claude-opus-4-6", branch: "main" }, 42);

      // Pre-prompt activity (turn 0)
      fire(deps, "PreToolUse", "s1", { tool_name: "Glob", tool_use_id: "t0", tool_input: {} });
      fire(deps, "PostToolUse", "s1", { tool_name: "Glob", tool_use_id: "t0", tool_response: ["a.ts"] } as HookData);

      // Turn 1: user prompt + tools
      fire(deps, "UserPromptSubmit", "s1", { prompt: "Fix the auth bug" });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_input: { file_path: "/src/auth.ts" },
        assistant_text: "Let me read the file",
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_response: "function auth() {}",
        post_tool_text: "I see the issue",
      } as HookData);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Edit",
        tool_use_id: "t2",
        tool_input: { file_path: "/src/auth.ts" },
        assistant_text: "I'll fix it now",
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "Edit",
        tool_use_id: "t2",
      });
      fire(deps, "Stop", "s1", {
        stop_text: "Fixed the auth bug",
        token_usage: makeTokenUsage({ input_tokens: 500, output_tokens: 200, cache_read_input_tokens: 100, cache_creation_input_tokens: 50 }),
      });

      // Turn 2: agent scenario
      fire(deps, "UserPromptSubmit", "s1", { prompt: "Now add tests" });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Task",
        tool_use_id: "task1",
        tool_input: { subagent_type: "Explore", prompt: "find test patterns" },
      });
      fire(deps, "SubagentStart", "s1", { agent_id: "a1", agent_type: "Explore" } as HookData);
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Glob",
        tool_use_id: "at1",
        parent_agent_id: "a1",
      });
      fire(deps, "PostToolUse", "s1", {
        tool_name: "Glob",
        tool_use_id: "at1",
        tool_response: ["test.ts"],
      } as HookData);
      fire(deps, "SubagentStop", "s1", {
        agent_id: "a1",
        agent_stop_text: "Found test patterns",
      });
      fire(deps, "Stop", "s1", {
        stop_text: "Tests added",
        token_usage: makeTokenUsage({ input_tokens: 1000, output_tokens: 400 }),
      });

      // Verify final state
      const session = deps.store.get("s1")!;
      expect(session.slug).toBe("my-slug");
      expect(session.modelName).toBe("opus");
      expect(session.branch).toBe("main");
      expect(session.pid).toBe(123); // subsequent fire() calls use default pid=123
      expect(session.currentTurn).toBe(2);
      expect(session.totalToolCount).toBe(5); // t0, t1, t2, task1, at1
      expect(session.turns.length).toBe(3); // turn 0, 1, 2
      expect(session.claudeStatus).toBe("idle");

      // File tracking
      expect(session.files["/src/auth.ts"]).toBeDefined();
      expect(session.files["/src/auth.ts"].ops).toContain("read");
      expect(session.files["/src/auth.ts"].ops).toContain("edit");

      // Agent
      expect(session.turns[2].agents.length).toBe(1);
      expect(session.turns[2].agents[0].status).toBe("done");
      expect(session.turns[2].agents[0].toolCount).toBe(1);

      // Turn freeze — Stop now freezes the current turn (freeze-on-Stop).
      expect(session.turns[0].frozen).toBe(true);
      expect(session.turns[1].frozen).toBe(true);
      expect(session.turns[2].frozen).toBe(true);

      // Stop text
      expect(session.turns[1].stopText).toBe("Fixed the auth bug");
      expect(session.turns[2].stopText).toBe("Tests added");
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // Patch content completeness (strengthened assertions)
  // ────────────────────────────────────────────────────────────────────

  describe("Patch content completeness", () => {
    it("set_meta patch contains all provided fields", () => {
      const patches = fire(deps, "SessionStart", "s1", {
        slug: "my-slug",
        branch: "main",
        tmux_session: "tmux-0",
      }, 42);

      const metaPatch = patches.find(p => p.op === "set_meta" && "slug" in p) as any;
      expect(metaPatch).toBeDefined();
      expect(metaPatch.slug).toBe("my-slug");
      expect(metaPatch.branch).toBe("main");
      expect(metaPatch.pid).toBe(42);
      expect(metaPatch.tmuxSession).toBe("tmux-0");
    });

    it("add_tool patch contains full tool object", () => {
      startSession(deps);
      const patches = fire(deps, "PreToolUse", "s1", {
        tool_name: "Read",
        tool_use_id: "t1",
        tool_input: { file_path: "/src/file.ts" },
        assistant_text: "Let me read",
      });

      const addToolPatch = patches.find(p => p.op === "add_tool") as any;
      expect(addToolPatch).toBeDefined();
      expect(addToolPatch.tool.toolUseId).toBe("t1");
      expect(addToolPatch.tool.name).toBe("Read");
      expect(addToolPatch.tool.status).toBe("running");
      expect(addToolPatch.tool.input).toEqual({ file_path: "/src/file.ts" });
      expect(addToolPatch.tool.assistantText).toBe("Let me read");
      expect(addToolPatch.turnNumber).toBe(0);
      expect(addToolPatch.stepIndex).toBeTypeOf("number");
    });

    it("add_prompt patch contains prompt text and type", () => {
      startSession(deps);
      const patches = fire(deps, "UserPromptSubmit", "s1", { prompt: "Fix the bug" });

      const promptPatch = patches.find(p => p.op === "add_prompt") as any;
      expect(promptPatch).toBeDefined();
      expect(promptPatch.prompt.text).toBe("Fix the bug");
      expect(promptPatch.prompt.type).toBe("user");
      expect(promptPatch.turnNumber).toBe(1);
    });

    it("set_turn_stop patch contains text and thinking", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      const patches = fire(deps, "Stop", "s1", {
        stop_text: "All done",
        stop_thinking: "I finished everything",
        token_usage: makeTokenUsage(),
      });

      const stopPatch = patches.find(p => p.op === "set_turn_stop") as any;
      expect(stopPatch).toBeDefined();
      expect(stopPatch.stopText).toBe("All done");
      expect(stopPatch.stopThinking).toBe("I finished everything");
      expect(stopPatch.turnNumber).toBe(1);
    });

    it("set_claude_status patch contains exact status value", () => {
      startSession(deps);
      const patches = fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });

      const statusPatch = patches.find(p => p.op === "set_claude_status") as any;
      expect(statusPatch).toBeDefined();
      expect(statusPatch.claudeStatus).toBe("responding");
    });

    it("add_agent patch contains full agent object", () => {
      startSession(deps);
      const patches = fire(deps, "SubagentStart", "s1", {
        agent_id: "a1",
        agent_type: "Explore",
      } as HookData);

      const agentPatch = patches.find(p => p.op === "add_agent") as any;
      expect(agentPatch).toBeDefined();
      expect(agentPatch.agent.agentId).toBe("a1");
      expect(agentPatch.agent.type).toBe("Explore");
      expect(agentPatch.agent.status).toBe("running");
      expect(agentPatch.agent.steps).toEqual([]);
      expect(agentPatch.agent.toolCount).toBe(0);
    });

    it("complete_agent patch includes transcriptPath", () => {
      startSession(deps);
      fire(deps, "SubagentStart", "s1", { agent_id: "a1", agent_type: "Explore" } as HookData);
      const patches = fire(deps, "SubagentStop", "s1", {
        agent_id: "a1",
        agent_stop_text: "done",
        agent_stop_thinking: "finished",
        agent_transcript_path: "/tmp/transcript.json",
      });

      const completePatch = patches.find(p => p.op === "complete_agent") as any;
      expect(completePatch).toBeDefined();
      expect(completePatch.agentId).toBe("a1");
      expect(completePatch.stopText).toBe("done");
      expect(completePatch.stopThinking).toBe("finished");
      expect(completePatch.transcriptPath).toBe("/tmp/transcript.json");
      expect(completePatch.duration).toBeTypeOf("number");

      // Also verify session state
      const agent = deps.store.get("s1")!.turns[0].agents[0];
      expect(agent.transcriptPath).toBe("/tmp/transcript.json");
    });

    it("track_file patch contains path and fileOp", () => {
      startSession(deps);
      const patches = fire(deps, "PreToolUse", "s1", {
        tool_name: "Edit",
        tool_use_id: "t1",
        tool_input: { file_path: "/src/auth.ts" },
      });

      const filePatch = patches.find(p => p.op === "track_file") as any;
      expect(filePatch).toBeDefined();
      expect(filePatch.path).toBe("/src/auth.ts");
      expect(filePatch.fileOp).toBe("edit");
    });

    it("set_token_usage patch contains full usage object", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      const usage = makeTokenUsage({ input_tokens: 500, output_tokens: 200 });
      const patches = fire(deps, "Stop", "s1", { token_usage: usage });

      const usagePatch = patches.find(p => p.op === "set_token_usage") as any;
      expect(usagePatch).toBeDefined();
      expect(usagePatch.usage.input_tokens).toBe(500);
      expect(usagePatch.usage.output_tokens).toBe(200);
    });

    it("set_plan patch contains plan content and filePath", () => {
      startSession(deps);
      fire(deps, "UserPromptSubmit", "s1", { prompt: "plan" });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "ExitPlanMode",
        tool_use_id: "t1",
        tool_input: { plan: "1. Do X\n2. Do Y" },
      });
      const patches = fire(deps, "PostToolUse", "s1", {
        tool_name: "ExitPlanMode",
        tool_use_id: "t1",
        tool_input: { plan: "1. Do X\n2. Do Y" },
        tool_response: { filePath: "/plan.md" },
      } as HookData);

      const planPatch = patches.find(p => p.op === "set_plan") as any;
      expect(planPatch).toBeDefined();
      expect(planPatch.plan.content).toBe("1. Do X\n2. Do Y");
      expect(planPatch.plan.filePath).toBe("/plan.md");
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // resetSession patch emission (bug fix)
  // ────────────────────────────────────────────────────────────────────

  describe("resetSession emits patches", () => {
    it("SessionStart on existing session emits reset patches", () => {
      // First session
      startSession(deps, "s1", { slug: "original" });
      fire(deps, "UserPromptSubmit", "s1", { prompt: "do stuff" });
      fire(deps, "PreToolUse", "s1", { tool_name: "Read", tool_use_id: "t1" });

      const session = deps.store.get("s1")!;
      expect(session.currentTurn).toBe(1);
      expect(session.totalToolCount).toBe(1);

      // /clear fires SessionEnd + SessionStart with same session ID
      fire(deps, "SessionEnd", "s1");
      const patches = fire(deps, "SessionStart", "s1");

      // Should contain reset patches
      expect(patches).toContainEqual({ op: "set_status", status: "active" });
      expect(patches).toContainEqual({ op: "set_claude_status", claudeStatus: "idle" });
      expect(patches).toContainEqual({ op: "set_plan", plan: null });
      expect(patches).toContainEqual({ op: "set_streaming_text", text: null });
      expect(patches.some(p => p.op === "set_token_usage")).toBe(true);

      // Session state should be reset
      const resetSession = deps.store.get("s1")!;
      expect(resetSession.currentTurn).toBe(0);
      expect(resetSession.totalToolCount).toBe(0);
      expect(resetSession.turns.length).toBe(1);
      expect(resetSession.plan).toBeNull();
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // Compaction marker (pi compaction_end → CompactionMarker)
  // ────────────────────────────────────────────────────────────────────

  describe("Compaction event handler", () => {
    it("records a CompactionMarker on the session with the correct turn number", () => {
      startSession(deps, "s1", { source: "pi" });
      fire(deps, "UserPromptSubmit", "s1", { prompt: "do work", source: "pi" });
      // currentTurn is now 1.
      const patches = fire(deps, "Compaction", "s1", {
        source: "pi",
        compaction_reason: "threshold",
        compaction_aborted: false,
        compaction_tokens_before: 47000,
        compaction_summary: "Earlier conversation summarized.",
      } as HookData);

      const session = deps.store.get("s1")!;
      expect(session.compactions).toHaveLength(1);
      expect(session.compactions[0]).toMatchObject({
        reason: "threshold",
        turnNumber: 1,
        tokensBefore: 47000,
        summary: "Earlier conversation summarized.",
        aborted: false,
      });
      expect(session.compactions[0].timestamp).toBeGreaterThan(0);

      const compactionPatch = patches.find((p) => p.op === "add_compaction");
      expect(compactionPatch).toBeDefined();
      // @ts-expect-error narrowing through patch union
      expect(compactionPatch?.marker.reason).toBe("threshold");
    });

    it("does NOT move turn boundaries (mid-stream compaction between PreToolUse and PostToolUse)", () => {
      startSession(deps, "s1", { source: "pi" });
      fire(deps, "UserPromptSubmit", "s1", { prompt: "long build", source: "pi" });
      fire(deps, "PreToolUse", "s1", {
        tool_name: "Bash",
        tool_use_id: "b1",
        tool_input: { command: "make build" },
      });
      const turnsBefore = deps.store.get("s1")!.turns.length;
      const currentTurnBefore = deps.store.get("s1")!.currentTurn;

      fire(deps, "Compaction", "s1", {
        source: "pi",
        compaction_reason: "overflow",
        compaction_tokens_before: 99000,
        compaction_summary: "Compacted",
      } as HookData);

      const session = deps.store.get("s1")!;
      // Turn count and currentTurn must be unchanged.
      expect(session.turns.length).toBe(turnsBefore);
      expect(session.currentTurn).toBe(currentTurnBefore);
      // The running tool is still in place.
      expect(session.toolIndex["b1"]).toBeDefined();
      // Marker is attached to the current turn (the in-progress one).
      expect(session.compactions[0].turnNumber).toBe(currentTurnBefore);

      // PostToolUse can still complete the tool.
      fire(deps, "PostToolUse", "s1", {
        tool_name: "Bash",
        tool_use_id: "b1",
        tool_response: { stdout: "done" },
      });
      const post = deps.store.get("s1")!;
      const loc = post.toolIndex["b1"]!;
      const tool = post.turns[loc.turnNumber].steps[loc.stepIndex].tools[loc.toolIndex];
      expect(tool.status).toBe("done");
    });

    it("records aborted compactions with aborted=true and safely handles missing summary", () => {
      startSession(deps, "s1", { source: "pi" });
      fire(deps, "Compaction", "s1", {
        source: "pi",
        compaction_reason: "manual",
        compaction_aborted: true,
        // No tokens_before, no summary.
      } as HookData);

      const session = deps.store.get("s1")!;
      expect(session.compactions[0]).toMatchObject({
        reason: "manual",
        aborted: true,
        tokensBefore: null,
        summary: null,
      });
    });

    it("compaction during turn 0 (pre-prompt) records turnNumber=-1 sentinel", () => {
      startSession(deps, "s1", { source: "pi" });
      // No UserPromptSubmit yet — currentTurn is 0 (pre-prompt activity).
      fire(deps, "Compaction", "s1", {
        source: "pi",
        compaction_reason: "manual",
      } as HookData);
      const session = deps.store.get("s1")!;
      expect(session.compactions[0].turnNumber).toBe(-1);
    });

    it("defaults reason to 'unknown' when not provided", () => {
      startSession(deps, "s1", { source: "pi" });
      fire(deps, "Compaction", "s1", { source: "pi" } as HookData);
      expect(deps.store.get("s1")!.compactions[0].reason).toBe("unknown");
    });
  });

  // ────────────────────────────────────────────────────────────────────
  // Role-based inbox suppression + pi-fault "attention" item
  // (handleTurnClose / handleStop)
  //
  // Headless/swarm sessions (role: "worker" | "coordinator") should not
  // generate per-turn "idle" inbox items — the orchestrator already knows
  // the turn ended. A detected pi fault (stop_reason: "error" | "aborted")
  // ALWAYS surfaces as a non-actionable "attention" item, regardless of
  // role, so a read-only client can see WHY a swarm worker stopped.
  // ────────────────────────────────────────────────────────────────────

  describe("TurnClose / Stop inbox suppression + attention fault", () => {
    it("TurnClose does NOT create an idle inbox item for role:'worker' sessions", () => {
      startSession(deps, "s1");
      deps.store.get("s1")!.role = "worker";
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "TurnClose", "s1", { stop_text: "did the work" });
      expect(deps.inbox.all().filter((i) => i.type === "idle")).toHaveLength(0);
    });

    it("TurnClose does NOT create an idle inbox item for role:'coordinator' sessions", () => {
      startSession(deps, "s1");
      deps.store.get("s1")!.role = "coordinator";
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "TurnClose", "s1", { stop_text: "did the work" });
      expect(deps.inbox.all().filter((i) => i.type === "idle")).toHaveLength(0);
    });

    it("regression: TurnClose on a default 'interactive' session STILL creates an idle inbox item", () => {
      startSession(deps, "s1");
      // createSession's pinned default for non-pi sources is "interactive".
      expect(deps.store.get("s1")!.role).toBe("interactive");
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "TurnClose", "s1", { stop_text: "done" });
      const items = deps.inbox.all().filter((i) => i.type === "idle");
      expect(items).toHaveLength(1);
      expect(items[0].sessionId).toBe("s1");
    });

    it("TurnClose with stop_reason:'error' produces a non-actionable attention item", () => {
      startSession(deps, "s1");
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "TurnClose", "s1", {
        stop_text: "rate limit hit",
        stop_reason: "error",
      });
      const items = deps.inbox.all().filter((i) => i.type === "attention");
      expect(items).toHaveLength(1);
      expect(items[0].actionable).toBe(false);
      expect(items[0].data.stopReason).toBe("error");
      expect(items[0].data.stopText).toBe("rate limit hit");
    });

    it("TurnClose with stop_reason:'aborted' produces a non-actionable attention item", () => {
      startSession(deps, "s1");
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "TurnClose", "s1", { stop_reason: "aborted" });
      const items = deps.inbox.all().filter((i) => i.type === "attention");
      expect(items).toHaveLength(1);
      expect(items[0].actionable).toBe(false);
      expect(items[0].data.stopReason).toBe("aborted");
    });

    it("TurnClose stop_reason:'toolUse' does NOT produce an attention item (not a fault)", () => {
      startSession(deps, "s1");
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "TurnClose", "s1", { stop_text: "ok", stop_reason: "toolUse" });
      expect(deps.inbox.all().filter((i) => i.type === "attention")).toHaveLength(0);
    });

    it("TurnClose attention item fires regardless of role (worker + error)", () => {
      startSession(deps, "s1");
      deps.store.get("s1")!.role = "worker";
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "TurnClose", "s1", { stop_text: "boom", stop_reason: "error" });
      // Worker → no idle
      expect(deps.inbox.all().filter((i) => i.type === "idle")).toHaveLength(0);
      // But the fault always surfaces
      const items = deps.inbox.all().filter((i) => i.type === "attention");
      expect(items).toHaveLength(1);
      expect(items[0].actionable).toBe(false);
    });

    it("Stop mirrors the role-based idle suppression for consistency", () => {
      startSession(deps, "s1");
      deps.store.get("s1")!.role = "worker";
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "Stop", "s1", { stop_text: "done" });
      expect(deps.inbox.all().filter((i) => i.type === "idle")).toHaveLength(0);
    });

    it("Stop still creates an idle item for default 'interactive' sessions (regression)", () => {
      startSession(deps, "s1");
      expect(deps.store.get("s1")!.role).toBe("interactive");
      fire(deps, "UserPromptSubmit", "s1", { prompt: "go" });
      fire(deps, "Stop", "s1", { stop_text: "done" });
      expect(deps.inbox.all().filter((i) => i.type === "idle")).toHaveLength(1);
    });
  });
});
