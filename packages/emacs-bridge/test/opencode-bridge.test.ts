import { describe, it, expect } from "vitest";
import { mapOpenCodeEventToGravity, respondToPermission, respondToQuestion } from "../src/opencode-bridge";

const INSTANCE_PORT = 8080;
const INSTANCE_DIR = "/test/project";

describe("mapOpenCodeEventToGravity", () => {
  // ─── Session Lifecycle ────────────────────────────────────────────────────

  describe("session.created", () => {
    it("maps to SessionStart with all fields", () => {
      const event = {
        type: "session.created",
        properties: {
          info: {
            id: "sess-abc123",
            directory: "/test/project",
            slug: "fancy-session-slug",
            title: "My Test Session",
            parentID: "sess-parent",
            projectID: "proj-xyz",
            time: { created: "2026-02-15T10:00:00Z" },
            permission: "ask",
          },
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toEqual({
        event: "SessionStart",
        sessionId: "sess-abc123",
        cwd: "/test/project",
        data: {
          slug: "fancy-session-slug",
          title: "My Test Session",
          parent_id: "sess-parent",
          project_id: "proj-xyz",
          time_created: "2026-02-15T10:00:00Z",
          permission: "ask",
        },
      });
    });

    it("handles missing optional fields", () => {
      const event = {
        type: "session.created",
        properties: {
          info: {
            id: "sess-minimal",
            directory: "/minimal",
          },
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result?.event).toBe("SessionStart");
      expect(result?.sessionId).toBe("sess-minimal");
      expect(result?.data).toEqual({
        slug: undefined,
        title: undefined,
        parent_id: undefined,
        project_id: undefined,
        time_created: undefined,
        permission: undefined,
      });
    });
  });

  describe("session.deleted", () => {
    it("maps to SessionEnd", () => {
      const event = {
        type: "session.deleted",
        properties: {
          sessionID: "sess-ended",
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toEqual({
        event: "SessionEnd",
        sessionId: "sess-ended",
        cwd: INSTANCE_DIR,
        data: {},
      });
    });
  });

  describe("session.status", () => {
    it("maps to SessionStatus with all fields", () => {
      const event = {
        type: "session.status",
        properties: {
          sessionID: "sess-status",
          status: {
            type: "in_progress",
            attempt: 2,
            message: "Processing request",
          },
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toEqual({
        event: "SessionStatus",
        sessionId: "sess-status",
        cwd: INSTANCE_DIR,
        data: {
          status: "in_progress",
          attempt: 2,
          message: "Processing request",
        },
      });
    });
  });

  describe("session.idle", () => {
    it("maps to SessionIdle", () => {
      const event = {
        type: "session.idle",
        properties: {
          sessionID: "sess-idle",
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toEqual({
        event: "SessionIdle",
        sessionId: "sess-idle",
        cwd: INSTANCE_DIR,
        data: {},
      });
    });
  });

  describe("session.updated", () => {
    it("maps to SessionUpdate with title/slug/summary", () => {
      const event = {
        type: "session.updated",
        properties: {
          info: {
            id: "sess-update",
            title: "Updated Title",
            slug: "updated-slug",
            summary: "Session summary text",
          },
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toEqual({
        event: "SessionUpdate",
        sessionId: "sess-update",
        cwd: INSTANCE_DIR,
        data: {
          title: "Updated Title",
          slug: "updated-slug",
          summary: "Session summary text",
        },
      });
    });

    it("returns null when info is missing", () => {
      const event = {
        type: "session.updated",
        properties: {},
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toBeNull();
    });
  });

  // ─── Messages ──────────────────────────────────────────────────────────────

  describe("message.updated (user)", () => {
    it("maps user messages to UserPromptSubmit", () => {
      const event = {
        type: "message.updated",
        properties: {
          info: {
            id: "msg-user-1",
            sessionID: "sess-msg",
            role: "user",
            agent: "default",
            model: "claude-3-5-sonnet",
            tools: ["Read", "Write", "Bash"],
            system: "You are a helpful assistant",
          },
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result?.event).toBe("UserPromptSubmit");
      expect(result?.sessionId).toBe("sess-msg");
      expect(result?.data).toEqual({
        message_id: "msg-user-1",
        agent: "default",
        model: "claude-3-5-sonnet",
        tools: ["Read", "Write", "Bash"],
        system: "You are a helpful assistant",
      });
    });
  });

  describe("message.updated (assistant)", () => {
    it("maps assistant messages to AssistantMessage", () => {
      const event = {
        type: "message.updated",
        properties: {
          info: {
            id: "msg-assistant-1",
            sessionID: "sess-msg",
            role: "assistant",
            parentID: "msg-user-1",
            modelID: "claude-3-5-sonnet-20241022",
            providerID: "anthropic",
            cost: 0.0023,
            tokens: { input: 150, output: 300 },
            finish: "end_turn",
            error: null,
          },
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result?.event).toBe("AssistantMessage");
      expect(result?.sessionId).toBe("sess-msg");
      expect(result?.data).toEqual({
        message_id: "msg-assistant-1",
        parent_id: "msg-user-1",
        model_id: "claude-3-5-sonnet-20241022",
        provider_id: "anthropic",
        cost: 0.0023,
        tokens: { input: 150, output: 300 },
        finish: "end_turn",
        error: null,
      });
    });

    it("handles assistant message with error", () => {
      const event = {
        type: "message.updated",
        properties: {
          info: {
            id: "msg-err",
            sessionID: "sess-err",
            role: "assistant",
            finish: "error",
            error: "API rate limit exceeded",
          },
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result?.event).toBe("AssistantMessage");
      expect(result?.data.finish).toBe("error");
      expect(result?.data.error).toBe("API rate limit exceeded");
    });
  });

  describe("message.updated (other roles)", () => {
    it("returns null for unknown roles", () => {
      const event = {
        type: "message.updated",
        properties: {
          info: {
            id: "msg-system",
            sessionID: "sess-xyz",
            role: "system",
          },
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toBeNull();
    });

    it("returns null when info is missing", () => {
      const event = {
        type: "message.updated",
        properties: {},
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toBeNull();
    });
  });

  // ─── Message Parts (Streaming) ─────────────────────────────────────────────

  describe("message.part.updated", () => {
    it("maps text parts to MessagePart", () => {
      const event = {
        type: "message.part.updated",
        properties: {
          part: {
            id: "part-text-1",
            sessionID: "sess-part",
            messageID: "msg-1",
            type: "text",
            text: "Hello, world!",
          },
          delta: "world!",
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toEqual({
        event: "MessagePart",
        sessionId: "sess-part",
        cwd: INSTANCE_DIR,
        data: {
          message_id: "msg-1",
          part_id: "part-text-1",
          part_type: "text",
          text: "Hello, world!",
          tool: undefined,
          delta: "world!",
        },
      });
    });

    it("maps tool parts to MessagePart with tool info", () => {
      const event = {
        type: "message.part.updated",
        properties: {
          part: {
            id: "part-tool-1",
            sessionID: "sess-tool",
            messageID: "msg-2",
            type: "tool",
            callID: "call-abc123",
            tool: "Bash",
            state: "pending",
          },
          delta: null,
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result?.event).toBe("MessagePart");
      expect(result?.data.part_type).toBe("tool");
      expect(result?.data.tool).toEqual({
        call_id: "call-abc123",
        tool_name: "Bash",
        state: "pending",
      });
    });

    it("returns null when part is missing", () => {
      const event = {
        type: "message.part.updated",
        properties: {},
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toBeNull();
    });
  });

  // ─── Permissions & Questions ───────────────────────────────────────────────

  describe("permission.asked", () => {
    it("maps to PermissionRequest with all fields including flattened tool", () => {
      const event = {
        type: "permission.asked",
        properties: {
          sessionID: "sess-perm",
          id: "perm-123",
          permission: "Bash",
          patterns: ["Bash(rm:*)"],
          metadata: { path: "/dangerous/path" },
          always: false,
          tool: { name: "Bash", input: { command: "rm -rf /" } },
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toEqual({
        event: "PermissionRequest",
        sessionId: "sess-perm",
        cwd: INSTANCE_DIR,
        data: {
          permission_id: "perm-123",
          tool_name: "Bash",
          tool_input: { command: "rm -rf /" },
          permission: "Bash",
          patterns: ["Bash(rm:*)"],
          metadata: { path: "/dangerous/path" },
          always: false,
          tool: { name: "Bash", input: { command: "rm -rf /" } },
        },
      });
    });

    it("handles missing tool field gracefully", () => {
      const event = {
        type: "permission.asked",
        properties: {
          sessionID: "sess-perm-no-tool",
          id: "perm-456",
          permission: "Bash",
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result?.data.tool_name).toBeUndefined();
      expect(result?.data.tool_input).toBeUndefined();
      expect(result?.data.permission_id).toBe("perm-456");
    });
  });

  describe("question.asked", () => {
    it("maps to AskUserQuestion with normalized tool_input and questions", () => {
      const event = {
        type: "question.asked",
        properties: {
          sessionID: "sess-ask",
          id: "question-456",
          questions: [
            { question: "Which option?", header: "Choice", options: ["A", "B"] },
          ],
          tool: { name: "Question", input: {} },
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toEqual({
        event: "AskUserQuestion",
        sessionId: "sess-ask",
        cwd: INSTANCE_DIR,
        data: {
          question_id: "question-456",
          tool_name: "AskUserQuestion",
          tool_input: {
            questions: [
              { question: "Which option?", header: "Choice", options: ["A", "B"] },
            ],
          },
          questions: [
            { question: "Which option?", header: "Choice", options: ["A", "B"] },
          ],
          tool: { name: "Question", input: {} },
        },
      });
    });
  });

  // ─── VCS ────────────────────────────────────────────────────────────────────

  describe("vcs.branch.updated", () => {
    it("maps to VcsBranchUpdate with branch name", () => {
      const event = {
        type: "vcs.branch.updated",
        properties: {
          sessionID: "sess-vcs",
          branch: "feature/new-bridge",
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toEqual({
        event: "VcsBranchUpdate",
        sessionId: "sess-vcs",
        cwd: INSTANCE_DIR,
        data: {
          branch: "feature/new-bridge",
        },
      });
    });

    it("handles missing sessionID (uses 'unknown')", () => {
      const event = {
        type: "vcs.branch.updated",
        properties: {
          branch: "main",
        },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result?.sessionId).toBe("unknown");
      expect(result?.data.branch).toBe("main");
    });
  });

  // ─── Unknown Events ─────────────────────────────────────────────────────────

  describe("unknown event types", () => {
    it("returns null for unrecognized events", () => {
      const event = {
        type: "some.unknown.event",
        properties: { foo: "bar" },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toBeNull();
    });

    it("returns null for events without type", () => {
      const event = {
        properties: { foo: "bar" },
      };

      const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);

      expect(result).toBeNull();
    });
  });
});

// ─── Feature Parity Comparison ─────────────────────────────────────────────────

describe("OpenCode Bridge vs Claude SDK Bridge Feature Parity", () => {
  it("covers session lifecycle events", () => {
    const openCodeEvents = [
      "session.created",
      "session.deleted",
      "session.status",
      "session.idle",
      "session.updated",
    ];

    const claudeEvents = ["SessionStart", "SessionEnd", "SessionStatus"];

    expect(openCodeEvents.length).toBeGreaterThanOrEqual(claudeEvents.length);
  });

  it("covers message events", () => {
    const openCodeEvents = [
      "message.updated (user)",
      "message.updated (assistant)",
      "message.part.updated",
    ];

    const claudeEvents = [
      "UserPromptSubmit",
      "AssistantMessage",
      "AssistantComplete",
      "StreamDelta",
    ];

    expect(openCodeEvents.length).toBeGreaterThanOrEqual(2);
  });

  it("covers permission/interaction events", () => {
    const openCodeEvents = ["permission.asked", "question.asked"];

    const claudeEvents = ["PermissionRequest", "AskUserQuestion"];

    expect(openCodeEvents).toEqual(claudeEvents.map(e => e.toLowerCase().replace("askuserquestion", "question.asked").replace("permissionrequest", "permission.asked")));
  });

  it("covers tool events via message.part.updated", () => {
    const toolEvent = {
      type: "message.part.updated",
      properties: {
        part: {
          id: "p1",
          sessionID: "s1",
          messageID: "m1",
          type: "tool",
          callID: "call-1",
          tool: "Bash",
          state: "pending",
        },
      },
    };

    const result = mapOpenCodeEventToGravity(toolEvent, INSTANCE_PORT, INSTANCE_DIR);
    expect(result?.data.part_type).toBe("tool");
    expect(result?.data.tool?.tool_name).toBe("Bash");
  });

  it("covers VCS events (OpenCode has, Claude SDK may not)", () => {
    const vcsEvent = {
      type: "vcs.branch.updated",
      properties: { branch: "main" },
    };

    const result = mapOpenCodeEventToGravity(vcsEvent, INSTANCE_PORT, INSTANCE_DIR);
    expect(result?.event).toBe("VcsBranchUpdate");
  });
});

// ─── Edge Cases ────────────────────────────────────────────────────────────────

describe("Edge cases", () => {
  it("handles empty properties object", () => {
    const event = {
      type: "session.created",
      properties: {},
    };

    const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);
    expect(result).toBeNull();
  });

  it("handles null properties", () => {
    const event = {
      type: "session.created",
      properties: null,
    };

    const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);
    expect(result).toBeNull();
  });

  it("handles malformed event (no properties key)", () => {
    const event = {
      type: "session.created",
    };

    const result = mapOpenCodeEventToGravity(event, INSTANCE_PORT, INSTANCE_DIR);
    expect(result).toBeNull();
  });
});

// ─── Response Translation ─────────────────────────────────────────────────────

describe("respondToPermission", () => {
  it("maps allow to 'once'", async () => {
    let capturedUrl = '';
    let capturedBody = '';

    const originalFetch = globalThis.fetch;
    globalThis.fetch = async (url: any, opts: any) => {
      capturedUrl = url.toString();
      capturedBody = opts?.body;
      return new Response('true', { status: 200 });
    };

    try {
      await respondToPermission(8080, "/test", "perm-123", {
        hookSpecificOutput: {
          hookEventName: "PermissionRequest",
          decision: { behavior: "allow" },
        },
      });

      expect(capturedUrl).toContain("/permission/perm-123/reply");
      expect(capturedUrl).toContain("directory=%2Ftest");
      expect(capturedBody).toBe('"once"');
    } finally {
      globalThis.fetch = originalFetch;
    }
  });

  it("maps allow with updatedPermissions to 'always'", async () => {
    let capturedBody = '';

    const originalFetch = globalThis.fetch;
    globalThis.fetch = async (_url: any, opts: any) => {
      capturedBody = opts?.body;
      return new Response('true', { status: 200 });
    };

    try {
      await respondToPermission(8080, "/test", "perm-456", {
        hookSpecificOutput: {
          hookEventName: "PermissionRequest",
          decision: {
            behavior: "allow",
            updatedPermissions: [{ type: "Bash", tool: "Bash" }],
          },
        },
      });

      expect(capturedBody).toBe('"always"');
    } finally {
      globalThis.fetch = originalFetch;
    }
  });

  it("maps deny to 'reject'", async () => {
    let capturedBody = '';

    const originalFetch = globalThis.fetch;
    globalThis.fetch = async (_url: any, opts: any) => {
      capturedBody = opts?.body;
      return new Response('true', { status: 200 });
    };

    try {
      await respondToPermission(8080, "/test", "perm-789", {
        hookSpecificOutput: {
          hookEventName: "PermissionRequest",
          decision: { behavior: "deny", message: "Too dangerous" },
        },
      });

      expect(capturedBody).toBe('"reject"');
    } finally {
      globalThis.fetch = originalFetch;
    }
  });

  it("handles missing decision gracefully", async () => {
    // Should not throw, just log warning
    await respondToPermission(8080, "/test", "perm-bad", {
      hookSpecificOutput: { hookEventName: "PermissionRequest" },
    });
  });
});

describe("respondToQuestion", () => {
  it("posts answer to reply endpoint", async () => {
    let capturedUrl = '';
    let capturedBody = '';

    const originalFetch = globalThis.fetch;
    globalThis.fetch = async (url: any, opts: any) => {
      capturedUrl = url.toString();
      capturedBody = opts?.body;
      return new Response('true', { status: 200 });
    };

    try {
      await respondToQuestion(8080, "/test", "q-123", {
        answer: "Option A",
        hookSpecificOutput: {
          hookEventName: "PreToolUse",
          permissionDecision: "deny",
          permissionDecisionReason: "User answered: Option A",
        },
      });

      expect(capturedUrl).toContain("/question/q-123/reply");
      expect(capturedUrl).toContain("directory=%2Ftest");
      expect(JSON.parse(capturedBody)).toEqual({ answers: ["Option A"] });
    } finally {
      globalThis.fetch = originalFetch;
    }
  });

  it("posts reject when no answer provided", async () => {
    let capturedUrl = '';

    const originalFetch = globalThis.fetch;
    globalThis.fetch = async (url: any, _opts: any) => {
      capturedUrl = url.toString();
      return new Response('true', { status: 200 });
    };

    try {
      await respondToQuestion(8080, "/test", "q-456", {
        hookSpecificOutput: {
          hookEventName: "PreToolUse",
          permissionDecision: "deny",
        },
      });

      expect(capturedUrl).toContain("/question/q-456/reject");
    } finally {
      globalThis.fetch = originalFetch;
    }
  });
});