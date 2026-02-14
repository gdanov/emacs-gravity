import { describe, it, expect } from "vitest";
import { join } from "path";
import { readFileSync } from "fs";
import {
  validateEnvelope,
  validateEventData,
  buildClaudeCodeEnvelope,
  buildOpenCodeEnvelope,
  VALID_ENVELOPE_KEYS,
  EVENT_DATA_SCHEMAS,
  SHARED_EVENTS,
} from "./socket-contract";
import { enrichEvent } from "../src/enrichment";
import { mapOpenCodeEventToGravity } from "../src/opencode-bridge";

const FIXTURES = join(__dirname, "fixtures");
const EVENTS = join(FIXTURES, "events");

function loadEvent(filename: string): any {
  const raw = JSON.parse(readFileSync(join(EVENTS, filename), "utf-8"));
  if (raw.transcript_path && raw.transcript_path.startsWith("__FIXTURE__/")) {
    raw.transcript_path = join(FIXTURES, raw.transcript_path.replace("__FIXTURE__/", ""));
  }
  return raw;
}

// ─── Part 1: Envelope compliance ─────────────────────────────────────────────

describe("Envelope compliance", () => {
  describe("Claude Code bridge envelopes", () => {
    const cases: Array<{ event: string; fixture?: string; rawData?: any }> = [
      { event: "SessionStart", rawData: { session_id: "sess-1", cwd: "/proj" } },
      { event: "SessionEnd", rawData: { session_id: "sess-1", cwd: "/proj" } },
      { event: "PreToolUse", fixture: "pretool_bash.json" },
      { event: "PostToolUse", fixture: "posttool_read.json" },
      { event: "SubagentStart", fixture: "subagent_start.json" },
      { event: "Stop", fixture: "stop_first.json" },
      { event: "Notification", rawData: { session_id: "sess-1", cwd: "/proj", title: "Done", message: "ok" } },
    ];

    for (const { event, fixture, rawData } of cases) {
      it(`${event} produces a valid envelope`, () => {
        const data = fixture ? loadEvent(fixture) : rawData;
        const enriched = enrichEvent({ ...data }, event);
        const envelope = buildClaudeCodeEnvelope(
          event,
          enriched.session_id || "unknown",
          enriched.cwd || "",
          null,
          enriched,
        );
        const result = validateEnvelope(envelope);
        expect(result.errors).toEqual([]);
        expect(result.valid).toBe(true);
      });
    }
  });

  describe("OpenCode bridge envelopes", () => {
    const ocEvents: Array<{ type: string; properties: any }> = [
      {
        type: "session.created",
        properties: {
          info: { id: "oc-1", directory: "/proj", slug: "test-slug", title: "Test" },
        },
      },
      { type: "session.deleted", properties: { sessionID: "oc-1" } },
      {
        type: "session.status",
        properties: { sessionID: "oc-1", status: { type: "busy", attempt: 1, message: "" } },
      },
      { type: "session.idle", properties: { sessionID: "oc-1" } },
      {
        type: "message.updated",
        properties: {
          info: { id: "msg-1", sessionID: "oc-1", role: "user", agent: "default" },
        },
      },
      {
        type: "message.updated",
        properties: {
          info: { id: "msg-2", sessionID: "oc-1", role: "assistant", parentID: "msg-1", modelID: "m1" },
        },
      },
      {
        type: "message.part.updated",
        properties: {
          part: { id: "p-1", sessionID: "oc-1", messageID: "msg-2", type: "text", text: "hello" },
        },
      },
      {
        type: "permission.asked",
        properties: { id: "perm-1", sessionID: "oc-1", permission: "write", tool: "Edit" },
      },
      {
        type: "question.asked",
        properties: { id: "q-1", sessionID: "oc-1", questions: [{ text: "which?" }], tool: "AskUserQuestion" },
      },
      { type: "vcs.branch.updated", properties: { sessionID: "oc-1", branch: "main" } },
      {
        type: "session.updated",
        properties: { info: { id: "oc-1", title: "Updated", slug: "new-slug" } },
      },
    ];

    for (const ocEvent of ocEvents) {
      it(`${ocEvent.type} produces a valid envelope`, () => {
        const mapped = mapOpenCodeEventToGravity(ocEvent, 3000, "/proj");
        expect(mapped).not.toBeNull();
        const envelope = buildOpenCodeEnvelope(
          mapped!.event,
          mapped!.sessionId,
          mapped!.cwd,
          mapped!.data,
          3000,
          "/proj",
        );
        const result = validateEnvelope(envelope);
        expect(result.errors).toEqual([]);
        expect(result.valid).toBe(true);
      });
    }
  });

  describe("Envelope validation rejects malformed messages", () => {
    it("rejects null", () => {
      expect(validateEnvelope(null).valid).toBe(false);
    });

    it("rejects array", () => {
      expect(validateEnvelope([]).valid).toBe(false);
    });

    it("rejects missing event", () => {
      const result = validateEnvelope({ session_id: "s", cwd: "/", data: {} });
      expect(result.valid).toBe(false);
      expect(result.errors).toContain("event must be a non-empty string");
    });

    it("rejects empty event string", () => {
      const result = validateEnvelope({ event: "", session_id: "s", cwd: "/", data: {} });
      expect(result.valid).toBe(false);
    });

    it("rejects missing session_id", () => {
      const result = validateEnvelope({ event: "Test", cwd: "/", data: {} });
      expect(result.valid).toBe(false);
      expect(result.errors).toContain("session_id must be a non-empty string");
    });

    it("rejects data as null", () => {
      const result = validateEnvelope({ event: "Test", session_id: "s", cwd: "/", data: null });
      expect(result.valid).toBe(false);
      expect(result.errors).toContain("data must be a non-null, non-array object");
    });

    it("rejects data as array", () => {
      const result = validateEnvelope({ event: "Test", session_id: "s", cwd: "/", data: [] });
      expect(result.valid).toBe(false);
    });

    it("rejects unknown top-level key", () => {
      const result = validateEnvelope({ event: "Test", session_id: "s", cwd: "/", data: {}, bogus: true });
      expect(result.valid).toBe(false);
      expect(result.errors[0]).toContain("Unknown top-level key: bogus");
    });

    it("rejects pid as string", () => {
      const result = validateEnvelope({ event: "Test", session_id: "s", cwd: "/", data: {}, pid: "bad" });
      expect(result.valid).toBe(false);
      expect(result.errors).toContain("pid must be a number or null");
    });

    it("accepts pid as null", () => {
      const result = validateEnvelope({ event: "Test", session_id: "s", cwd: "/", data: {}, pid: null });
      expect(result.valid).toBe(true);
    });

    it("accepts pid as number", () => {
      const result = validateEnvelope({ event: "Test", session_id: "s", cwd: "/", data: {}, pid: 1234 });
      expect(result.valid).toBe(true);
    });
  });
});

// ─── Part 2: Claude Code bridge event payloads ──────────────────────────────

describe("Claude Code bridge event payloads", () => {
  it("PreToolUse has required data fields", () => {
    const raw = loadEvent("pretool_bash.json");
    const enriched = enrichEvent({ ...raw }, "PreToolUse");
    const result = validateEventData("PreToolUse", enriched);
    expect(result.errors).toEqual([]);
    expect(enriched.tool_use_id).toBe("toolu_0117AHsMTSWiUkLVBhQ43wzv");
    expect(enriched.tool_name).toBe("Bash");
    expect(enriched.tool_input).toBeDefined();
  });

  it("PostToolUse has required data fields", () => {
    const raw = loadEvent("posttool_read.json");
    const enriched = enrichEvent({ ...raw }, "PostToolUse");
    const result = validateEventData("PostToolUse", enriched);
    expect(result.errors).toEqual([]);
    expect(enriched.tool_use_id).toBeTruthy();
    expect(enriched.tool_name).toBe("Read");
  });

  it("SubagentStart has required data fields", () => {
    const raw = loadEvent("subagent_start.json");
    const enriched = enrichEvent({ ...raw }, "SubagentStart");
    const result = validateEventData("SubagentStart", enriched);
    expect(result.errors).toEqual([]);
    expect(enriched.agent_id).toBe("agent_abc123");
  });

  it("SubagentStop has required data fields", () => {
    const raw = loadEvent("subagent_stop.json");
    const enriched = enrichEvent({ ...raw }, "SubagentStop");
    const result = validateEventData("SubagentStop", enriched);
    expect(result.errors).toEqual([]);
    expect(enriched.agent_id).toBe("agent_abc123");
  });

  it("Stop has valid data fields", () => {
    const raw = loadEvent("stop_first.json");
    const enriched = enrichEvent({ ...raw }, "Stop");
    const result = validateEventData("Stop", enriched);
    expect(result.errors).toEqual([]);
    // stop_text is optional but should be present from fixture
    expect(enriched.stop_text).toBeTruthy();
    expect(enriched.token_usage).toBeDefined();
  });

  it("SessionStart passes with no required data fields", () => {
    const enriched = enrichEvent({ session_id: "s1", cwd: "/proj" }, "SessionStart");
    const result = validateEventData("SessionStart", enriched);
    expect(result.errors).toEqual([]);
  });

  it("SessionEnd passes with no required data fields", () => {
    const enriched = enrichEvent({ session_id: "s1", cwd: "/proj" }, "SessionEnd");
    const result = validateEventData("SessionEnd", enriched);
    expect(result.errors).toEqual([]);
  });

  it("Notification passes with no required data fields", () => {
    const enriched = enrichEvent(
      { session_id: "s1", cwd: "/proj", title: "Info", message: "msg" },
      "Notification",
    );
    const result = validateEventData("Notification", enriched);
    expect(result.errors).toEqual([]);
  });

  describe("Optional enrichment fields have correct types when present", () => {
    it("assistant_text is a string", () => {
      const raw = loadEvent("pretool_bash.json");
      const enriched = enrichEvent({ ...raw }, "PreToolUse");
      if (enriched.assistant_text !== undefined) {
        expect(typeof enriched.assistant_text).toBe("string");
      }
    });

    it("assistant_thinking is a string", () => {
      const raw = loadEvent("pretool_bash.json");
      const enriched = enrichEvent({ ...raw }, "PreToolUse");
      if (enriched.assistant_thinking !== undefined) {
        expect(typeof enriched.assistant_thinking).toBe("string");
      }
    });

    it("token_usage has numeric fields", () => {
      const raw = loadEvent("stop_first.json");
      const enriched = enrichEvent({ ...raw }, "Stop");
      if (enriched.token_usage) {
        expect(typeof enriched.token_usage.input_tokens).toBe("number");
        expect(typeof enriched.token_usage.output_tokens).toBe("number");
      }
    });

    it("parent_agent_id is a string when present", () => {
      const raw = loadEvent("pretool_bash.json");
      const enriched = enrichEvent({ ...raw }, "PreToolUse", {
        agentState: { "test-session-001": ["agent_a"] },
      });
      expect(typeof enriched.parent_agent_id).toBe("string");
    });
  });
});

// ─── Part 3: OpenCode bridge event payloads ─────────────────────────────────

describe("OpenCode bridge event payloads", () => {
  it("session.created → SessionStart with required fields", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "session.created", properties: { info: { id: "oc-1", directory: "/proj", slug: "s", title: "T" } } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.event).toBe("SessionStart");
    expect(mapped!.sessionId).toBe("oc-1");
    const result = validateEventData("SessionStart", mapped!.data);
    expect(result.errors).toEqual([]);
    expect(mapped!.data.slug).toBe("s");
    expect(mapped!.data.title).toBe("T");
  });

  it("session.deleted → SessionEnd", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "session.deleted", properties: { sessionID: "oc-1" } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.event).toBe("SessionEnd");
    expect(mapped!.sessionId).toBe("oc-1");
  });

  it("session.status → SessionStatus with required status", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "session.status", properties: { sessionID: "oc-1", status: { type: "busy", attempt: 2, message: "thinking" } } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.event).toBe("SessionStatus");
    const result = validateEventData("SessionStatus", mapped!.data);
    expect(result.errors).toEqual([]);
    expect(mapped!.data.status).toBe("busy");
    expect(mapped!.data.attempt).toBe(2);
  });

  it("session.idle → SessionIdle", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "session.idle", properties: { sessionID: "oc-1" } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.event).toBe("SessionIdle");
    const result = validateEventData("SessionIdle", mapped!.data);
    expect(result.errors).toEqual([]);
  });

  it("message.updated (user) → UserPromptSubmit with message_id", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "message.updated", properties: { info: { id: "m-1", sessionID: "oc-1", role: "user", agent: "default", model: "gpt-4" } } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.event).toBe("UserPromptSubmit");
    expect(mapped!.data.message_id).toBe("m-1");
    expect(mapped!.data.agent).toBe("default");
  });

  it("message.updated (assistant) → AssistantMessage", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "message.updated", properties: { info: { id: "m-2", sessionID: "oc-1", role: "assistant", parentID: "m-1", modelID: "gpt-4", cost: 0.01, tokens: 100, finish: "stop" } } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.event).toBe("AssistantMessage");
    const result = validateEventData("AssistantMessage", mapped!.data);
    expect(result.errors).toEqual([]);
    expect(mapped!.data.message_id).toBe("m-2");
    expect(mapped!.data.model_id).toBe("gpt-4");
    expect(mapped!.data.cost).toBe(0.01);
  });

  it("message.part.updated → MessagePart with required fields", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "message.part.updated", properties: { part: { id: "p-1", sessionID: "oc-1", messageID: "m-2", type: "text", text: "hello" }, delta: true } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.event).toBe("MessagePart");
    const result = validateEventData("MessagePart", mapped!.data);
    expect(result.errors).toEqual([]);
    expect(mapped!.data.message_id).toBe("m-2");
    expect(mapped!.data.part_id).toBe("p-1");
    expect(mapped!.data.part_type).toBe("text");
  });

  it("message.part.updated (tool) → MessagePart with tool info", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "message.part.updated", properties: { part: { id: "p-2", sessionID: "oc-1", messageID: "m-2", type: "tool", callID: "call-1", tool: "Edit", state: "running" } } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.data.tool).toBeDefined();
    expect(mapped!.data.tool.call_id).toBe("call-1");
    expect(mapped!.data.tool.tool_name).toBe("Edit");
    expect(mapped!.data.tool.state).toBe("running");
  });

  it("permission.asked → PermissionRequest with permission_id", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "permission.asked", properties: { id: "perm-1", sessionID: "oc-1", permission: "write", tool: "Edit" } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.event).toBe("PermissionRequest");
    const result = validateEventData("PermissionRequest", mapped!.data);
    expect(result.errors).toEqual([]);
    expect(mapped!.data.permission_id).toBe("perm-1");
  });

  it("question.asked → AskUserQuestion with required fields", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "question.asked", properties: { id: "q-1", sessionID: "oc-1", questions: [{ text: "which?" }], tool: "AUQ" } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.event).toBe("AskUserQuestion");
    const result = validateEventData("AskUserQuestion", mapped!.data);
    expect(result.errors).toEqual([]);
    expect(mapped!.data.question_id).toBe("q-1");
    expect(mapped!.data.questions).toEqual([{ text: "which?" }]);
  });

  it("vcs.branch.updated → VcsBranchUpdate with branch", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "vcs.branch.updated", properties: { sessionID: "oc-1", branch: "feature/x" } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.event).toBe("VcsBranchUpdate");
    const result = validateEventData("VcsBranchUpdate", mapped!.data);
    expect(result.errors).toEqual([]);
    expect(mapped!.data.branch).toBe("feature/x");
  });

  it("session.updated → SessionUpdate", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "session.updated", properties: { info: { id: "oc-1", title: "New Title", slug: "new-slug", summary: "sum" } } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.event).toBe("SessionUpdate");
    const result = validateEventData("SessionUpdate", mapped!.data);
    expect(result.errors).toEqual([]);
    expect(mapped!.data.title).toBe("New Title");
  });
});

// ─── Part 4: Cross-bridge consistency for shared events ─────────────────────

describe("Cross-bridge consistency", () => {
  for (const eventName of SHARED_EVENTS) {
    it(`${eventName} has a schema defined`, () => {
      expect(EVENT_DATA_SCHEMAS[eventName]).toBeDefined();
    });
  }

  it("SessionStart: both bridges produce valid envelopes with same event name", () => {
    // Claude Code
    const ccData = enrichEvent({ session_id: "cc-1", cwd: "/proj" }, "SessionStart");
    const ccEnv = buildClaudeCodeEnvelope("SessionStart", "cc-1", "/proj", null, ccData);
    expect(validateEnvelope(ccEnv).valid).toBe(true);

    // OpenCode
    const ocMapped = mapOpenCodeEventToGravity(
      { type: "session.created", properties: { info: { id: "oc-1", directory: "/proj", slug: "s", title: "T" } } },
      3000, "/proj",
    );
    const ocEnv = buildOpenCodeEnvelope(ocMapped!.event, ocMapped!.sessionId, ocMapped!.cwd, ocMapped!.data, 3000, "/proj");
    expect(validateEnvelope(ocEnv).valid).toBe(true);

    // Same event name
    expect(ccEnv.event).toBe(ocEnv.event);
    expect(ccEnv.event).toBe("SessionStart");
  });

  it("SessionEnd: both bridges use the same event name", () => {
    const ccEnv = buildClaudeCodeEnvelope("SessionEnd", "cc-1", "/proj", null, {});
    const ocMapped = mapOpenCodeEventToGravity(
      { type: "session.deleted", properties: { sessionID: "oc-1" } },
      3000, "/proj",
    );
    const ocEnv = buildOpenCodeEnvelope(ocMapped!.event, ocMapped!.sessionId, ocMapped!.cwd, ocMapped!.data, 3000, "/proj");

    expect(ccEnv.event).toBe(ocEnv.event);
    expect(ccEnv.event).toBe("SessionEnd");
  });

  it("UserPromptSubmit: both bridges use the same event name", () => {
    const ccEnv = buildClaudeCodeEnvelope("UserPromptSubmit", "cc-1", "/proj", null, { prompt: "hello" });
    const ocMapped = mapOpenCodeEventToGravity(
      { type: "message.updated", properties: { info: { id: "m-1", sessionID: "oc-1", role: "user" } } },
      3000, "/proj",
    );
    const ocEnv = buildOpenCodeEnvelope(ocMapped!.event, ocMapped!.sessionId, ocMapped!.cwd, ocMapped!.data, 3000, "/proj");

    expect(ccEnv.event).toBe(ocEnv.event);
    expect(ccEnv.event).toBe("UserPromptSubmit");
  });

  it("Both bridges include cwd and session_id in envelope", () => {
    const ccEnv = buildClaudeCodeEnvelope("SessionStart", "cc-1", "/proj", null, {});
    const ocMapped = mapOpenCodeEventToGravity(
      { type: "session.created", properties: { info: { id: "oc-1", directory: "/proj", slug: "s", title: "T" } } },
      3000, "/proj",
    );
    const ocEnv = buildOpenCodeEnvelope(ocMapped!.event, ocMapped!.sessionId, ocMapped!.cwd, ocMapped!.data, 3000, "/proj");

    expect(typeof ccEnv.session_id).toBe("string");
    expect(typeof ocEnv.session_id).toBe("string");
    expect(typeof ccEnv.cwd).toBe("string");
    expect(typeof ocEnv.cwd).toBe("string");
  });
});

// ─── Part 5: Null and edge cases ────────────────────────────────────────────

describe("Null and edge cases", () => {
  it("Unknown event type returns null", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "unknown.event.type", properties: {} },
      3000, "/proj",
    );
    expect(mapped).toBeNull();
  });

  it("message.updated with missing info returns null", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "message.updated", properties: {} },
      3000, "/proj",
    );
    expect(mapped).toBeNull();
  });

  it("message.part.updated with missing part returns null", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "message.part.updated", properties: {} },
      3000, "/proj",
    );
    expect(mapped).toBeNull();
  });

  it("session.updated with missing info returns null", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "session.updated", properties: {} },
      3000, "/proj",
    );
    expect(mapped).toBeNull();
  });

  it("message.updated with unknown role returns null", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "message.updated", properties: { info: { id: "m-1", sessionID: "oc-1", role: "system" } } },
      3000, "/proj",
    );
    expect(mapped).toBeNull();
  });

  it("vcs.branch.updated with no sessionID uses 'unknown' fallback", () => {
    const mapped = mapOpenCodeEventToGravity(
      { type: "vcs.branch.updated", properties: { branch: "main" } },
      3000, "/proj",
    );
    expect(mapped).not.toBeNull();
    expect(mapped!.sessionId).toBe("unknown");
    expect(mapped!.data.branch).toBe("main");
  });

  it("Missing properties object defaults to empty", () => {
    // mapOpenCodeEventToGravity accesses event.properties || {}
    const mapped = mapOpenCodeEventToGravity(
      { type: "session.idle" },
      3000, "/proj",
    );
    // session.idle reads props.sessionID which will be undefined
    expect(mapped).not.toBeNull();
    expect(mapped!.event).toBe("SessionIdle");
  });

  it("validateEventData passes for unknown event type", () => {
    const result = validateEventData("SomeNewEvent", { anything: true });
    expect(result.valid).toBe(true);
    expect(result.errors).toEqual([]);
  });

  it("validateEventData fails when required field is missing", () => {
    const result = validateEventData("PreToolUse", { tool_name: "Bash" });
    expect(result.valid).toBe(false);
    expect(result.errors).toContain("Missing required data field: tool_use_id");
    expect(result.errors).toContain("Missing required data field: tool_input");
  });
});
