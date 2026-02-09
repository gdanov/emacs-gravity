import { describe, it, expect, beforeAll } from "vitest";
import { join } from "path";
import { readFileSync, mkdirSync, copyFileSync, existsSync } from "fs";
import { enrichEvent } from "../src/enrichment";

const FIXTURES = join(__dirname, "fixtures");
const EVENTS = join(FIXTURES, "events");

/** Load an event fixture and resolve __FIXTURE__ placeholders to actual fixture dir. */
function loadEvent(filename: string): any {
  const raw = JSON.parse(readFileSync(join(EVENTS, filename), "utf-8"));
  if (raw.transcript_path && raw.transcript_path.startsWith("__FIXTURE__/")) {
    raw.transcript_path = join(FIXTURES, raw.transcript_path.replace("__FIXTURE__/", ""));
  }
  return raw;
}

// Set up agent transcript at the path enrichment.ts will construct:
// {transcriptDir}/{sessionBase}/subagents/agent-{agentId}.jsonl
beforeAll(() => {
  // For subagent_stop.json: transcript_path = .../at_first_stop.jsonl, agent_id = agent_abc123
  // Agent transcript path = .../at_first_stop/subagents/agent-agent_abc123.jsonl
  const subagentDir = join(FIXTURES, "at_first_stop", "subagents");
  if (!existsSync(subagentDir)) {
    mkdirSync(subagentDir, { recursive: true });
  }
  const dest = join(subagentDir, "agent-agent_abc123.jsonl");
  if (!existsSync(dest)) {
    copyFileSync(join(FIXTURES, "agent_transcript.jsonl"), dest);
  }
});

describe("Event enrichment snapshots", () => {
  // --- PreToolUse ---

  it("PreToolUse: Bash with thinking", () => {
    const raw = loadEvent("pretool_bash.json");
    const enriched = enrichEvent(raw, "PreToolUse");

    expect(enriched.assistant_thinking).toBeTruthy();
    expect(enriched.assistant_thinking).toContain("pointing me to a specific GitHub issue");
    expect(enriched.tool_use_id).toBe("toolu_0117AHsMTSWiUkLVBhQ43wzv");
    expect(enriched.slug).toBe("dazzling-brewing-otter");
    expect(enriched).toMatchSnapshot();
  });

  it("PreToolUse: Task agent start with thinking", () => {
    const raw = loadEvent("pretool_task.json");
    const enriched = enrichEvent(raw, "PreToolUse");

    expect(enriched.assistant_thinking).toBeTruthy();
    expect(enriched.assistant_thinking).toContain("hook");
    expect(enriched.slug).toBe("dazzling-brewing-otter");
    expect(enriched).toMatchSnapshot();
  });

  // --- PostToolUse ---

  it("PostToolUse: Read with following text", () => {
    const raw = loadEvent("posttool_read.json");
    const enriched = enrichEvent(raw, "PostToolUse");

    expect(enriched.post_tool_text).toBeTruthy();
    expect(enriched.post_tool_text).toContain("hello");
    expect(enriched.post_tool_thinking).toBeTruthy();
    expect(enriched.slug).toBe("test-slug-posttool");
    expect(enriched).toMatchSnapshot();
  });

  // --- Stop ---

  it("Stop: first stop with trailing text and token usage", () => {
    const raw = loadEvent("stop_first.json");
    const enriched = enrichEvent(raw, "Stop");

    expect(enriched.stop_text).toBeTruthy();
    expect(enriched.stop_text).toContain("Based on the research");
    expect(enriched.token_usage).toBeDefined();
    // Token usage fields exist (may be 0 if transcript has no top-level usage entries)
    expect(enriched.token_usage).toHaveProperty("input_tokens");
    expect(enriched.token_usage).toHaveProperty("output_tokens");
    expect(enriched.token_usage).toHaveProperty("cache_read_input_tokens");
    expect(enriched.token_usage).toHaveProperty("cache_creation_input_tokens");
    expect(enriched.slug).toBe("dazzling-brewing-otter");
    expect(enriched).toMatchSnapshot();
  });

  it("Stop: second stop with different trailing text", () => {
    const raw = loadEvent("stop_second.json");
    const enriched = enrichEvent(raw, "Stop");

    expect(enriched.stop_text).toBeTruthy();
    expect(enriched.stop_text).toContain("You're right, my initial analysis was wrong");
    expect(enriched.token_usage).toBeDefined();
    expect(enriched.slug).toBe("dazzling-brewing-otter");
    expect(enriched).toMatchSnapshot();
  });

  // --- SubagentStart ---

  it("SubagentStart: injects agent transcript path", () => {
    const raw = loadEvent("subagent_start.json");
    const enriched = enrichEvent(raw, "SubagentStart");

    expect(enriched.agent_transcript_path).toBeTruthy();
    expect(enriched.agent_transcript_path).toContain("agent-agent_abc123.jsonl");
    expect(enriched.agent_transcript_path).toContain("subagents");
    expect(enriched.slug).toBe("dazzling-brewing-otter");
    // Snapshot will contain absolute paths, so check structure without snapshot
  });

  // --- SubagentStop ---

  it("SubagentStop: extracts agent tool IDs and trailing text", () => {
    const raw = loadEvent("subagent_stop.json");
    const enriched = enrichEvent(raw, "SubagentStop");

    expect(enriched.agent_tool_ids).toBeTruthy();
    expect(enriched.agent_tool_ids).toEqual(["toolu_agent_001", "toolu_agent_002"]);
    expect(enriched.agent_transcript_path).toContain("agent-agent_abc123.jsonl");
    expect(enriched.agent_stop_text).toBeTruthy();
    expect(enriched.agent_stop_text).toContain("hook system does fire PreToolUse");
    expect(enriched.agent_stop_thinking).toBeTruthy();
    expect(enriched.agent_stop_thinking).toContain("Based on my research");
    expect(enriched.slug).toBe("dazzling-brewing-otter");
  });

  // --- Agent attribution ---

  it("PreToolUse: attributes tool to single active agent", () => {
    const raw = loadEvent("pretool_bash.json");
    const enriched = enrichEvent(raw, "PreToolUse", {
      agentState: { "test-session-001": ["agent_xyz"] },
    });

    expect(enriched.parent_agent_id).toBe("agent_xyz");
  });

  it("PreToolUse: marks ambiguous with multiple agents", () => {
    const raw = loadEvent("pretool_bash.json");
    const enriched = enrichEvent(raw, "PreToolUse", {
      agentState: { "test-session-001": ["agent_a", "agent_b"] },
    });

    // Tool ID won't be found in any agent transcript (they don't exist),
    // so it should be marked ambiguous
    expect(enriched.parent_agent_id).toBe("ambiguous");
    expect(enriched.candidate_agent_ids).toEqual(["agent_a", "agent_b"]);
  });

  // --- Passthrough events ---

  it("SessionStart: passes through without enrichment", () => {
    const raw = {
      session_id: "test-session-001",
      cwd: "/test/project",
    };
    const enriched = enrichEvent(raw, "SessionStart");

    expect(enriched.session_id).toBe("test-session-001");
    // No enrichment fields added
    expect(enriched.stop_text).toBeUndefined();
    expect(enriched.assistant_text).toBeUndefined();
    expect(enriched.agent_tool_ids).toBeUndefined();
  });

  it("Notification: passes through without enrichment", () => {
    const raw = {
      session_id: "test-session-001",
      cwd: "/test/project",
      title: "Task complete",
      message: "All tests passed",
    };
    const enriched = enrichEvent(raw, "Notification");

    expect(enriched.title).toBe("Task complete");
    expect(enriched.message).toBe("All tests passed");
  });
});
