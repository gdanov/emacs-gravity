/**
 * End-of-turn replay tests.
 *
 * Verifies end-of-turn message extraction and enrichment for every known
 * scenario: simple turns, agent turns, race conditions, dedup data, and
 * inter-tool text.
 *
 * Two layers:
 *   1. Raw extraction functions (extractTrailingText, extractFollowingContent)
 *   2. enrichEvent() from enrichment.ts
 */

import { describe, it, expect } from "vitest";
import { join, dirname, basename } from "path";
import { writeFileSync, mkdtempSync, mkdirSync } from "fs";
import { tmpdir } from "os";
import {
  extractTrailingText,
  extractFollowingContent,
} from "../src/index";
import { enrichEvent } from "../src/enrichment";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Create a temp JSONL file from an array of objects. Returns the file path. */
function makeTranscript(lines: any[]): string {
  const dir = mkdtempSync(join(tmpdir(), "eot-replay-"));
  const f = join(dir, "transcript.jsonl");
  writeFileSync(f, lines.map((l) => JSON.stringify(l)).join("\n") + "\n");
  return f;
}

/** Create an agent sidechain transcript at the path enrichment.ts constructs. */
function makeAgentTranscript(
  mainTranscriptPath: string,
  agentId: string,
  lines: any[]
): string {
  const transcriptDir = dirname(mainTranscriptPath);
  const sessionBase = basename(mainTranscriptPath, ".jsonl");
  const dir = join(transcriptDir, sessionBase, "subagents");
  mkdirSync(dir, { recursive: true });
  const f = join(dir, `agent-${agentId}.jsonl`);
  writeFileSync(f, lines.map((l) => JSON.stringify(l)).join("\n") + "\n");
  return f;
}

/** A progress entry with a slug (needed for enrichEvent to extract slug). */
const slugEntry = (slug = "test-slug") => ({
  type: "progress",
  slug,
  sessionId: "test-session",
});

/** User text prompt (turn boundary). */
const userText = (text: string) => ({
  type: "user",
  message: { role: "user", content: text },
});

/** Assistant tool_use block. */
const toolUse = (id: string, name = "Bash") => ({
  type: "assistant",
  message: { content: [{ type: "tool_use", id, name, input: {} }] },
});

/** User tool_result (NOT a turn boundary). */
const toolResult = (id: string, content = "ok") => ({
  type: "user",
  message: { content: [{ type: "tool_result", tool_use_id: id, content }] },
});

/** Assistant text block. */
const assistantText = (text: string) => ({
  type: "assistant",
  message: { content: [{ type: "text", text }] },
});

/** Assistant thinking block. */
const assistantThinking = (thinking: string) => ({
  type: "assistant",
  message: { content: [{ type: "thinking", thinking }] },
});

/** Sidechain marker (first line of agent transcripts). */
const sidechainMarker = () => ({
  type: "progress",
  isSidechain: true,
  slug: "test-slug",
});

/** Make a Stop event payload pointing at a transcript. */
const stopEvent = (transcriptPath: string) => ({
  session_id: "test-session",
  cwd: dirname(transcriptPath),
  transcript_path: transcriptPath,
});

/** Make a SubagentStop event payload. */
const subagentStopEvent = (transcriptPath: string, agentId: string) => ({
  session_id: "test-session",
  cwd: dirname(transcriptPath),
  transcript_path: transcriptPath,
  agent_id: agentId,
});

/** Make a PostToolUse event payload. */
const postToolEvent = (transcriptPath: string, toolUseId: string) => ({
  session_id: "test-session",
  cwd: dirname(transcriptPath),
  transcript_path: transcriptPath,
  tool_use_id: toolUseId,
  tool_name: "Read",
  tool_input: {},
});

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe("Simple turn endings", () => {
  it("extracts trailing text only (no thinking)", () => {
    const f = makeTranscript([
      slugEntry(),
      userText("Fix the bug"),
      toolUse("t1"),
      toolResult("t1"),
      assistantText("All done, the bug is fixed."),
    ]);
    const raw = extractTrailingText(f);
    expect(raw.text).toBe("All done, the bug is fixed.");
    expect(raw.thinking).toBe("");

    // enrichEvent produces the same
    const enriched = enrichEvent(stopEvent(f), "Stop");
    expect(enriched.stop_text).toBe("All done, the bug is fixed.");
    expect(enriched.stop_thinking).toBeUndefined();
  });

  it("extracts both text and thinking", () => {
    const f = makeTranscript([
      slugEntry(),
      userText("Summarize"),
      toolUse("t1"),
      toolResult("t1"),
      assistantThinking("Let me wrap up the findings"),
      assistantText("Here is the summary."),
    ]);
    const raw = extractTrailingText(f);
    expect(raw.text).toBe("Here is the summary.");
    expect(raw.thinking).toBe("Let me wrap up the findings");

    const enriched = enrichEvent(stopEvent(f), "Stop");
    expect(enriched.stop_text).toBe("Here is the summary.");
    expect(enriched.stop_thinking).toBe("Let me wrap up the findings");
  });

  it("joins multiple trailing text blocks with paragraph break", () => {
    const f = makeTranscript([
      slugEntry(),
      toolResult("t1"),
      assistantText("First paragraph."),
      assistantText("Second paragraph."),
    ]);
    const raw = extractTrailingText(f);
    expect(raw.text).toBe("First paragraph.\n\nSecond paragraph.");

    const enriched = enrichEvent(stopEvent(f), "Stop");
    expect(enriched.stop_text).toBe("First paragraph.\n\nSecond paragraph.");
  });

  it("extracts thinking only when no text follows", () => {
    const f = makeTranscript([
      slugEntry(),
      toolResult("t1"),
      assistantThinking("I need to think more about this"),
    ]);
    const raw = extractTrailingText(f);
    expect(raw.text).toBe("");
    expect(raw.thinking).toBe("I need to think more about this");

    // enrichEvent: stop_text won't be set (falsy), stop_thinking will
    const enriched = enrichEvent(stopEvent(f), "Stop");
    expect(enriched.stop_text).toBeUndefined();
    expect(enriched.stop_thinking).toBe("I need to think more about this");
  });

  it("returns empty when no trailing content after tool_result", () => {
    const f = makeTranscript([
      slugEntry(),
      toolUse("t1"),
      toolResult("t1"),
    ]);
    const raw = extractTrailingText(f);
    expect(raw.text).toBe("");
    expect(raw.thinking).toBe("");
  });

  it("skips '(no content)' placeholder text", () => {
    const f = makeTranscript([
      slugEntry(),
      toolResult("t1"),
      assistantText("(no content)"),
    ]);
    const raw = extractTrailingText(f);
    expect(raw.text).toBe("");
  });

  it("preserves whitespace-only text (documents current behavior)", () => {
    const f = makeTranscript([
      slugEntry(),
      toolResult("t1"),
      assistantText("\n\n"),
    ]);
    const raw = extractTrailingText(f);
    // Current behavior: whitespace-only is NOT filtered (only "(no content)" is)
    expect(raw.text).toBe("\n\n");
  });

  it("handles pre-prompt activity (turn 0, no user text message)", () => {
    const f = makeTranscript([
      slugEntry(),
      toolUse("t1", "Glob"),
      toolResult("t1", "src/index.ts\nsrc/app.ts"),
      assistantText("Found 5 files across the project."),
    ]);
    const raw = extractTrailingText(f);
    expect(raw.text).toBe("Found 5 files across the project.");

    const enriched = enrichEvent(stopEvent(f), "Stop");
    expect(enriched.stop_text).toBe("Found 5 files across the project.");
  });
});

describe("Agent turn endings", () => {
  it("extracts from sidechain transcript with isSidechain marker", () => {
    const mainPath = makeTranscript([slugEntry()]);
    // extractTrailingTextFromAgent walks backward and returns on the FIRST
    // assistant message that has text or thinking. So thinking + text must be
    // in the same message (or thinking in a later message) to both be captured.
    const agentLines = [
      sidechainMarker(),
      assistantThinking("Searching the codebase"),
      toolUse("at1", "Grep"),
      toolResult("at1", "found 3 matches"),
      // Final agent message has both thinking and text in content array
      {
        type: "assistant",
        message: {
          content: [
            { type: "thinking", thinking: "Based on my analysis" },
            { type: "text", text: "The codebase has 3 relevant patterns." },
          ],
        },
      },
    ];
    const agentPath = makeAgentTranscript(mainPath, "agent_001", agentLines);

    const raw = extractTrailingText(agentPath);
    expect(raw.text).toBe("The codebase has 3 relevant patterns.");
    expect(raw.thinking).toBe("Based on my analysis");

    // enrichEvent for SubagentStop
    const enriched = enrichEvent(
      subagentStopEvent(mainPath, "agent_001"),
      "SubagentStop"
    );
    expect(enriched.agent_stop_text).toBe("The codebase has 3 relevant patterns.");
    expect(enriched.agent_stop_thinking).toBe("Based on my analysis");
  });

  it("falls back to main transcript when sidechain is empty (raw extraction)", () => {
    // Create main transcript with trailing text
    const mainPath = makeTranscript([
      slugEntry(),
      toolUse("t1", "Task"),
      toolResult("t1", "agent output"),
      assistantText("Agent analysis complete: found 42 files."),
    ]);
    // Create empty sidechain
    makeAgentTranscript(mainPath, "agent_002", []);

    // Sidechain extraction returns empty
    const transcriptDir = dirname(mainPath);
    const sessionBase = basename(mainPath, ".jsonl");
    const agentPath = join(
      transcriptDir,
      sessionBase,
      "subagents",
      "agent-agent_002.jsonl"
    );
    const sidechainResult = extractTrailingText(agentPath);
    expect(sidechainResult.text).toBe("");

    // Main transcript extraction finds the text
    // NOTE: enrichEvent() in enrichment.ts does NOT have the main-transcript
    // fallback. That logic is only in main() in index.ts. This test documents
    // the gap — only the raw extraction function is testable here.
    const mainResult = extractTrailingText(mainPath);
    expect(mainResult.text).toContain("Agent analysis complete");
  });

  it("returns empty when both sidechain and main are empty", () => {
    const mainPath = makeTranscript([slugEntry()]);
    makeAgentTranscript(mainPath, "agent_003", []);

    const enriched = enrichEvent(
      subagentStopEvent(mainPath, "agent_003"),
      "SubagentStop"
    );
    expect(enriched.agent_stop_text).toBeUndefined();
    expect(enriched.agent_stop_thinking).toBeUndefined();
  });

  it("extracts agent tool IDs from sidechain", () => {
    const mainPath = makeTranscript([slugEntry()]);
    const agentLines = [
      sidechainMarker(),
      toolUse("at1", "Glob"),
      toolResult("at1"),
      toolUse("at2", "Read"),
      toolResult("at2"),
      toolUse("at3", "Grep"),
      toolResult("at3"),
      assistantText("Done."),
    ];
    makeAgentTranscript(mainPath, "agent_004", agentLines);

    const enriched = enrichEvent(
      subagentStopEvent(mainPath, "agent_004"),
      "SubagentStop"
    );
    expect(enriched.agent_tool_ids).toEqual(["at1", "at2", "at3"]);
  });

  it("extracts text from mixed tool_use+text in same assistant message", () => {
    // Agent transcripts can have tool_use and text in the same content array
    const mainPath = makeTranscript([slugEntry()]);
    const agentLines = [
      sidechainMarker(),
      {
        type: "assistant",
        message: {
          content: [
            { type: "tool_use", id: "at1", name: "Read", input: {} },
            { type: "text", text: "Mixed content summary" },
          ],
        },
      },
    ];
    makeAgentTranscript(mainPath, "agent_005", agentLines);

    const enriched = enrichEvent(
      subagentStopEvent(mainPath, "agent_005"),
      "SubagentStop"
    );
    // extractTrailingTextFromAgent does NOT stop at tool_use in same message
    expect(enriched.agent_stop_text).toBe("Mixed content summary");
  });
});

describe("Race conditions", () => {
  it("maxBytes prevents next-turn contamination via enrichEvent", () => {
    // Completed turn with trailing text
    const completedTurn = [
      slugEntry(),
      userText("Fix the bug"),
      toolUse("t1"),
      toolResult("t1"),
      assistantText("Bug is fixed, all tests pass."),
    ];
    // Next turn starts with new tool_use
    const nextTurn = [
      { type: "progress" },
      userText("Now add tests"),
      assistantThinking("Let me write tests"),
      toolUse("t2", "Edit"),
      toolResult("t2"),
    ];

    const allContent =
      [...completedTurn, ...nextTurn].map((l) => JSON.stringify(l)).join("\n") + "\n";
    const dir = mkdtempSync(join(tmpdir(), "eot-replay-race-"));
    const f = join(dir, "transcript.jsonl");
    writeFileSync(f, allContent);

    // Without snapshot: walks backward from end, hits t2 tool_use, returns empty
    const noSnapshot = extractTrailingText(f);
    expect(noSnapshot.text).toBe("");

    // With snapshot: only reads up to completed turn boundary
    const snapshotSize =
      completedTurn.map((l) => JSON.stringify(l)).join("\n").length + 1;
    const withSnapshot = extractTrailingText(f, snapshotSize);
    expect(withSnapshot.text).toBe("Bug is fixed, all tests pass.");

    // enrichEvent with stopSnapshotBytes
    const enriched = enrichEvent(
      { session_id: "test-session", cwd: dir, transcript_path: f },
      "Stop",
      { stopSnapshotBytes: snapshotSize }
    );
    expect(enriched.stop_text).toBe("Bug is fixed, all tests pass.");
  });

  it("empty transcript returns empty without crash", () => {
    const f = makeTranscript([]);
    const raw = extractTrailingText(f);
    expect(raw).toEqual({ text: "", thinking: "" });
  });
});

describe("Dedup data (both fields populated for Emacs-side dedup)", () => {
  it("PostToolUse and Stop both extract overlapping text", () => {
    // Transcript where last tool's following text is the same as trailing text
    const f = makeTranscript([
      slugEntry(),
      userText("Do the thing"),
      toolUse("t1"),
      toolResult("t1"),
      assistantText("All done successfully."),
    ]);

    // PostToolUse extracts following text
    const following = extractFollowingContent(f, "t1");
    expect(following.text).toBe("All done successfully.");

    // Stop extracts the same trailing text
    const trailing = extractTrailingText(f);
    expect(trailing.text).toBe("All done successfully.");

    // Both populated identically — Emacs insert-stop-text handles dedup at render time
  });

  it("PostToolUse following content stops at next tool_use", () => {
    const f = makeTranscript([
      slugEntry(),
      toolUse("t1"),
      toolResult("t1"),
      assistantText("After tool 1"),
      toolUse("t2"),
      toolResult("t2"),
      assistantText("After tool 2"),
    ]);

    const following = extractFollowingContent(f, "t1");
    expect(following.text).toBe("After tool 1");
    // Does NOT include "After tool 2"
  });

  it("last PostToolUse text+thinking match Stop text+thinking", () => {
    const f = makeTranscript([
      slugEntry(),
      toolUse("t_last"),
      toolResult("t_last"),
      assistantThinking("Reflecting on the result"),
      assistantText("Final conclusion."),
    ]);

    const following = extractFollowingContent(f, "t_last");
    expect(following.text).toBe("Final conclusion.");
    expect(following.thinking).toBe("Reflecting on the result");

    const trailing = extractTrailingText(f);
    expect(trailing.text).toBe("Final conclusion.");
    expect(trailing.thinking).toBe("Reflecting on the result");

    // Both identical — dedup in Emacs will suppress one
  });
});

describe("Inter-tool text (PostToolUse enrichment)", () => {
  it("extracts text between sequential tools", () => {
    const f = makeTranscript([
      slugEntry(),
      toolUse("t1"),
      toolResult("t1"),
      assistantText("Now I'll read the config."),
      toolUse("t2"),
      toolResult("t2"),
    ]);

    const following = extractFollowingContent(f, "t1");
    expect(following.text).toBe("Now I'll read the config.");

    // Via enrichEvent
    const enriched = enrichEvent(postToolEvent(f, "t1"), "PostToolUse");
    expect(enriched.post_tool_text).toBe("Now I'll read the config.");
  });

  it("extracts thinking + text between tools", () => {
    const f = makeTranscript([
      slugEntry(),
      toolUse("t1"),
      toolResult("t1"),
      assistantThinking("I see an issue here"),
      assistantText("Let me fix the config."),
      toolUse("t2"),
    ]);

    const following = extractFollowingContent(f, "t1");
    expect(following.thinking).toBe("I see an issue here");
    expect(following.text).toBe("Let me fix the config.");
  });

  it("returns empty when tool_result not found", () => {
    const f = makeTranscript([
      slugEntry(),
      toolUse("t1"),
      toolResult("t1"),
    ]);

    const following = extractFollowingContent(f, "nonexistent_id");
    expect(following).toEqual({ text: "", thinking: "" });
  });
});
