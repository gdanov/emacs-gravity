import { describe, it, expect } from "vitest";
import { join } from "path";
import { writeFileSync, mkdtempSync } from "fs";
import { tmpdir } from "os";
import { extractTrailingText, extractPrecedingContent, extractFollowingContent, readTail, readHead } from "../src/index";

const FIXTURES = join(__dirname, "fixtures");

describe("readTail", () => {
  it("reads entire small file when under maxBytes", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "small.txt");
    writeFileSync(f, "line1\nline2\nline3\n");
    const result = readTail(f, 1024);
    expect(result).toBe("line1\nline2\nline3\n");
  });

  it("reads only tail when file exceeds maxBytes", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "large.txt");
    // Write 100 lines, each ~20 chars
    const lines = Array.from({ length: 100 }, (_, i) => `line-${String(i).padStart(3, "0")}-padding`);
    writeFileSync(f, lines.join("\n") + "\n");
    // Read only last 200 bytes — should get tail lines, skip partial first line
    const result = readTail(f, 200);
    expect(result).not.toContain("line-000");
    // Should start at a complete line boundary
    expect(result).toMatch(/^line-\d{3}/);
    expect(result).toContain("line-099");
  });
});

describe("extractTrailingText", () => {
  it("extracts trailing text from first stop (after Task agent)", () => {
    const result = extractTrailingText(join(FIXTURES, "at_first_stop.jsonl"));
    expect(result.text).toContain("Based on the research");
    expect(result.text).toContain("AskUserQuestion");
    expect(result.text.length).toBeGreaterThan(100);
  });

  it("extracts trailing text from second stop (after Bash tool)", () => {
    const result = extractTrailingText(join(FIXTURES, "at_second_stop.jsonl"));
    expect(result.text).toContain("You're right, my initial analysis was wrong");
    expect(result.text).toContain("PreToolUse");
    expect(result.text.length).toBeGreaterThan(100);
  });

  it("extracts trailing text from minimal fixture", () => {
    const result = extractTrailingText(join(FIXTURES, "minimal_trailing_text.jsonl"));
    expect(result.text).toContain("Based on the research");
  });

  it("extracts trailing text from second cycle minimal fixture", () => {
    const result = extractTrailingText(join(FIXTURES, "minimal_second_cycle.jsonl"));
    expect(result.text).toContain("You're right, my initial analysis was wrong");
  });

  it("returns empty for empty file", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "empty.jsonl");
    writeFileSync(f, "");
    const result = extractTrailingText(f);
    expect(result).toEqual({ text: "", thinking: "" });
  });

  it("returns empty when transcript ends with tool_use", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "tool_use_end.jsonl");
    const lines = [
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "tool_use", id: "tool_123", name: "Bash", input: {} }] },
      }),
      JSON.stringify({
        type: "progress",
      }),
    ];
    writeFileSync(f, lines.join("\n") + "\n");
    const result = extractTrailingText(f);
    expect(result).toEqual({ text: "", thinking: "" });
  });

  it("collects multiple trailing text blocks", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "multi_text.jsonl");
    const lines = [
      JSON.stringify({
        type: "user",
        message: { content: [{ type: "tool_result", tool_use_id: "t1", content: "ok" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "text", text: "First paragraph" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "text", text: "Second paragraph" }] },
      }),
    ];
    writeFileSync(f, lines.join("\n") + "\n");
    const result = extractTrailingText(f);
    expect(result.text).toBe("First paragraph\n\nSecond paragraph");
  });

  it("extracts thinking from trailing content", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "with_thinking.jsonl");
    const lines = [
      JSON.stringify({
        type: "user",
        message: { content: [{ type: "tool_result", tool_use_id: "t1", content: "ok" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "thinking", thinking: "Let me think about this" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "text", text: "Here is my answer" }] },
      }),
    ];
    writeFileSync(f, lines.join("\n") + "\n");
    const result = extractTrailingText(f);
    expect(result.text).toBe("Here is my answer");
    expect(result.thinking).toBe("Let me think about this");
  });

  it("extracts trailing text with maxBytes snapshot (next turn contamination)", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "snapshot.jsonl");
    // Simulate: completed turn has trailing text, then next turn starts with tool_use
    const completedTurn = [
      JSON.stringify({ type: "user", message: { content: [{ type: "tool_result", tool_use_id: "t1", content: "ok" }] } }),
      JSON.stringify({ type: "assistant", message: { content: [{ type: "tool_use", id: "t2", name: "Read", input: {} }] } }),
      JSON.stringify({ type: "user", message: { content: [{ type: "tool_result", tool_use_id: "t2", content: "file contents" }] } }),
      JSON.stringify({ type: "assistant", message: { content: [{ type: "text", text: "All done! The changes are complete." }] } }),
    ];
    const nextTurn = [
      JSON.stringify({ type: "progress" }),
      JSON.stringify({ type: "system" }),
      JSON.stringify({ type: "assistant", message: { content: [{ type: "tool_use", id: "t3", name: "Bash", input: {} }] } }),
      JSON.stringify({ type: "user", message: { content: [{ type: "tool_result", tool_use_id: "t3", content: "output" }] } }),
    ];
    // Write full transcript (completed turn + next turn)
    const allLines = [...completedTurn, ...nextTurn].join("\n") + "\n";
    writeFileSync(f, allLines);

    // Without maxBytes: finds next turn's tool_use first, returns empty
    const resultNoSnapshot = extractTrailingText(f);
    expect(resultNoSnapshot.text).toBe("");

    // With maxBytes snapshot: only reads up to completed turn, finds trailing text
    const snapshotSize = completedTurn.join("\n").length + 1; // +1 for trailing newline
    const resultWithSnapshot = extractTrailingText(f, snapshotSize);
    expect(resultWithSnapshot.text).toBe("All done! The changes are complete.");
  });

  it("extracts trailing text with maxBytes when user messages are only tool_result", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "continued_session.jsonl");
    // Simulate continued session: no user text messages, all tool_result
    const turn = [
      JSON.stringify({ type: "assistant", message: { content: [{ type: "tool_use", id: "t1", name: "Glob", input: {} }] } }),
      JSON.stringify({ type: "user", message: { content: [{ type: "tool_result", tool_use_id: "t1", content: "files" }] } }),
      JSON.stringify({ type: "assistant", message: { content: [{ type: "text", text: "Here are the results." }] } }),
      JSON.stringify({ type: "assistant", message: { content: [{ type: "text", text: "Everything looks good." }] } }),
    ];
    const content = turn.join("\n") + "\n";
    writeFileSync(f, content);
    const result = extractTrailingText(f, Buffer.byteLength(content));
    expect(result.text).toBe("Here are the results.\n\nEverything looks good.");
  });
});

describe("readHead", () => {
  it("reads entire small file when under maxBytes", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "small.txt");
    writeFileSync(f, "line1\nline2\nline3\n");
    const result = readHead(f, 1024);
    expect(result).toBe("line1\nline2\nline3\n");
  });

  it("reads only head when file exceeds maxBytes", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "large.txt");
    const lines = Array.from({ length: 100 }, (_, i) => `line-${String(i).padStart(3, "0")}-padding`);
    writeFileSync(f, lines.join("\n") + "\n");
    // Read only first 200 bytes
    const result = readHead(f, 200);
    expect(result).toContain("line-000");
    expect(result).not.toContain("line-099");
    // Should end at a complete line boundary (last line before the cut)
    expect(result).toMatch(/line-\d{3}-padding$/);
  });
});

describe("extractPrecedingContent", () => {
  it("extracts thinking before Bash tool call", () => {
    const result = extractPrecedingContent(
      join(FIXTURES, "preceding_content_bash.jsonl"),
      "toolu_0117AHsMTSWiUkLVBhQ43wzv"
    );
    expect(result.thinking).toContain("pointing me to a specific GitHub issue");
  });

  it("extracts thinking before Task tool call in full transcript", () => {
    const result = extractPrecedingContent(
      join(FIXTURES, "at_first_stop.jsonl"),
      "toolu_01DpAA4KkHMsWYXGnPuSn9fa"
    );
    expect(result.thinking).toContain("hook");
  });

  it("falls back to end when tool_use_id not found", () => {
    const result = extractPrecedingContent(
      join(FIXTURES, "at_first_stop.jsonl"),
      "nonexistent_id"
    );
    // Should find trailing text from end (the reply text)
    expect(result.text.length).toBeGreaterThan(0);
  });

  it("returns empty for empty file", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "empty.jsonl");
    writeFileSync(f, "");
    const result = extractPrecedingContent(f, "any_id");
    expect(result).toEqual({ text: "", thinking: "" });
  });

  it("skips (no content) placeholder text", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "no_content.jsonl");
    const lines = [
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "text", text: "(no content)" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "tool_use", id: "tool_x", name: "Read", input: {} }] },
      }),
    ];
    writeFileSync(f, lines.join("\n") + "\n");
    const result = extractPrecedingContent(f, "tool_x");
    expect(result.text).toBe("");
  });

  it("collects multiple text blocks before tool_use", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "multi_preceding.jsonl");
    const lines = [
      JSON.stringify({
        type: "user",
        message: { content: [{ type: "tool_result", tool_use_id: "prev", content: "ok" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "text", text: "First paragraph" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "text", text: "Second paragraph" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "tool_use", id: "tool_y", name: "Read", input: {} }] },
      }),
    ];
    writeFileSync(f, lines.join("\n") + "\n");
    const result = extractPrecedingContent(f, "tool_y");
    expect(result.text).toBe("First paragraph\n\nSecond paragraph");
  });
});

describe("extractFollowingContent", () => {
  it("extracts text following a tool_result", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "following.jsonl");
    const lines = [
      JSON.stringify({
        type: "user",
        message: { content: [{ type: "tool_result", tool_use_id: "tool_123", content: "ok" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "text", text: "Great! Now I understand" }] },
      }),
    ];
    writeFileSync(f, lines.join("\n") + "\n");
    const result = extractFollowingContent(f, "tool_123");
    expect(result.text).toBe("Great! Now I understand");
  });

  it("extracts multiple text blocks following a tool_result", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "following_multi.jsonl");
    const lines = [
      JSON.stringify({
        type: "user",
        message: { content: [{ type: "tool_result", tool_use_id: "tool_456", content: "result" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "text", text: "First response" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "text", text: "Second response" }] },
      }),
    ];
    writeFileSync(f, lines.join("\n") + "\n");
    const result = extractFollowingContent(f, "tool_456");
    expect(result.text).toBe("First response\n\nSecond response");
  });

  it("stops at next tool_use", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "following_stops.jsonl");
    const lines = [
      JSON.stringify({
        type: "user",
        message: { content: [{ type: "tool_result", tool_use_id: "tool_a", content: "ok" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "text", text: "After tool_a" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "tool_use", id: "tool_b", name: "Read", input: {} }] },
      }),
    ];
    writeFileSync(f, lines.join("\n") + "\n");
    const result = extractFollowingContent(f, "tool_a");
    expect(result.text).toBe("After tool_a");
  });

  it("extracts thinking following tool_result", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "following_thinking.jsonl");
    const lines = [
      JSON.stringify({
        type: "user",
        message: { content: [{ type: "tool_result", tool_use_id: "t1", content: "done" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "thinking", thinking: "Let me process this" }] },
      }),
      JSON.stringify({
        type: "assistant",
        message: { content: [{ type: "text", text: "Here's my response" }] },
      }),
    ];
    writeFileSync(f, lines.join("\n") + "\n");
    const result = extractFollowingContent(f, "t1");
    expect(result.thinking).toBe("Let me process this");
    expect(result.text).toBe("Here's my response");
  });

  it("returns empty when tool_result not found", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "not_found.jsonl");
    const lines = [
      JSON.stringify({
        type: "user",
        message: { content: [{ type: "tool_result", tool_use_id: "tool_x", content: "ok" }] },
      }),
    ];
    writeFileSync(f, lines.join("\n") + "\n");
    const result = extractFollowingContent(f, "nonexistent");
    expect(result).toEqual({ text: "", thinking: "" });
  });

  it("returns empty for empty file", () => {
    const dir = mkdtempSync(join(tmpdir(), "bridge-test-"));
    const f = join(dir, "empty.jsonl");
    writeFileSync(f, "");
    const result = extractFollowingContent(f, "any");
    expect(result).toEqual({ text: "", thinking: "" });
  });
});
