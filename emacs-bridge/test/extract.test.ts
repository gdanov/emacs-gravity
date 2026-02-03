import { describe, it, expect } from "vitest";
import { join } from "path";
import { writeFileSync, mkdtempSync } from "fs";
import { tmpdir } from "os";
import { extractTrailingText, extractPrecedingContent, readTail } from "../src/index";

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
});
