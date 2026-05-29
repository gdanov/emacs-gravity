// turn-accumulator-normalize.test.ts — Tests for Edit tool input/result normalization
//
// Tests that pi's Edit tool format (oldText/newText/diff) is normalized
// to Claude Code format (old_string/new_string/structuredPatch) so the
// UI rendering code works identically for both.

import { describe, it, expect, beforeEach } from "vitest";
import {
  createAccState,
  accToolStart,
  accToolEnd,
  type AccState,
} from "../src/pi-driver/turn-accumulator.js";

function makeState(): AccState {
  return createAccState("sess-1", "/tmp", "medium");
}

describe("Edit tool normalization", () => {
  describe("input field mapping (accToolStart)", () => {
    it("transforms pi's oldText/newText to old_string/new_string in toolInput", () => {
      const state = makeState();
      const toolInput = {
        path: "/foo/bar.txt",
        oldText: "hello world",
        newText: "hello universe",
      };
      accToolStart(state, "tc-1", "Edit", toolInput);
      expect(state.currentToolInput).toEqual({
        file_path: "/foo/bar.txt",
        old_string: "hello world",
        new_string: "hello universe",
      });
    });

    it("passes through already-normalized input unchanged", () => {
      const state = makeState();
      const toolInput = {
        file_path: "/foo/bar.txt",
        old_string: "hello",
        new_string: "world",
      };
      accToolStart(state, "tc-1", "Edit", toolInput);
      expect(state.currentToolInput).toEqual(toolInput);
    });

    it("preserves other input fields", () => {
      const state = makeState();
      const toolInput = {
        path: "/a.txt",
        oldText: "old",
        newText: "new",
        extra_field: "preserved",
      };
      accToolStart(state, "tc-1", "Edit", toolInput);
      expect(state.currentToolInput).toEqual({
        file_path: "/a.txt",
        old_string: "old",
        new_string: "new",
        extra_field: "preserved",
      });
    });

    it("handles non-Edit tools unchanged", () => {
      const state = makeState();
      const toolInput = { command: "ls -la", other: true };
      accToolStart(state, "tc-1", "Bash", toolInput);
      expect(state.currentToolInput).toEqual(toolInput);
    });
  });

  describe("result field mapping (accToolEnd)", () => {
    it("transforms pi's diff to structuredPatch", () => {
      const state = makeState();
      const toolInput = { file_path: "/a.txt", old_string: "a", new_string: "b" };
      accToolStart(state, "tc-1", "Edit", toolInput);
      const piResult = {
        diff: `@@ -1,1 +1,1 @@
-old
+new`,
        firstChangedLine: 1,
      };
      const results = accToolEnd(state, "tc-1", "Edit", piResult);
      expect(results.length).toBe(2); // PreToolUse + PostToolUse
      const postTool = results[1];
      expect(postTool.hookEvent).toBe("PostToolUse");
      const hookData = postTool.hookData;
      const r = hookData.tool_response as Record<string, unknown>;
      expect(r).toHaveProperty("structuredPatch");
      expect(Array.isArray(r.structuredPatch)).toBe(true);
      expect(r.structuredPatch.length).toBeGreaterThan(0);
      expect(r.structuredPatch[0]).toHaveProperty("oldStart");
      expect(r.structuredPatch[0]).toHaveProperty("lines");
    });

    it("preserves oldString/newString from pi result", () => {
      const state = makeState();
      const toolInput = { file_path: "/a.txt" };
      accToolStart(state, "tc-1", "Edit", toolInput);
      const piResult = {
        diff: "@@ -1 +1 @@\n-a\n+b",
        oldString: "original",
        newString: "modified",
      };
      const results = accToolEnd(state, "tc-1", "Edit", piResult);
      const r = (results[1].hookData as { tool_response: Record<string, unknown> }).tool_response;
      expect(r.oldString).toBe("original");
      expect(r.newString).toBe("modified");
    });

    it("passes through already-normalized structuredPatch", () => {
      const state = makeState();
      accToolStart(state, "tc-1", "Edit", { file_path: "/a.txt" });
      const ccResult = {
        structuredPatch: [{ oldStart: 1, oldLines: 1, newStart: 1, newLines: 1, lines: ["+x"] }],
        oldString: "a",
        newString: "b",
      };
      const results = accToolEnd(state, "tc-1", "Edit", ccResult);
      const r = (results[1].hookData as { tool_response: Record<string, unknown> }).tool_response;
      expect(r.structuredPatch).toEqual(ccResult.structuredPatch);
    });

    it("handles non-Edit tools unchanged", () => {
      const state = makeState();
      accToolStart(state, "tc-1", "Bash", { command: "ls" });
      const result = { stdout: "file1\nfile2" };
      const results = accToolEnd(state, "tc-1", "Bash", result);
      const r = (results[1].hookData as { tool_response: unknown }).tool_response;
      expect(r).toEqual(result);
    });
  });
});
