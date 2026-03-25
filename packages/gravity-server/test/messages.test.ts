import { describe, it, expect } from "vitest";
import { parseTerminalMessage, isHookMessage } from "../src/protocol/messages.js";

describe("isHookMessage", () => {
  it("returns true for a hook-style message with event and session_id", () => {
    const msg = {
      event: "SessionStart",
      session_id: "test-123",
      cwd: "/tmp",
      pid: 1234,
      data: {},
      needs_response: false,
    };
    expect(isHookMessage(msg)).toBe(true);
  });

  it("returns true for any hook event type", () => {
    for (const event of ["PreToolUse", "PostToolUse", "Stop", "SubagentStart", "SessionEnd"]) {
      expect(isHookMessage({ event, session_id: "s1" })).toBe(true);
    }
  });

  it("returns false for a valid terminal message", () => {
    const msg = { type: "request.overview" };
    expect(isHookMessage(msg)).toBe(false);
  });

  it("returns false when event is not a string", () => {
    expect(isHookMessage({ event: 42, session_id: "s1" })).toBe(false);
  });

  it("returns false when session_id is not a string", () => {
    expect(isHookMessage({ event: "SessionStart", session_id: 123 })).toBe(false);
  });

  it("returns false for empty object", () => {
    expect(isHookMessage({})).toBe(false);
  });
});

describe("parseTerminalMessage rejects hook messages", () => {
  it("returns null for a hook-style SessionStart message", () => {
    const hookMsg = JSON.stringify({
      event: "SessionStart",
      session_id: "test-abc",
      cwd: "/tmp",
      pid: 1234,
      data: {},
      needs_response: false,
    });
    expect(parseTerminalMessage(hookMsg)).toBeNull();
  });

  it("returns null for a hook-style PreToolUse message", () => {
    const hookMsg = JSON.stringify({
      event: "PreToolUse",
      session_id: "test-xyz",
      cwd: "/tmp/project",
      pid: 5678,
      data: { tool_name: "Read", tool_use_id: "tu_001", tool_input: {} },
      needs_response: false,
    });
    expect(parseTerminalMessage(hookMsg)).toBeNull();
  });

  it("returns null for a bidirectional hook-style PermissionRequest", () => {
    const hookMsg = JSON.stringify({
      event: "PermissionRequest",
      session_id: "test-perm",
      cwd: "/tmp",
      pid: 9999,
      data: { tool_name: "Bash", tool_input: { command: "rm -rf /" } },
      needs_response: true,
    });
    expect(parseTerminalMessage(hookMsg)).toBeNull();
  });

  it("still parses valid terminal messages correctly", () => {
    const termMsg = JSON.stringify({ type: "request.overview" });
    const result = parseTerminalMessage(termMsg);
    expect(result).not.toBeNull();
    expect(result!.type).toBe("request.overview");
  });

  it("still returns null for unknown terminal message types", () => {
    const badMsg = JSON.stringify({ type: "unknown.type" });
    expect(parseTerminalMessage(badMsg)).toBeNull();
  });

  it("still returns null for invalid JSON", () => {
    expect(parseTerminalMessage("not json at all")).toBeNull();
  });
});
