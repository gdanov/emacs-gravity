import { describe, it, expect } from "vitest";
import {
  parseTerminalMessage,
  isHookMessage,
  helloProtocolVersion,
  protocolMismatch,
  shouldSendInboxOnPoll,
} from "../src/protocol/messages.js";
import { PROTOCOL_VERSION } from "@gravity/shared";

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

  it("parses a hello and preserves protocolVersion", () => {
    const hello = JSON.stringify({ type: "hello", capabilities: [], protocolVersion: PROTOCOL_VERSION });
    const result = parseTerminalMessage(hello);
    expect(result).not.toBeNull();
    expect(result!.type).toBe("hello");
    expect(helloProtocolVersion(result!)).toBe(PROTOCOL_VERSION);
  });
});

describe("protocol version handshake", () => {
  it("treats a hello without protocolVersion as legacy version 0", () => {
    const hello = parseTerminalMessage(JSON.stringify({ type: "hello", capabilities: [] }))!;
    expect(helloProtocolVersion(hello)).toBe(0);
  });

  it("returns no mismatch when the client matches the server", () => {
    expect(protocolMismatch(PROTOCOL_VERSION)).toBeNull();
  });

  it("flags an older (legacy) client and tells it to rebuild", () => {
    const mismatch = protocolMismatch(0);
    expect(mismatch).not.toBeNull();
    expect(mismatch!.serverVersion).toBe(PROTOCOL_VERSION);
    expect(mismatch!.clientVersion).toBe(0);
    expect(mismatch!.text).toContain("out of date");
  });

  it("flags a newer client and tells the user to update the server", () => {
    const mismatch = protocolMismatch(PROTOCOL_VERSION + 1);
    expect(mismatch).not.toBeNull();
    expect(mismatch!.text).toContain("Update gravity-server");
  });

  it("legacy push-era menu bar (no version) is flagged — the regression we hit", () => {
    const staleHello = parseTerminalMessage(JSON.stringify({ type: "hello", capabilities: [] }))!;
    const mismatch = protocolMismatch(helloProtocolVersion(staleHello));
    expect(mismatch).not.toBeNull();
  });
});

describe("inbox poll delivery (read-only client clears on empty)", () => {
  it("sends while non-empty", () => {
    expect(shouldSendInboxOnPoll(1, false)).toBe(true);
    expect(shouldSendInboxOnPoll(3, true)).toBe(true);
  });

  it("sends exactly once on the non-empty→empty transition, then stops", () => {
    // Simulate the per-connection flag across a sequence of polls, mirroring
    // the handler: a question arrives, then is answered (inbox empties).
    let wasNonEmpty = false;

    // poll while empty before any question: no send
    expect(shouldSendInboxOnPoll(0, wasNonEmpty)).toBe(false);

    // question arrives → poll sees 1 item → send, remember non-empty
    expect(shouldSendInboxOnPoll(1, wasNonEmpty)).toBe(true);
    wasNonEmpty = 1 > 0;

    // user answers → inbox empty → poll MUST still send (the empty array)
    // so the menu bar clears its attention indicator
    expect(shouldSendInboxOnPoll(0, wasNonEmpty)).toBe(true);
    wasNonEmpty = 0 > 0;

    // subsequent empty polls: no churn
    expect(shouldSendInboxOnPoll(0, wasNonEmpty)).toBe(false);
    expect(shouldSendInboxOnPoll(0, wasNonEmpty)).toBe(false);
  });
});
