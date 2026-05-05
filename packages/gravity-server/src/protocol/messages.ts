// messages.ts — Protocol message parsing and validation

import type { TerminalMessage } from "@gravity/shared";

const VALID_TERMINAL_MESSAGE_TYPES = new Set([
  "hello",
  "action.permission",
  "action.question",
  "action.plan-review",
  "action.turn-auto-approve",
  "request.session",
  "request.overview",
  "request.resync",
  "hint.session-dead",
  "poll",
]);

/** Check if a parsed JSON object looks like a hook-style message (has `event` field). */
export function isHookMessage(obj: Record<string, unknown>): boolean {
  return typeof obj.event === "string" && typeof obj.session_id === "string";
}

/** Parse a raw JSON line into a TerminalMessage. Returns null on invalid input. */
export function parseTerminalMessage(line: string): TerminalMessage | null {
  try {
    const msg = JSON.parse(line);
    if (typeof msg !== "object" || msg === null) return null;
    if (isHookMessage(msg)) return null;
    if (!VALID_TERMINAL_MESSAGE_TYPES.has(msg.type)) return null;
    return msg as TerminalMessage;
  } catch {
    return null;
  }
}
