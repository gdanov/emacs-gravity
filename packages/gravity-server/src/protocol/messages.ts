// messages.ts — Protocol message parsing and validation

import type { TerminalMessage } from "@gravity/shared";
import { PROTOCOL_VERSION } from "@gravity/shared";

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
  // Pi driver control messages (handled in handleTerminalMessage's switch).
  "pi.start",
  "pi.prompt",
  "pi.steer",
  "pi.abort",
  "pi.set-thinking",
  "pi.set-session-name",
  "pi.set-model",
  "pi.resume",
  "pi.compact",
  "pi.new-session",
  "pi.stop",
  "pi.refresh-commands",
  "pi.refresh-models",
]);

/** The `protocol.mismatch` payload, or null when the versions match. */
export interface ProtocolMismatch {
  readonly serverVersion: number;
  readonly clientVersion: number;
  readonly text: string;
}

/**
 * Read a client's protocol version from its `hello`. A client that omits
 * the field is a legacy (pre-versioning) client — treated as version 0.
 */
export function helloProtocolVersion(msg: TerminalMessage): number {
  const raw = (msg as Record<string, unknown>).protocolVersion;
  return typeof raw === "number" ? raw : 0;
}

/**
 * Compare a client's protocol version against the server's. Returns the
 * `protocol.mismatch` payload to send back, or null when they match.
 */
export function protocolMismatch(clientVersion: number): ProtocolMismatch | null {
  if (clientVersion === PROTOCOL_VERSION) return null;
  const text = clientVersion < PROTOCOL_VERSION
    ? `This client speaks protocol v${clientVersion} but the server is v${PROTOCOL_VERSION}. It is out of date and may miss inbox/question updates — rebuild and relaunch it (e.g. \`make menubar\`).`
    : `This client speaks protocol v${clientVersion} but the server is only v${PROTOCOL_VERSION}. Update gravity-server (e.g. \`make restart-server\`).`;
  return { serverVersion: PROTOCOL_VERSION, clientVersion, text };
}

/**
 * Whether a `poll` reply should include an `inbox-items` message.
 *
 * Send while the inbox is non-empty, AND once on the non-empty→empty
 * transition (so read-only clients clear their attention indicator), but
 * not on every poll while it stays empty. `wasNonEmpty` is the per-connection
 * record of whether the previous `inbox-items` send was non-empty.
 */
export function shouldSendInboxOnPoll(itemCount: number, wasNonEmpty: boolean): boolean {
  return itemCount > 0 || wasNonEmpty;
}

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
