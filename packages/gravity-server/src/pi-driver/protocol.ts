// protocol.ts — JSONL parser and RPC command formatter for pi
//
// Reads pi's JSONL stdout, dispatching events to onEvent and RPC responses
// (`{type: "response", ...}`) to outstanding command promises.
// Formats RPC commands (prompt, steer, abort, set_thinking_level, set_model,
// and request/response style commands like get_session_stats / get_state)
// to JSONL for pi's stdin.

import type { PiEvent, PiCommand, ThinkingLevel, PiProtocolEvent, PiResponse } from "./types.js";

/** Parsed form of one JSONL line from pi's stdout. */
type ParsedLine =
  | { kind: "event"; event: PiEvent }
  | { kind: "response"; response: PiResponse };

/**
 * Parse a single JSON line. Pi 0.74 emits two kinds of stdout messages:
 * - Events: `{type: "<event_name>", ...}` (no `command` field)
 * - Responses: `{type: "response", command: "...", success: bool, ...}`
 */
function parseJsonLine(line: string): ParsedLine {
  try {
    const parsed = JSON.parse(line);
    if (parsed && typeof parsed === "object" && "type" in parsed) {
      if (parsed.type === "response" && typeof parsed.command === "string") {
        return { kind: "response", response: parsed as PiResponse };
      }
      return { kind: "event", event: parsed as PiEvent };
    }
    return { kind: "event", event: { type: "unknown", ...parsed } as PiEvent };
  } catch {
    return { kind: "event", event: { type: "unknown", raw: line } as PiEvent };
  }
}

/**
 * Options for creating a PiProtocol instance.
 */
export interface PiProtocolOptions {
  /**
   * Called for each parsed pi event.
   * May be called multiple times per chunk (one event per line).
   */
  onEvent: (evt: PiProtocolEvent) => void;
  /**
   * Called when pi's stderr receives a line.
   * Defaults to logging to stderr.
   */
  onStderr?: (line: string) => void;
}

/**
 * Generate a unique request id for RPC correlation. Pi accepts any string;
 * the adapter uses a monotonic counter with a per-process prefix.
 */
let requestCounter = 0;
function nextRequestId(): string {
  return `gr-${process.pid}-${++requestCounter}`;
}

/**
 * PiProtocol — JSONL parser for pi's stdout, RPC command formatter for stdin.
 *
 * Usage:
 * ```ts
 * const proto = new PiProtocol({
 *   onEvent: (evt) => console.log("pi event:", evt.event.type),
 *   onStderr: (line) => console.error("[pi]", line),
 * });
 *
 * // Pipe stdout chunks from pi subprocess
 * pi.stdout.on("data", (chunk) => proto.feed(chunk.toString()));
 *
 * // Send a prompt
 * proto.sendCommand({ type: "prompt", text: "Hello, pi!" });
 * ```
 */
export class PiProtocol {
  private buffer = "";
  private readonly onEvent: (evt: PiProtocolEvent) => void;
  private readonly onStderr: (line: string) => void;
  private commandWriter: ((line: string) => void) | null = null;
  /** In-flight RPC requests awaiting a matching response (keyed by request id). */
  private pendingRequests: Map<string, {
    resolve: (response: PiResponse) => void;
    reject: (err: Error) => void;
    timer: NodeJS.Timeout;
  }> = new Map();

  constructor(options: PiProtocolOptions) {
    this.onEvent = options.onEvent;
    this.onStderr = options.onStderr ?? ((line) => {
      process.stderr.write(`[pi] ${line}\n`);
    });
  }

  /**
   * Feed a text chunk from pi's stdout into the parser.
   * Dispatches events to onEvent and responses to outstanding request
   * promises (matched by id when present).
   */
  feed(data: string): void {
    this.buffer += data;
    let newlineIdx: number;
    while ((newlineIdx = this.buffer.indexOf("\n")) !== -1) {
      const line = this.buffer.substring(0, newlineIdx).trim();
      this.buffer = this.buffer.substring(newlineIdx + 1);
      if (line.length === 0) continue;
      const parsed = parseJsonLine(line);
      if (parsed.kind === "response") {
        this.dispatchResponse(parsed.response);
      } else {
        this.onEvent({ event: parsed.event, raw: line });
      }
    }
  }

  /** Route a response to its waiter, or drop it if unmatched. */
  private dispatchResponse(response: PiResponse): void {
    const id = response.id;
    if (!id) {
      // Untagged response — could not be correlated. Pi sends these for
      // commands that didn't carry an id; we don't currently consume them.
      return;
    }
    const pending = this.pendingRequests.get(id);
    if (!pending) return;
    this.pendingRequests.delete(id);
    clearTimeout(pending.timer);
    pending.resolve(response);
  }

  /**
   * Send an RPC command with request/response correlation. Returns a promise
   * resolving to pi's response. Times out after `timeoutMs` (default 10s).
   *
   * Use for commands where the caller cares about the response: get_state,
   * get_session_stats, set_model, etc. For fire-and-forget commands use
   * `sendCommand`.
   */
  request(command: PiCommand, timeoutMs = 10_000): Promise<PiResponse> {
    if (!this.commandWriter) {
      return Promise.reject(new Error("pi command writer not connected"));
    }
    const id = nextRequestId();
    const line = PiProtocol.formatCommand(command, id);
    return new Promise<PiResponse>((resolve, reject) => {
      const timer = setTimeout(() => {
        this.pendingRequests.delete(id);
        reject(new Error(`pi RPC ${command.type} timed out after ${timeoutMs}ms`));
      }, timeoutMs);
      this.pendingRequests.set(id, { resolve, reject, timer });
      this.commandWriter!(line);
    });
  }

  /**
   * Feed a text chunk from pi's stderr.
   * Passes through onStderr callback.
   */
  feedStderr(data: string): void {
    const lines = data.split("\n");
    for (const line of lines) {
      if (line.trim().length > 0) {
        this.onStderr(line);
      }
    }
  }

  /**
   * Flush any remaining buffered content (should be called when stdin closes).
   * Returns the remaining buffer if any.
   */
  flush(): string {
    const remaining = this.buffer;
    this.buffer = "";
    return remaining;
  }

  /**
   * Set the command writer (called by spawn.ts to connect to subprocess stdin).
   */
  setCommandWriter(writer: (line: string) => void): void {
    this.commandWriter = writer;
  }

  /**
   * Send a command to pi's stdin.
   */
  sendCommand(command: PiCommand): void {
    if (this.commandWriter) {
      this.commandWriter(PiProtocol.formatCommand(command));
    }
  }

  /**
   * Format any PiCommand to a JSONL string. If `id` is provided, it is
   * attached for request/response correlation (pi echoes it on the
   * matching `{type:"response", id}` line).
   */
  static formatCommand(cmd: PiCommand, id?: string): string {
    const withId = (obj: object) => (id ? { ...obj, id } : obj);
    switch (cmd.type) {
      case "prompt": {
        const base: Record<string, unknown> = { type: "prompt", message: cmd.message };
        if (cmd.images && cmd.images.length > 0) base.images = cmd.images;
        return JSON.stringify(withId(base)) + "\n";
      }
      case "steer":
        return JSON.stringify(withId({ type: "steer", message: cmd.message })) + "\n";
      case "abort":
        return JSON.stringify(withId({ type: "abort" })) + "\n";
      case "set_thinking_level":
        return JSON.stringify(withId({ type: "set_thinking_level", level: cmd.level })) + "\n";
      case "set_model":
        return JSON.stringify(withId({ type: "set_model", provider: cmd.provider, modelId: cmd.modelId })) + "\n";
      case "get_session_stats":
        return JSON.stringify(withId({ type: "get_session_stats" })) + "\n";
      case "get_state":
        return JSON.stringify(withId({ type: "get_state" })) + "\n";
      case "get_commands":
        return JSON.stringify(withId({ type: "get_commands" })) + "\n";
      case "switch_session":
        return JSON.stringify(withId({ type: "switch_session", sessionPath: cmd.sessionPath })) + "\n";
      case "compact": {
        const body: Record<string, unknown> = { type: "compact" };
        if (cmd.customInstructions) body.customInstructions = cmd.customInstructions;
        return JSON.stringify(withId(body)) + "\n";
      }
      case "new_session": {
        const body: Record<string, unknown> = { type: "new_session" };
        if (cmd.parentSession) body.parentSession = cmd.parentSession;
        return JSON.stringify(withId(body)) + "\n";
      }
      case "extension_ui_response": {
        // extension_ui_response carries pi's own request id (`cmd.id`) — do
        // NOT overwrite it with the optional RPC correlation id. Pi matches
        // the response back to its in-flight UI request by the inner id.
        const body: Record<string, unknown> = { type: "extension_ui_response", id: cmd.id };
        if (cmd.value !== undefined) body.value = cmd.value;
        if (cmd.confirmed !== undefined) body.confirmed = cmd.confirmed;
        if (cmd.cancelled !== undefined) body.cancelled = cmd.cancelled;
        return JSON.stringify(body) + "\n";
      }
    }
  }

  /**
   * Format a prompt command for pi's stdin. Pi expects { message, images? } —
   * field name is `message`, not `text` (verified against pi 0.74).
   */
  static formatPrompt(text: string, images?: string[]): string {
    const cmd: PiCommand = { type: "prompt", message: text };
    if (images && images.length > 0) {
      cmd.images = images;
    }
    return JSON.stringify(cmd) + "\n";
  }

  /**
   * Format a steer command for pi's stdin.
   */
  static formatSteer(text: string): string {
    return JSON.stringify({ type: "steer", message: text } as PiCommand) + "\n";
  }

  /**
   * Format an abort command for pi's stdin.
   */
  static formatAbort(): string {
    return JSON.stringify({ type: "abort" } as PiCommand) + "\n";
  }

  /**
   * Format a set_thinking_level command for pi's stdin.
   */
  static formatThinkingLevel(level: ThinkingLevel): string {
    return JSON.stringify({ type: "set_thinking_level", level } as PiCommand) + "\n";
  }

  /**
   * Format a set_model command for pi's stdin.
   * Pi expects { type: "set_model", provider, modelId } (verified against
   * pi 0.74 RPC docs).
   */
  static formatSetModel(provider: string, modelId: string): string {
    return JSON.stringify({ type: "set_model", provider, modelId } as PiCommand) + "\n";
  }
}
