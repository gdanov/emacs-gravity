// protocol.ts — JSONL parser and RPC command formatter for pi
//
// Reads pi's JSONL stdout and emits typed PiEvent objects.
// Formats RPC commands (prompt, steer, abort, set_thinking_level) to JSONL for pi's stdin.

import type { PiEvent, PiCommand, ThinkingLevel, PiProtocolEvent } from "./types.js";

/**
 * Parse a single JSON line into a PiEvent (or unknown fallback).
 */
function parseJsonLine(line: string): PiEvent {
  try {
    const parsed = JSON.parse(line);
    // Validate it has a type field
    if (parsed && typeof parsed === "object" && "type" in parsed) {
      return parsed as PiEvent;
    }
    // Unknown event with type field
    return { type: "unknown", ...parsed };
  } catch {
    // Malformed JSON — wrap as unknown event
    return { type: "unknown", raw: line } as PiEvent;
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

  constructor(options: PiProtocolOptions) {
    this.onEvent = options.onEvent;
    this.onStderr = options.onStderr ?? ((line) => {
      process.stderr.write(`[pi] ${line}\n`);
    });
  }

  /**
   * Feed a text chunk from pi's stdout into the parser.
   * Calls onEvent for each complete JSON line.
   */
  feed(data: string): void {
    this.buffer += data;
    let newlineIdx: number;
    while ((newlineIdx = this.buffer.indexOf("\n")) !== -1) {
      const line = this.buffer.substring(0, newlineIdx).trim();
      this.buffer = this.buffer.substring(newlineIdx + 1);
      if (line.length === 0) continue;
      const event = parseJsonLine(line);
      this.onEvent({ event, raw: line });
    }
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
   * Format any PiCommand to a JSONL string.
   */
  static formatCommand(cmd: PiCommand): string {
    switch (cmd.type) {
      case "prompt":
        return PiProtocol.formatPrompt(cmd.text, cmd.images);
      case "steer":
        return PiProtocol.formatSteer(cmd.text);
      case "abort":
        return PiProtocol.formatAbort();
      case "set_thinking_level":
        return PiProtocol.formatThinkingLevel(cmd.level);
    }
  }

  /**
   * Format a prompt command for pi's stdin.
   */
  static formatPrompt(text: string, images?: string[]): string {
    const cmd: PiCommand = { type: "prompt", text };
    if (images && images.length > 0) {
      cmd.images = images;
    }
    return JSON.stringify(cmd) + "\n";
  }

  /**
   * Format a steer command for pi's stdin.
   */
  static formatSteer(text: string): string {
    return JSON.stringify({ type: "steer", text } as PiCommand) + "\n";
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
}
