// spawn.ts — Subprocess spawning and lifecycle management for pi
//
// Spawns a pi subprocess in RPC mode, connects protocol parser, and
// manages process cleanup on shutdown.

import { spawn, type ChildProcess } from "child_process";
import { appendFileSync, mkdirSync } from "fs";
import { homedir } from "os";
import { join } from "path";
import { PiProtocol } from "./protocol.js";

// Optional raw-event capture for debugging. Set GRAVITY_PI_RAW_LOG to a path
// to dump pi's raw stdout there (newline-delimited JSON, one event per line).
const RAW_LOG = process.env.GRAVITY_PI_RAW_LOG;
import type {
  PiDriver,
  PiDriverOptions,
  ThinkingLevel,
  PiProtocolEvent,
  PiSessionStats,
  ExtensionUIResponsePayload,
} from "./types.js";

/** Path to the pi binary (default: "pi"). */
const PI_BINARY = process.env.PI_BINARY_PATH ?? "pi";

/** Default thinking level if not specified. */
const DEFAULT_THINKING_LEVEL: ThinkingLevel = "medium";

/**
 * Default directory for pi session files. Pi writes one `.jsonl` per session
 * here when started with `--session-dir`. Resume reads from the same place.
 */
const DEFAULT_PI_SESSION_DIR = join(homedir(), ".local", "state", "gravity-pi-sessions");

/**
 * Spawn a pi subprocess and return its control interface.
 *
 * The subprocess runs in RPC mode. Sessions are persisted under
 * `--session-dir` (defaults to ~/.local/state/gravity-pi-sessions) so they
 * can be resumed later via `--session <id-or-path>` or pi's `switch_session`
 * RPC. Protocol parsing and RPC commands are handled by the PiProtocol
 * instance.
 */
export function spawnPiSync(
  options: PiDriverOptions = {},
): {
  driver: PiDriver & { setEventHandler: (h: (evt: PiProtocolEvent) => void) => void };
  process: ChildProcess;
} {
  const cwd = options.cwd ?? process.cwd();
  const thinkingLevel = options.thinkingLevel ?? DEFAULT_THINKING_LEVEL;
  const sessionDir = options.sessionDir ?? DEFAULT_PI_SESSION_DIR;

  // Make sure the session dir exists; pi expects to be able to write into it.
  try {
    mkdirSync(sessionDir, { recursive: true });
  } catch (err) {
    process.stderr.write(`[pi-adapter] could not create session dir ${sessionDir}: ${(err as Error).message}\n`);
  }

  // cwd is set via the spawn() options below; pi inherits it. Pi has no --cwd flag.
  const args = [
    "--mode", "rpc",
    "--session-dir", sessionDir,
    "--thinking", thinkingLevel,
  ];
  if (options.resumeSession) {
    args.push("--session", options.resumeSession);
  }

  // Build environment with optional overrides
  const env: Record<string, string> = { ...process.env } as Record<string, string>;
  if (options.model) env["PI_MODEL"] = options.model;
  if (options.provider) env["PI_PROVIDER"] = options.provider;

  const child: ChildProcess = spawn(PI_BINARY, args, {
    cwd,
    env,
    stdio: ["pipe", "pipe", "pipe"],
  });

  let stopped = false;
  let onPiEvent: ((evt: PiProtocolEvent) => void) | null = null;

  const proto = new PiProtocol({
    onEvent: (evt) => {
      if (onPiEvent) {
        onPiEvent(evt);
      }
    },
    onStderr: (line) => {
      process.stderr.write(`[pi] ${line}\n`);
    },
  });

  // Wire stdout -> protocol parser
  child.stdout?.on("data", (chunk: Buffer) => {
    const s = chunk.toString();
    if (RAW_LOG) {
      try { appendFileSync(RAW_LOG, s); } catch { /* best-effort */ }
    }
    proto.feed(s);
  });

  // Wire stderr -> protocol parser
  child.stderr?.on("data", (chunk: Buffer) => {
    proto.feedStderr(chunk.toString());
  });

  // Handle process exit
  child.on("exit", (code, signal) => {
    if (!stopped) {
      stopped = true;
      const msg = code !== null
        ? `pi subprocess exited with code ${code}`
        : signal
          ? `pi subprocess killed by signal ${signal}`
          : "pi subprocess exited";
      process.stderr.write(`[pi] ${msg}\n`);
    }
  });

  child.on("error", (err) => {
    if (!stopped) {
      stopped = true;
      process.stderr.write(`[pi] subprocess error: ${err.message}\n`);
    }
  });

  // Wire protocol -> subprocess stdin
  proto.setCommandWriter((line: string) => {
    if (child.stdin && !child.stdin.destroyed) {
      child.stdin.write(line);
    }
  });

  // Driver interface
  const driver: PiDriver & { setEventHandler: (h: (evt: PiProtocolEvent) => void) => void } = {
    prompt: (text: string, images?: string[]): Promise<void> => {
      return new Promise((resolve, reject) => {
        if (stopped) {
          reject(new Error("pi subprocess already stopped"));
          return;
        }
        if (child.stdin && !child.stdin.destroyed) {
          const line = PiProtocol.formatPrompt(text, images);
          child.stdin.write(line, (err) => {
            if (err) reject(err);
            else resolve();
          });
        } else {
          reject(new Error("pi stdin unavailable"));
        }
      });
    },

    steer: (text: string): void => {
      if (stopped || !child.stdin || child.stdin.destroyed) return;
      child.stdin.write(PiProtocol.formatSteer(text));
    },

    abort: (): void => {
      if (stopped || !child.stdin || child.stdin.destroyed) return;
      child.stdin.write(PiProtocol.formatAbort());
    },

    setThinkingLevel: (level: ThinkingLevel): void => {
      if (stopped || !child.stdin || child.stdin.destroyed) return;
      child.stdin.write(PiProtocol.formatThinkingLevel(level));
    },

    setModel: (provider: string, modelId: string): void => {
      if (stopped || !child.stdin || child.stdin.destroyed) return;
      child.stdin.write(PiProtocol.formatSetModel(provider, modelId));
    },

    getSessionStats: async (): Promise<PiSessionStats> => {
      if (stopped) throw new Error("pi subprocess already stopped");
      const response = await proto.request({ type: "get_session_stats" });
      if (!response.success) {
        throw new Error(`pi get_session_stats failed: ${response.error ?? "unknown error"}`);
      }
      return (response.data ?? {}) as PiSessionStats;
    },

    getState: async (): Promise<Record<string, unknown>> => {
      if (stopped) throw new Error("pi subprocess already stopped");
      const response = await proto.request({ type: "get_state" });
      if (!response.success) {
        throw new Error(`pi get_state failed: ${response.error ?? "unknown error"}`);
      }
      return (response.data ?? {}) as Record<string, unknown>;
    },

    switchSession: async (sessionPath: string): Promise<boolean> => {
      if (stopped) throw new Error("pi subprocess already stopped");
      const response = await proto.request({ type: "switch_session", sessionPath });
      if (!response.success) {
        throw new Error(`pi switch_session failed: ${response.error ?? "unknown error"}`);
      }
      // Pi may cancel the switch via session_before_switch extension; the
      // response carries `data: { cancelled: bool }`.
      const data = (response.data ?? {}) as { cancelled?: boolean };
      return data.cancelled !== true;
    },

    sendExtensionUIResponse: (payload: ExtensionUIResponsePayload): void => {
      if (stopped || !child.stdin || child.stdin.destroyed) return;
      // Fire-and-forget. Pi correlates by the embedded `id` field, not by
      // RPC request/response — so we use sendCommand instead of request.
      proto.sendCommand({ type: "extension_ui_response", ...payload });
    },

    compact: async (customInstructions?: string) => {
      if (stopped) throw new Error("pi subprocess already stopped");
      const response = await proto.request(
        customInstructions
          ? { type: "compact", customInstructions }
          : { type: "compact" },
        60_000, // compaction can take a while (extra LLM call)
      );
      if (!response.success) {
        throw new Error(`pi compact failed: ${response.error ?? "unknown error"}`);
      }
      const data = (response.data ?? {}) as { summary?: string; tokensBefore?: number };
      return data;
    },

    newSession: async (parentSession?: string) => {
      if (stopped) throw new Error("pi subprocess already stopped");
      const response = await proto.request(
        parentSession
          ? { type: "new_session", parentSession }
          : { type: "new_session" },
      );
      if (!response.success) {
        throw new Error(`pi new_session failed: ${response.error ?? "unknown error"}`);
      }
      const data = (response.data ?? {}) as { cancelled?: boolean };
      return data.cancelled !== true;
    },

    stop: async (): Promise<void> => {
      if (stopped) return;
      stopped = true;

      // Graceful shutdown: send SIGTERM
      if (!child.killed) {
        child.kill("SIGTERM");
      }

      // Close stdin
      if (child.stdin && !child.stdin.destroyed) {
        child.stdin.end();
      }
    },

    setEventHandler: (h: (evt: PiProtocolEvent) => void) => {
      onPiEvent = h;
    },
  };

  return { driver, process: child };
}
