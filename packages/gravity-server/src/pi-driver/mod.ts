// mod.ts — Compose all pi-driver modules, export startPiDriver
//
// Main entry point for the pi adapter. Composes:
// - spawn.ts: subprocess lifecycle
// - protocol.ts: JSONL parsing and RPC commands
// - turn-accumulator.ts: batching pi turns into gravity turns
// - hook-translator.ts: translating pi events to HookData
// - session.ts: effort level and metadata management
//
// Exports startPiDriver() which spawns pi and returns a control interface
// that emits translation results for processing by gravity-server.

import { spawnPiSync } from "./spawn.js";
import { translatePiEvent, createSessionStart, createSessionEnd } from "./hook-translator.js";
import { createAccState } from "./turn-accumulator.js";
import {
  createSessionMetadata,
  updateModel,
  updateThinkingLevel,
  normalizeThinkingLevel,
  thinkingToEffort,
  type SessionMetadata,
} from "./session.js";
import type {
  PiDriverOptions,
  PiProtocolEvent,
  AccState,
  TranslationResult,
  ThinkingLevel,
} from "./types.js";

/** Generate a unique session ID for the pi driver. */
function generateSessionId(): string {
  const now = Date.now().toString(36);
  const random = Math.random().toString(36).substring(2, 10);
  return `pi-${now}-${random}`;
}

/**
 * Callback type for translation results.
 * Called when a pi event yields a gravity event that should be processed.
 */
export type TranslationCallback = (result: TranslationResult) => void;

/**
 * Callback type for session lifecycle events.
 */
export type SessionLifecycleCallback = (event: "start" | "stop" | "error", metadata?: SessionMetadata) => void;

/**
 * Options for startPiDriver.
 */
export interface StartPiDriverOptions extends PiDriverOptions {
  /**
   * The outer session ID (from startPiSession). Used as the routing key for
   * all TranslationResult objects emitted by this driver. If not provided,
   * a new ID is generated internally.
   */
  sessionId?: string;
  /**
   * Called with each translation result from pi events.
   */
  onTranslation: TranslationCallback;
  /**
   * Called on session lifecycle events (start, stop, error).
   * Defaults to no-op.
   */
  onLifecycle?: SessionLifecycleCallback;
}

/**
 * Return value from startPiDriver().
 */
export interface PiDriverInstance {
  /** Send a user prompt to pi. */
  prompt(text: string, images?: string[]): Promise<void>;
  /** Send a steering message to pi. */
  steer(text: string): void;
  /** Abort current execution. */
  abort(): void;
  /** Set thinking level at runtime. */
  setThinkingLevel(level: ThinkingLevel): void;
  /** Set thinking level via string (will be normalized). */
  setEffortLevel(level: string): void;
  /** Stop the pi subprocess and clean up. */
  stop(): Promise<void>;
  /** Get current session metadata. */
  getMetadata(): SessionMetadata;
}

/**
 * Start the pi driver.
 *
 * Spawns a pi subprocess in RPC mode and translates its events into
 * gravity-server's HookData format, calling onTranslation for each event.
 *
 * @example
 * ```ts
 * const driver = await startPiDriver({
 *   cwd: "/path/to/project",
 *   thinkingLevel: "medium",
 *   onTranslation: (result) => {
 *     const patches = handleEvent(
 *       result.hookEvent,
 *       sessionId,
 *       cwd,
 *       result.hookData,
 *       null
 *     );
 *     // broadcast patches to terminals...
 *   },
 * });
 *
 * await driver.prompt("Hello, pi!");
 * ```
 */
export function startPiDriver(options: StartPiDriverOptions): PiDriverInstance {
  const sessionId = options.sessionId ?? generateSessionId();
  const cwd = options.cwd ?? process.cwd();
  const thinkingLevel = options.thinkingLevel ?? "medium";

  // Create accumulator state — uses the outer sessionId for all event routing
  let state = createAccState(sessionId, cwd, thinkingToEffort(thinkingLevel));

  // Create session metadata
  let metadata = createSessionMetadata(sessionId, cwd, thinkingLevel);

  // Callback for session lifecycle
  const onLifecycle = options.onLifecycle ?? (() => {});

  // Spawn pi subprocess
  const { driver, process: childProcess } = spawnPiSync({
    cwd,
    thinkingLevel,
    model: options.model,
    provider: options.provider,
    piBinaryPath: options.piBinaryPath,
  });

  // Set up event handler
  driver.setEventHandler((evt: PiProtocolEvent) => {
    // Translate pi event into zero or more gravity events.
    const result = translatePiEvent(evt.event, state);
    if (result.kind === "emit") {
      for (const r of result.results) {
        options.onTranslation(r);
      }
    }

    // Handle special events for driver-level metadata + lifecycle callbacks.
    switch (evt.event.type) {
      case "model_select":
        metadata = updateModel(metadata, (evt.event as { model: string; provider: string }).model, (evt.event as { model: string; provider: string }).provider);
        state.modelName = (evt.event as { model: string }).model;
        break;
      case "agent_start":
        onLifecycle("start", metadata);
        break;
      case "agent_end":
        onLifecycle("stop", metadata);
        break;
    }
  });

  // Handle process errors
  childProcess.on("error", (err) => {
    onLifecycle("error", metadata);
    process.stderr.write(`[pi-adapter] process error: ${err.message}\n`);
  });

  childProcess.on("exit", (code, signal) => {
    // Emit SessionEnd if session was active
    const sessionEnd = createSessionEnd(state);
    options.onTranslation(sessionEnd);
    onLifecycle("stop", metadata);
  });

  // Return driver interface
  return {
    prompt: (text: string, images?: string[]) => {
      return driver.prompt(text, images);
    },

    steer: (text: string) => {
      driver.steer(text);
    },

    abort: () => {
      driver.abort();
    },

    setThinkingLevel: (level: ThinkingLevel) => {
      driver.setThinkingLevel(level);
      metadata = updateThinkingLevel(metadata, level);
      state.effortLevel = thinkingToEffort(level);
    },

    setEffortLevel: (level: string) => {
      const normalized = normalizeThinkingLevel(level);
      driver.setThinkingLevel(normalized);
      metadata = updateThinkingLevel(metadata, normalized);
      state.effortLevel = thinkingToEffort(normalized);
    },

    stop: () => {
      return driver.stop();
    },

    getMetadata: () => {
      return metadata;
    },
  };
}