// logger.ts — Fire-and-forget append-only log writer.
//
// Per the spec, this is the emitter's only persistent observation channel:
// every drop (queue overflow OR source-side coalesce distinguishing
// detail), every reconnect attempt, every send failure, etc. is logged
// here so a pi session's emitter activity can be reconstructed after the
// fact — including from `--mode json` where stdout is owned by pi.
//
// Critical contract: logToFile() MUST NEVER throw and MUST NEVER produce
// an unhandled rejection. The implementation catches every error inside
// the async chain. Callers can therefore call it from synchronous event
// handlers without any extra plumbing.
//
// Default path: process.env.GRAVITY_PI_EMITTER_LOG ??
//               ~/.local/state/gravity-pi-emitter.log
// Tests must always inject an explicit path (under /tmp) — never write
// to a real home directory.

import { appendFile, mkdir } from "node:fs/promises";
import { dirname, join } from "node:path";
import { homedir } from "node:os";

/** Default log path env-var override. */
export const GRAVITY_PI_EMITTER_LOG_ENV = "GRAVITY_PI_EMITTER_LOG";

/** Build the default log path from an optional HOME override. */
export function defaultLogPath(env: NodeJS.ProcessEnv = process.env): string {
  const home = env["HOME"] ?? homedir();
  return join(home, ".local", "state", "gravity-pi-emitter.log");
}

/**
 * Per-target-path write chain: each entry holds the tail of the
 * promise chain that appends to a particular log file. New
 * `logToFile()` calls chain themselves onto the current tail so writes
 * to the SAME path execute strictly in call order, while writes to
 * DIFFERENT paths remain independent (each path has its own chain).
 *
 * The chain's terminal `.catch(() => {})` ensures no unhandledRejection
 * can escape into a synchronous event handler — a logging failure must
 * never propagate.
 */
const writeChains: Map<string, Promise<void>> = new Map();

/**
 * Append one line to the log file. Fire-and-forget.
 *
 * Format: `<ISO timestamp> <message>\n` — one line per call, never
 * multi-line, so tailing the file is trivial.
 *
 * Behavior:
 *  - If the parent directory does not exist, it is created (recursive).
 *  - The write itself is fire-and-forget; the returned Promise is not
 *    exposed to the caller (the function returns void synchronously).
 *  - Writes to the SAME path are serialized through a per-path
 *    promise chain, so lines are appended in call order even when
 *    many callers invoke `logToFile` synchronously in quick
 *    succession. Writes to DIFFERENT paths remain independent
 *    (parallel) — only the same-target case is serialized.
 *  - Any error from mkdir/appendFile is silently caught — a logging
 *    failure must never propagate into a synchronous event handler.
 *
 * @param message  Free-form text. Should not contain newlines.
 * @param logPath  Absolute path to write to. If omitted, defaults to
 *                 the GRAVITY_PI_EMITTER_LOG env var, then
 *                 ~/.local/state/gravity-pi-emitter.log.
 */
export function logToFile(message: string, logPath?: string): void {
  const target = logPath ?? process.env[GRAVITY_PI_EMITTER_LOG_ENV] ?? defaultLogPath();
  const line = `${new Date().toISOString()} ${message}\n`;

  // Chain onto the existing per-path tail so appends are strictly
  // ordered per-path. The terminal .catch swallows any I/O error so
  // no unhandledRejection escapes into headless pi; logToFile itself
  // returns void synchronously.
  const prev = writeChains.get(target) ?? Promise.resolve();
  const next = prev
    .then(() => mkdir(dirname(target), { recursive: true }))
    .then(() => appendFile(target, line, "utf8"))
    .catch(() => {
      /* swallow — logging must never throw */
    });
  writeChains.set(target, next);
}