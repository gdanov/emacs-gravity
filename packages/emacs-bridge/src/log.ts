// Shared logging utility for bridge and daemon.
import { appendFileSync, existsSync, mkdirSync, statSync, renameSync, unlinkSync } from "fs";
import { dirname, join } from "path";

export type LogLevel = 'debug' | 'info' | 'warn' | 'error';
const LOG_LEVELS: Record<LogLevel, number> = { debug: 0, info: 1, warn: 2, error: 3 };

let currentLogLevel: LogLevel = (process.env.EMACS_BRIDGE_LOG_LEVEL as LogLevel) || 'warn';
let logFile = "/tmp/emacs-bridge.log";
const MAX_SIZE = parseInt(process.env.EMACS_BRIDGE_LOG_MAX_SIZE || "1048576", 10); // 1MB
let rotationChecked = false;

export function setLogLevel(level: LogLevel): void { currentLogLevel = level; }
export function setLogFile(path: string): void { logFile = path; }

export function initLogForSession(transcriptPath: string | undefined): void {
  if (!transcriptPath) return;
  const transcriptDir = dirname(transcriptPath);
  const gravityDir = join(transcriptDir, "gravity");
  if (!existsSync(gravityDir)) {
    mkdirSync(gravityDir, { recursive: true });
  }
  logFile = join(gravityDir, "bridge.log");
}

function rotateIfNeeded(path: string, maxSize: number): void {
  try {
    const stats = statSync(path);
    if (stats.size > maxSize) {
      const backup = path + ".1";
      try { unlinkSync(backup); } catch {}
      renameSync(path, backup);
    }
  } catch {
    // File doesn't exist yet or stat failed
  }
}

export function log(msg: string, level: LogLevel = 'debug'): void {
  if (LOG_LEVELS[level] < LOG_LEVELS[currentLogLevel]) return;
  try {
    if (!rotationChecked) {
      rotationChecked = true;
      rotateIfNeeded(logFile, MAX_SIZE);
    }
    const timestamp = new Date().toISOString();
    appendFileSync(logFile, `[${timestamp}] [${level.toUpperCase()}] ${msg}\n`);
  } catch {
    // ignore logging errors
  }
}
