// Shared logging utility for bridge and daemon.
import { appendFileSync } from "fs";

export type LogLevel = 'debug' | 'info' | 'warn' | 'error';
const LOG_LEVELS: Record<LogLevel, number> = { debug: 0, info: 1, warn: 2, error: 3 };

let currentLogLevel: LogLevel = (process.env.EMACS_BRIDGE_LOG_LEVEL as LogLevel) || 'warn';
let logFile = "/tmp/emacs-bridge.log";

export function setLogLevel(level: LogLevel): void { currentLogLevel = level; }
export function setLogFile(path: string): void { logFile = path; }

export function log(msg: string, level: LogLevel = 'debug'): void {
  if (LOG_LEVELS[level] < LOG_LEVELS[currentLogLevel]) return;
  try {
    const timestamp = new Date().toISOString();
    appendFileSync(logFile, `[${timestamp}] [${level.toUpperCase()}] ${msg}\n`);
  } catch {
    // ignore logging errors
  }
}
