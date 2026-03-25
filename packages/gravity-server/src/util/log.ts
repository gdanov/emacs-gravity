// log.ts — Logging for gravity-server

import { appendFileSync, statSync, renameSync, unlinkSync } from "fs";

export const LOG_PATH = process.env.GRAVITY_LOG_PATH || "/tmp/gravity-server.log";
const MAX_SIZE = parseInt(process.env.GRAVITY_LOG_MAX_SIZE || "2097152", 10); // 2MB
const CHECK_INTERVAL = 100;

let writeCount = 0;

function rotateIfNeeded(): void {
  try {
    const stats = statSync(LOG_PATH);
    if (stats.size > MAX_SIZE) {
      const backup = LOG_PATH + ".1";
      try { unlinkSync(backup); } catch {}
      renameSync(LOG_PATH, backup);
    }
  } catch {
    // File doesn't exist yet or stat failed
  }
}

export function log(message: string, level: "debug" | "info" | "warn" | "error" = "info"): void {
  const ts = new Date().toISOString();
  const line = `[${ts}] [${level}] ${message}\n`;
  try {
    appendFileSync(LOG_PATH, line);
    if (++writeCount >= CHECK_INTERVAL) {
      writeCount = 0;
      rotateIfNeeded();
    }
  } catch {
    // Ignore write failures
  }
}
