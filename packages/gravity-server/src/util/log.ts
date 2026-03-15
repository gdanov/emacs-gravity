// log.ts — Logging for gravity-server

import { appendFileSync, mkdirSync } from "fs";
import { dirname } from "path";

const LOG_PATH = "/tmp/gravity-server.log";

export function log(message: string, level: "debug" | "info" | "warn" | "error" = "info"): void {
  const ts = new Date().toISOString();
  const line = `[${ts}] [${level}] ${message}\n`;
  try {
    appendFileSync(LOG_PATH, line);
  } catch {
    // Ignore write failures
  }
}
