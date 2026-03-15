import { Logger } from "effect";
import { appendFileSync } from "fs";

const LOG_FILE = "/tmp/emacs-bridge.log";

const FileLogger = Logger.make(({ message, logLevel }) => {
  try {
    const timestamp = new Date().toISOString();
    const level = typeof logLevel === "string" ? logLevel.toUpperCase() : "LOG";
    const text = Array.isArray(message)
      ? message.map((m: unknown) => typeof m === "string" ? m : JSON.stringify(m)).join(" ")
      : typeof message === "string" ? message : JSON.stringify(message);
    appendFileSync(LOG_FILE, `[${timestamp}] [${level}] ${text}\n`);
  } catch {
    // ignore logging errors
  }
});

export const LoggerLive = Logger.layer([FileLogger]);
