// logger.test.ts — Tests for the fire-and-forget log writer.

import { describe, it, expect, afterEach } from "vitest";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { logToFile } from "./logger.js";
import { GRAVITY_PI_EMITTER_LOG_ENV } from "./logger.js";

let tmpDir = "";
const SAVED: Record<string, string | undefined> = {};

afterEach(() => {
  if (tmpDir) {
    try {
      rmSync(tmpDir, { recursive: true, force: true });
    } catch {
      /* ignore */
    }
    tmpDir = "";
  }
  // Restore env vars we mutated.
  for (const [k, v] of Object.entries(SAVED)) {
    if (v === undefined) delete process.env[k];
    else process.env[k] = v;
  }
  for (const k of Object.keys(SAVED)) delete SAVED[k];
});

function freshDir(): string {
  tmpDir = mkdtempSync(join(tmpdir(), "gravity-pi-logger-"));
  return tmpDir;
}

async function waitFor(predicate: () => boolean, timeoutMs = 2000): Promise<void> {
  const start = Date.now();
  while (!predicate() && Date.now() - start < timeoutMs) {
    await new Promise((r) => setTimeout(r, 5));
  }
  if (!predicate()) throw new Error(`waitFor timed out after ${timeoutMs}ms`);
}

describe("logToFile", () => {
  it("writes an ISO-prefixed line to an injected path", async () => {
    const dir = freshDir();
    const path = join(dir, "log.txt");
    logToFile("hello world", path);
    await waitFor(() => {
      try {
        return readFileSync(path, "utf8").includes("hello world");
      } catch {
        return false;
      }
    });
    const content = readFileSync(path, "utf8");
    expect(content).toMatch(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.* hello world\n$/);
  });

  it("appends multiple lines, each on its own line", async () => {
    const dir = freshDir();
    const path = join(dir, "log.txt");
    logToFile("one", path);
    logToFile("two", path);
    logToFile("three", path);
    await waitFor(() => {
      try {
        const c = readFileSync(path, "utf8");
        return c.split("\n").filter(Boolean).length >= 3;
      } catch {
        return false;
      }
    });
    const lines = readFileSync(path, "utf8").split("\n").filter(Boolean);
    expect(lines.length).toBe(3);
    expect(lines[0]).toContain("one");
    expect(lines[1]).toContain("two");
    expect(lines[2]).toContain("three");
  });

  it("creates the parent directory if missing", async () => {
    const dir = freshDir();
    const path = join(dir, "deep", "nested", "log.txt");
    logToFile("nested", path);
    await waitFor(() => {
      try {
        return readFileSync(path, "utf8").includes("nested");
      } catch {
        return false;
      }
    });
    expect(readFileSync(path, "utf8")).toContain("nested");
  });

  it("never throws synchronously even if the path is unwritable", () => {
    // A path under /dev/null/<file> resolves to a non-writable location
    // on Linux/macOS. logToFile must NOT throw.
    const unwritable = "/dev/null/cannot-write-here/log.txt";
    expect(() => logToFile("test", unwritable)).not.toThrow();
  });

  it("does not produce an unhandledRejection when the write fails", async () => {
    const captured: unknown[] = [];
    const handler = (reason: unknown): void => {
      captured.push(reason);
    };
    process.on("unhandledRejection", handler);
    try {
      logToFile("test", "/dev/null/cannot-write-here/log.txt");
      // Give any detached promise a tick to settle.
      await new Promise((r) => setTimeout(r, 50));
      expect(captured).toEqual([]);
    } finally {
      process.removeListener("unhandledRejection", handler);
    }
  });

  it("honors the GRAVITY_PI_EMITTER_LOG env var when no explicit path given", async () => {
    const dir = freshDir();
    const path = join(dir, "envlog.txt");
    SAVED[GRAVITY_PI_EMITTER_LOG_ENV] = process.env[GRAVITY_PI_EMITTER_LOG_ENV];
    process.env[GRAVITY_PI_EMITTER_LOG_ENV] = path;
    try {
      logToFile("via env");
      await waitFor(() => {
        try {
          return readFileSync(path, "utf8").includes("via env");
        } catch {
          return false;
        }
      });
      expect(readFileSync(path, "utf8")).toContain("via env");
    } finally {
      // afterEach restores.
    }
  });
});