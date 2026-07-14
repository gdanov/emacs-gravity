// index.test.ts — Tests for the default-exported pi extension factory.
//
// Verifies the kill-switch contract: when either GRAVITY_DRIVER=1 or
// GRAVITY_PI_EMITTER=off is set, the factory must register zero
// `pi.on(...)` handlers and return. When neither is set, the factory
// registers the documented set of handlers.

import { describe, it, expect, afterEach, vi } from "vitest";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import gravityPiEmitter from "./index.js";
import {
  GRAVITY_DRIVER_ENV,
  GRAVITY_PI_EMITTER_ENV,
  GRAVITY_PI_EMITTER_OFF,
} from "./inert.js";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

/** Snapshot/restore real env vars so test order cannot leak. */
const TRACKED = [
  GRAVITY_DRIVER_ENV,
  GRAVITY_PI_EMITTER_ENV,
  "GRAVITY_PI_EMITTER_LOG",
] as const;
const SAVED: Record<string, string | undefined> = {};

/** Per-suite scratch dir for the emitter log file. The default
 *  log path resolves to `~/.local/state/gravity-pi-emitter.log`, which
 *  we must NOT touch — point GRAVITY_PI_EMITTER_LOG at a temp file
 *  inside this dir for every test instead. */
const scratchDir = mkdtempSync(join(tmpdir(), "pi-emitter-index-test-"));
const inertLogPath = join(scratchDir, "inert.log");
const activeLogPath = join(scratchDir, "active.log");

describe("gravityPiEmitter", () => {
  afterEach(() => {
    for (const key of TRACKED) {
      if (SAVED[key] === undefined) {
        delete process.env[key];
      } else {
        process.env[key] = SAVED[key];
      }
    }
    // Best-effort cleanup of the log file between tests so each test
    // sees a clean slate. The dir itself is removed at process exit.
    try {
      rmSync(inertLogPath, { force: true });
    } catch {
      /* ignore */
    }
    try {
      rmSync(activeLogPath, { force: true });
    } catch {
      /* ignore */
    }
  });

  it("registers no handlers when GRAVITY_PI_EMITTER === 'off'", () => {
    for (const key of TRACKED) {
      SAVED[key] = process.env[key];
    }
    delete process.env[GRAVITY_DRIVER_ENV];
    process.env[GRAVITY_PI_EMITTER_ENV] = GRAVITY_PI_EMITTER_OFF;
    process.env["GRAVITY_PI_EMITTER_LOG"] = inertLogPath;

    const on = vi.fn();
    const fakePi = { on } as unknown as ExtensionAPI;
    gravityPiEmitter(fakePi);
    expect(on).not.toHaveBeenCalled();
  });

  it("registers no handlers when GRAVITY_DRIVER === '1'", () => {
    for (const key of TRACKED) {
      SAVED[key] = process.env[key];
    }
    process.env[GRAVITY_DRIVER_ENV] = "1";
    delete process.env[GRAVITY_PI_EMITTER_ENV];
    process.env["GRAVITY_PI_EMITTER_LOG"] = inertLogPath;

    const on = vi.fn();
    const fakePi = { on } as unknown as ExtensionAPI;
    gravityPiEmitter(fakePi);
    expect(on).not.toHaveBeenCalled();
  });

  it("registers the documented set of handlers when neither kill-switch is set", () => {
    for (const key of TRACKED) {
      SAVED[key] = process.env[key];
    }
    delete process.env[GRAVITY_DRIVER_ENV];
    delete process.env[GRAVITY_PI_EMITTER_ENV];
    process.env["GRAVITY_PI_EMITTER_LOG"] = activeLogPath;

    const on = vi.fn();
    const fakePi = { on } as unknown as ExtensionAPI;
    gravityPiEmitter(fakePi);
    expect(on).toHaveBeenCalled();
    // Loose lower bound — the exact count grows whenever a new event
    // type is added. Today: 16 (14 event registrations + 2 session
    // bookkeeping handlers in index.ts); 10 is a safe floor that
    // catches a regression that drops most handlers without
    // brittleness against future additions.
    expect(on.mock.calls.length).toBeGreaterThanOrEqual(10);
  });
});

// Best-effort scratch-dir cleanup at process exit so the temp dir
// does not accumulate. The process is short-lived so this is just
// hygiene; the OS will reclaim on its own.
process.on("exit", () => {
  try {
    rmSync(scratchDir, { recursive: true, force: true });
  } catch {
    /* best effort */
  }
});