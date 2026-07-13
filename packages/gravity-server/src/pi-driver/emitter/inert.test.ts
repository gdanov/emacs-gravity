// inert.test.ts — Tests for the kill-switch function.

import { describe, it, expect, afterEach } from "vitest";
import {
  isInert,
  GRAVITY_DRIVER_ENV,
  GRAVITY_PI_EMITTER_ENV,
  GRAVITY_PI_EMITTER_OFF,
} from "./inert.js";

/** Snapshot/restore real env vars we touch so test order does not leak. */
const TRACKED = [GRAVITY_DRIVER_ENV, GRAVITY_PI_EMITTER_ENV] as const;
const SAVED: Record<string, string | undefined> = {};

describe("isInert", () => {
  afterEach(() => {
    for (const key of TRACKED) {
      if (SAVED[key] === undefined) {
        delete process.env[key];
      } else {
        process.env[key] = SAVED[key];
      }
    }
  });

  it("returns false when no env vars are set", () => {
    for (const key of TRACKED) {
      SAVED[key] = process.env[key];
      delete process.env[key];
    }
    expect(isInert({})).toBe(false);
  });

  it("returns true when GRAVITY_DRIVER === '1'", () => {
    expect(isInert({ [GRAVITY_DRIVER_ENV]: "1" })).toBe(true);
  });

  it("returns false when GRAVITY_DRIVER is set to anything else", () => {
    expect(isInert({ [GRAVITY_DRIVER_ENV]: "0" })).toBe(false);
    expect(isInert({ [GRAVITY_DRIVER_ENV]: "true" })).toBe(false);
    expect(isInert({ [GRAVITY_DRIVER_ENV]: "" })).toBe(false);
  });

  it("returns true when GRAVITY_PI_EMITTER === 'off'", () => {
    expect(isInert({ [GRAVITY_PI_EMITTER_ENV]: GRAVITY_PI_EMITTER_OFF })).toBe(true);
  });

  it("returns false when GRAVITY_PI_EMITTER is set to anything else", () => {
    expect(isInert({ [GRAVITY_PI_EMITTER_ENV]: "0" })).toBe(false);
    expect(isInert({ [GRAVITY_PI_EMITTER_ENV]: "OFF" })).toBe(false);
    expect(isInert({ [GRAVITY_PI_EMITTER_ENV]: "false" })).toBe(false);
  });

  it("returns true when BOTH kill-switches are set", () => {
    expect(
      isInert({
        [GRAVITY_DRIVER_ENV]: "1",
        [GRAVITY_PI_EMITTER_ENV]: GRAVITY_PI_EMITTER_OFF,
      }),
    ).toBe(true);
  });

  it("reads from process.env when no argument is given", () => {
    SAVED[GRAVITY_DRIVER_ENV] = process.env[GRAVITY_DRIVER_ENV];
    SAVED[GRAVITY_PI_EMITTER_ENV] = process.env[GRAVITY_PI_EMITTER_ENV];
    process.env[GRAVITY_PI_EMITTER_ENV] = GRAVITY_PI_EMITTER_OFF;
    try {
      expect(isInert()).toBe(true);
    } finally {
      // afterEach restores, but restore here too in case test runner order changes
      if (SAVED[GRAVITY_PI_EMITTER_ENV] === undefined) {
        delete process.env[GRAVITY_PI_EMITTER_ENV];
      } else {
        process.env[GRAVITY_PI_EMITTER_ENV] = SAVED[GRAVITY_PI_EMITTER_ENV];
      }
    }
  });
});