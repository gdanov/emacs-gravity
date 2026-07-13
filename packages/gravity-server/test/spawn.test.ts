// spawn.test.ts — Unit tests for the env-construction helper exported
// from pi-driver/spawn.ts. These tests deliberately do NOT spawn a real
// pi subprocess — they only exercise the pure `buildSpawnEnv` helper,
// which is the minimal piece that needs to be testable in isolation to
// prove the GRAVITY_DRIVER marker is always set on driver-managed pi.

import { describe, it, expect } from "vitest";
import { buildSpawnEnv } from "../src/pi-driver/spawn.js";

describe("buildSpawnEnv", () => {
  it("sets GRAVITY_DRIVER=1 unconditionally when no model or provider is passed", () => {
    const env = buildSpawnEnv({});
    expect(env["GRAVITY_DRIVER"]).toBe("1");
    // The unconditional marker must not depend on whether the caller
    // supplied a model/provider — every spawned pi is driver-managed.
    expect("PI_MODEL" in env).toBe(false);
    expect("PI_PROVIDER" in env).toBe(false);
  });

  it("sets GRAVITY_DRIVER=1 when only a model is passed", () => {
    const env = buildSpawnEnv({ model: "claude-sonnet-4" });
    expect(env["GRAVITY_DRIVER"]).toBe("1");
    expect(env["PI_MODEL"]).toBe("claude-sonnet-4");
    // Provider omitted by caller → must not be defaulted.
    expect("PI_PROVIDER" in env).toBe(false);
  });

  it("sets GRAVITY_DRIVER=1 when only a provider is passed", () => {
    const env = buildSpawnEnv({ provider: "anthropic" });
    expect(env["GRAVITY_DRIVER"]).toBe("1");
    expect(env["PI_PROVIDER"]).toBe("anthropic");
    expect("PI_MODEL" in env).toBe(false);
  });

  it("sets GRAVITY_DRIVER=1 when both model and provider are passed", () => {
    const env = buildSpawnEnv({ model: "claude-sonnet-4", provider: "anthropic" });
    expect(env["GRAVITY_DRIVER"]).toBe("1");
    expect(env["PI_MODEL"]).toBe("claude-sonnet-4");
    expect(env["PI_PROVIDER"]).toBe("anthropic");
  });

  it("inherits the parent process environment so the child sees PATH, HOME, etc.", () => {
    // We cannot pin the contents of process.env from the test (the host
    // varies), but we CAN assert that at least one well-known parent
    // variable is inherited verbatim. PATH is the safest bet — it is
    // virtually always present in any reasonable test runner.
    const env = buildSpawnEnv({});
    expect(env["PATH"]).toBe(process.env["PATH"]);
  });

  it("does not mutate the caller's process.env reference", () => {
    const env = buildSpawnEnv({ model: "claude-sonnet-4" });
    // Returned object must be a fresh record; a write to one key on it
    // must not bleed into the parent's process.env.
    env["MUTATION_PROBE"] = "sentinel";
    expect(process.env["MUTATION_PROBE"]).toBeUndefined();
  });
});