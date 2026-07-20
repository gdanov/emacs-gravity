import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { pathToFileURL, fileURLToPath } from "node:url";
import { Fs, FsTest } from "@gravity/shared";
import {
  removeVersionMarker,
  resolveOwnVersion,
  writeVersionMarker,
} from "../src/gravity-server.js";
import {
  ServerConfig,
  ServerConfigLive,
  ServerConfigTest,
} from "../src/services/config.js";

describe("gravity-server version marker", () => {
  let tempDir = "";

  beforeEach(() => {
    tempDir = mkdtempSync(join(tmpdir(), "gravity-version-marker-"));
  });

  afterEach(() => {
    vi.unstubAllEnvs();
    rmSync(tempDir, { recursive: true, force: true });
  });

  it("resolves the version from the sibling plugin manifest", () => {
    mkdirSync(join(tempDir, "dist"));
    mkdirSync(join(tempDir, ".claude-plugin"));
    writeFileSync(
      join(tempDir, ".claude-plugin", "plugin.json"),
      JSON.stringify({ version: "9.9.9" }),
    );

    const moduleUrl = pathToFileURL(join(tempDir, "dist", "gravity-server.mjs")).href;

    expect(resolveOwnVersion(moduleUrl)).toBe("9.9.9");
  });

  it("falls back to dev when the sibling plugin manifest is absent", () => {
    mkdirSync(join(tempDir, "dist"));

    const moduleUrl = pathToFileURL(join(tempDir, "dist", "gravity-server.mjs")).href;

    expect(resolveOwnVersion(moduleUrl)).toBe("dev");
  });

  it("uses the configured or default version marker path", () => {
    vi.stubEnv("HOME", tempDir);
    vi.stubEnv("GRAVITY_SERVER_VERSION_FILE", undefined);

    const defaultConfig = Effect.runSync(
      Effect.provide(Effect.service(ServerConfig), ServerConfigLive),
    );
    expect(defaultConfig.versionFilePath).toBe(
      join(tempDir, ".local", "state", "gravity-server.version"),
    );

    const configuredPath = join(tempDir, "custom", "server.version");
    vi.stubEnv("GRAVITY_SERVER_VERSION_FILE", configuredPath);
    const configuredConfig = Effect.runSync(
      Effect.provide(Effect.service(ServerConfig), ServerConfigLive),
    );
    expect(configuredConfig.versionFilePath).toBe(configuredPath);

    const testConfig = Effect.runSync(
      Effect.provide(Effect.service(ServerConfig), ServerConfigTest()),
    );
    expect(testConfig.versionFilePath).toBe("/tmp/test-server.version");
  });

  it("writes and removes the marker without requiring sockets", () => {
    const markerPath = join(tempDir, "server.version");
    const writeEffect = Effect.gen(function* () {
      const fs = yield* Effect.service(Fs);
      yield* writeVersionMarker(fs, markerPath, "9.9.9");
      return yield* fs.readFile(markerPath);
    });

    expect(Effect.runSync(Effect.provide(writeEffect, FsTest({})))).toBe("9.9.9");

    writeFileSync(markerPath, "9.9.9");
    removeVersionMarker(markerPath);
    expect(existsSync(markerPath)).toBe(false);
    expect(() => removeVersionMarker(markerPath)).not.toThrow();
  });

  it("writes the marker before the hook server starts listening", () => {
    const sourcePath = join(
      dirname(fileURLToPath(import.meta.url)),
      "..",
      "src",
      "gravity-server.ts",
    );
    const source = readFileSync(sourcePath, "utf-8");
    const markerWriteIndex = source.indexOf("yield* writeVersionMarker(");
    const hookListenIndex = source.indexOf("hookServer.listen(");

    expect(markerWriteIndex).toBeGreaterThanOrEqual(0);
    expect(hookListenIndex).toBeGreaterThanOrEqual(0);
    expect(markerWriteIndex).toBeLessThan(hookListenIndex);
  });
});
