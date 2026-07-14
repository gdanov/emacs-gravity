// pi-driver-install.test.ts — Unit tests for `installEmitter`.
//
// Per plan.md §4 WP2 / WP2 §3: install into a fresh temp dir
// (`installed`), call again with identical `source` (`current` and
// assert mtime untouched), call again with different `source`
// (`refreshed`, content updated), and against an unwritable dir
// (`failed`, no thrown exception).
//
// All tests use /tmp-based scratch dirs — never any real ~/.pi/ path.
// Each test uses its own mkdtempSync so concurrent test ordering can
// never interfere.

import { describe, it, expect, afterEach, beforeAll } from "vitest";
import {
  chmodSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { pathToFileURL } from "node:url";

import { installEmitter, DEFAULT_FILENAME } from "../src/pi-driver/install.js";
import { resolveEmitterBundleSource } from "./helpers/emitter-bundle.js";

/** Track every temp dir we create so afterEach can clean them all up. */
const tempDirs: string[] = [];

function freshDir(prefix: string): string {
  const d = mkdtempSync(join(tmpdir(), prefix));
  tempDirs.push(d);
  return d;
}

afterEach(() => {
  while (tempDirs.length > 0) {
    const d = tempDirs.pop();
    if (d === undefined) break;
    // Best-effort cleanup — chmod back to writable first so a test
    // that tightened permissions doesn't break teardown.
    try {
      chmodSync(d, 0o755);
    } catch {
      /* may already be gone */
    }
    try {
      rmSync(d, { recursive: true, force: true });
    } catch {
      /* best effort */
    }
  }
});

describe("installEmitter — `installed` action", () => {
  it("writes a new file when the target does not exist", async () => {
    const dir = freshDir("gravity-install-fresh-");
    const source = "// hello gravity\nconsole.log('hi');\n";
    const result = await installEmitter({ installDir: dir, source });

    expect(result.action).toBe("installed");
    expect(result.path).toBe(join(dir, DEFAULT_FILENAME));
    expect(result.error).toBeUndefined();

    const written = readFileSync(result.path, "utf8");
    expect(written).toBe(source);
  });

  it("creates installDir recursively if it does not exist yet", async () => {
    const parent = freshDir("gravity-install-recursive-");
    const dir = join(parent, "deeply", "nested", "extensions");
    const result = await installEmitter({ installDir: dir, source: "x" });
    expect(result.action).toBe("installed");
    expect(readFileSync(result.path, "utf8")).toBe("x");
  });

  it("honors a custom filename when provided", async () => {
    const dir = freshDir("gravity-install-custom-");
    const result = await installEmitter({
      installDir: dir,
      source: "abc",
      filename: "my-extension.js",
    });
    expect(result.action).toBe("installed");
    expect(result.path).toBe(join(dir, "my-extension.js"));
    expect(readFileSync(result.path, "utf8")).toBe("abc");
  });
});

describe("installEmitter — `current` action (byte-identical no-op)", () => {
  it("returns current without writing when contents already match", async () => {
    const dir = freshDir("gravity-install-current-");
    const source = "// stable bundle contents\n";

    // First call installs.
    const first = await installEmitter({ installDir: dir, source });
    expect(first.action).toBe("installed");
    const firstMtime = statSync(first.path).mtimeMs;
    const firstSize = statSync(first.path).size;
    expect(firstSize).toBe(source.length);

    // Second call with identical source: must be `current`, no temp
    // file residue, mtime unchanged.
    const second = await installEmitter({ installDir: dir, source });
    expect(second.action).toBe("current");
    expect(second.path).toBe(first.path);
    expect(second.error).toBeUndefined();

    const secondMtime = statSync(second.path).mtimeMs;
    expect(secondMtime).toBe(firstMtime);

    // No `.tmp-*` residue left behind.
    // (We can't enumerate dir entries via the fs module directly
    // without using node:fs readdirSync, but the bundle's only file
    // should be the target itself.)
    const { readdirSync } = await import("node:fs");
    const names = readdirSync(dir).sort();
    expect(names).toEqual([DEFAULT_FILENAME]);
  });

  it("`current` is also returned when an existing file has the same bytes", async () => {
    const dir = freshDir("gravity-install-pre-existing-");
    const source = "pre-existing content\n";
    // Pre-create a matching file (simulate a prior install).
    const target = join(dir, DEFAULT_FILENAME);
    writeFileSync(target, source, "utf8");
    const preMtime = statSync(target).mtimeMs;

    const result = await installEmitter({ installDir: dir, source });
    expect(result.action).toBe("current");
    expect(result.path).toBe(target);
    // mtime unchanged
    expect(statSync(target).mtimeMs).toBe(preMtime);
  });
});

describe("installEmitter — `refreshed` action (atomic replace)", () => {
  it("replaces an existing file when content differs", async () => {
    const dir = freshDir("gravity-install-refresh-");
    const oldSource = "// version 1\n";
    const newSource = "// version 2 — totally different bytes\n";

    const first = await installEmitter({ installDir: dir, source: oldSource });
    expect(first.action).toBe("installed");
    expect(readFileSync(first.path, "utf8")).toBe(oldSource);

    // Sleep a few ms to guarantee a measurable mtime difference on
    // filesystems with 1s+ mtime granularity (e.g. some macOS
    // HFS+/APFS configurations, or test sandboxes with coarser
    // timestamps). We do not assert the mtime DELTA — only that the
    // content was updated and there is no temp-file residue.
    await new Promise((r) => setTimeout(r, 25));

    const second = await installEmitter({ installDir: dir, source: newSource });
    expect(second.action).toBe("refreshed");
    expect(second.path).toBe(first.path);
    expect(second.error).toBeUndefined();

    const written = readFileSync(second.path, "utf8");
    expect(written).toBe(newSource);
    expect(written).not.toBe(oldSource);

    // No `.tmp-*` residue left behind after the rename.
    const { readdirSync } = await import("node:fs");
    const names = readdirSync(dir).sort();
    expect(names).toEqual([DEFAULT_FILENAME]);
  });

  it("refreshed when the existing file is the placeholder/empty string", async () => {
    const dir = freshDir("gravity-install-empty-");
    const target = join(dir, DEFAULT_FILENAME);
    writeFileSync(target, "", "utf8");

    const result = await installEmitter({ installDir: dir, source: "x" });
    expect(result.action).toBe("refreshed");
    expect(readFileSync(result.path, "utf8")).toBe("x");
  });
});

describe("installEmitter — `failed` action (no throw)", () => {
  it("returns failed when the installDir path is under an existing file (ENOTDIR)", async () => {
    // Use a regular file as the parent of installDir so mkdir with
    // recursive:true cannot create a directory under it (ENOTDIR).
    const dir = freshDir("gravity-install-enotdir-");
    const blockerPath = join(dir, "blocker");
    writeFileSync(blockerPath, "i am a file, not a directory\n", "utf8");
    const installDir = join(blockerPath, "extensions");

    const result = await installEmitter({ installDir, source: "x" });
    expect(result.action).toBe("failed");
    expect(result.path).toBe(join(installDir, DEFAULT_FILENAME));
    expect(typeof result.error).toBe("string");
    expect(result.error!.length).toBeGreaterThan(0);
  });

  it("returns failed (does not throw) when installDir is an unwritable directory", async () => {
    // POSIX-only chmod path: make the dir read-only and try to
    // install. We restore permissions in afterEach regardless of
    // outcome, so the rmSync there won't choke on 0o500.
    if (process.platform === "win32") {
      // Skip on Windows — chmod semantics are different and the
      // ENOTDIR case above already covers the contract on all
      // platforms.
      return;
    }
    const dir = freshDir("gravity-install-unwritable-");
    chmodSync(dir, 0o500);

    const result = await installEmitter({ installDir: dir, source: "x" });
    expect(result.action).toBe("failed");
    expect(typeof result.error).toBe("string");
    // Restore permissions so the afterEach rmSync can delete the dir.
    chmodSync(dir, 0o755);
  });

  it("returns failed when source write to disk errors out (path under /dev/null)", async () => {
    // `/dev/null/null` cannot be written as a file (mkdir fails with
    // ENOTDIR on Linux/macOS, which the function catches).
    const result = await installEmitter({
      installDir: "/dev/null/null",
      source: "x",
    });
    expect(result.action).toBe("failed");
    expect(typeof result.error).toBe("string");
  });
});

describe("installEmitter — return shape invariants", () => {
  it("returns `path` as `join(installDir, filename)` even on failure", async () => {
    const dir = "/dev/null/null";
    const result = await installEmitter({ installDir: dir, source: "x" });
    expect(result.path).toBe(join(dir, DEFAULT_FILENAME));
  });

  it("never throws, even when source is the empty string", async () => {
    const dir = freshDir("gravity-install-empty-source-");
    const r1 = await installEmitter({ installDir: dir, source: "" });
    expect(r1.action).toBe("installed");
    expect(readFileSync(r1.path, "utf8")).toBe("");

    // Second call with empty source: byte-equal → current.
    const r2 = await installEmitter({ installDir: dir, source: "" });
    expect(r2.action).toBe("current");
  });

  it("never throws, even when source contains only newlines", async () => {
    const dir = freshDir("gravity-install-newlines-");
    const r = await installEmitter({ installDir: dir, source: "\n\n\n" });
    expect(r.action).toBe("installed");
    expect(readFileSync(r.path, "utf8")).toBe("\n\n\n");
  });
});

describe("installEmitter — real built PI_EMITTER_SOURCE", () => {
  let realSource: string;

  beforeAll(async () => {
    realSource = await resolveEmitterBundleSource();
  });

  it("resolves a non-trivial bundle source (>1000 chars)", () => {
    expect(typeof realSource).toBe("string");
    expect(realSource.length).toBeGreaterThan(1000);
  });

  it("writes the bundle byte-identically and it loads as a default-exported factory", async () => {
    const dir = freshDir("gravity-install-real-bundle-");
    const result = await installEmitter({ installDir: dir, source: realSource });
    expect(result.action).toBe("installed");
    expect(result.error).toBeUndefined();

    const written = readFileSync(result.path, "utf8");
    expect(written).toBe(realSource);

    // Dynamic import of the installed file proves the bytes are not
    // just a match but actually loadable, executable JS. Node's ESM
    // loader requires a `file://` URL — `pathToFileURL` provides it
    // portably (the result.path is an OS path that some module-
    // resolution modes refuse as an ESM specifier).
    const mod = (await import(pathToFileURL(result.path).href)) as {
      default?: unknown;
    };
    expect(typeof mod.default).toBe("function");
  });
});

describe("installEmitter — server's own call-path (install→installed, restart→current, stale→refreshed)", () => {
  let serverSource: string;

  beforeAll(async () => {
    // Use the same shape the server passes: PI_EMITTER_SOURCE (the
    // bundled JS) into the configured `piExtensionInstallDir` (which
    // the test injects as a fresh temp dir). Mirrors the call in
    // gravity-server.ts where installEmitter is called with exactly
    // these two arguments.
    serverSource = await resolveEmitterBundleSource();
  });

  it("first call → installed; restart with identical source → current; stale source → refreshed", async () => {
    const dir = freshDir("gravity-install-server-call-");

    // First call → `installed`.
    const first = await installEmitter({
      installDir: dir,
      source: serverSource,
    });
    expect(first.action).toBe("installed");
    expect(first.error).toBeUndefined();
    expect(readFileSync(first.path, "utf8")).toBe(serverSource);

    // Second call with IDENTICAL source → `current`, no filesystem
    // mutation. The bundle source we resolve is canonical so the
    // second call must short-circuit on the byte-equal check.
    const second = await installEmitter({
      installDir: dir,
      source: serverSource,
    });
    expect(second.action).toBe("current");
    expect(second.path).toBe(first.path);
    expect(second.error).toBeUndefined();

    // Third call with a DIFFERENT source → `refreshed`. Appending a
    // comment line is sufficient to force the byte-equal check to
    // fail without changing the bundle's executable semantics.
    const staleSource = `${serverSource}\n// staleness-marker-for-test\n`;
    const third = await installEmitter({
      installDir: dir,
      source: staleSource,
    });
    expect(third.action).toBe("refreshed");
    expect(third.path).toBe(first.path);
    expect(third.error).toBeUndefined();
    expect(readFileSync(third.path, "utf8")).toBe(staleSource);
  });
});