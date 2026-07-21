// ensure-server-version-marker.test.ts — tests for T5–T9.
//
// T5: unit (shell fn via `sh -c`) for `_gravity_version_gt`,
//     `_gravity_is_semver`, and `_gravity_own_version`.
// T6: integration — gated fast path with a live server. PostToolUse
//     skips the case body entirely (no reads, no spawn). SessionStart
//     with own == marker enters the case body but does not restart.
// T7: integration — stale restart via SessionStart. Old stub killed,
//     new stub spawned via `GRAVITY_SERVER_BIN`, and the OLD fully
//     exits before the NEW socket is bound (unlink-race proof via a
//     watcher that records when the socket disappears vs when the
//     pid file changes).
// T8: integration — absent marker triggers exactly one restart;
//     literal `dev` marker never restarts (gated event).
// T9: integration — two concurrent SessionStart invocations → exactly
//     one restart, marker ends at the newer version.
//
// All tests use per-test temp HOME so the real `~/.local/state/...`
// is never touched. All spawned `sh`/`node` children are bounded and
// force-killed in `afterEach`. The fake-`$0` trick is the documented
// way to drive `_ensure-server` from a vitest test as if a specific
// hook event had fired.

import { afterEach, describe, expect, it } from "vitest";
import { spawn, spawnSync, type ChildProcess } from "node:child_process";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { createServer, type Server } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";

const BRIDGE_ROOT = join(__dirname, "..");
const HOOKS_SRC_DIR = join(BRIDGE_ROOT, "hooks");
const STUB_PATH = join(BRIDGE_ROOT, "test", "fixtures", "stub-gravity-server.mjs");

/** Generous bound for spawned children; a regression that hangs the
 *  production shell (e.g. infinite lock-wait) fails the test fast. */
const CHILD_TIMEOUT_MS = 10_000;

// ─── Per-test temp dirs ──────────────────────────────────────────────────

const tempDirs: string[] = [];
function freshTempDir(prefix: string): string {
  const d = mkdtempSync(join(tmpdir(), prefix));
  tempDirs.push(d);
  return d;
}

afterEach(() => {
  for (const d of tempDirs.splice(0)) {
    try {
      rmSync(d, { recursive: true, force: true });
    } catch {
      /* ignore */
    }
  }
});

// ─── Spawn / lifecycle helpers ───────────────────────────────────────────

interface SpawnedProcess {
  child: ChildProcess;
  killChild: () => void;
  awaitExit: (
    timeoutMs?: number,
  ) => Promise<{ code: number | null; signal: NodeJS.Signals | null }>;
}

const liveChildren: SpawnedProcess[] = [];

function trackedSpawn(
  command: string,
  args: string[],
  opts: Parameters<typeof spawn>[2],
): SpawnedProcess {
  const child = spawn(command, args, opts);
  const handle: SpawnedProcess = {
    child,
    killChild: () => {
      if (!child.killed && child.exitCode === null) {
        try {
          child.kill("SIGKILL");
        } catch {
          /* ignore */
        }
      }
    },
    awaitExit: (timeoutMs = CHILD_TIMEOUT_MS) =>
      new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          handle.killChild();
          reject(
            new Error(
              `child ${command} ${args.join(" ")} did not exit within ${timeoutMs} ms (SIGKILL'd)`,
            ),
          );
        }, timeoutMs);
        timer.unref();
        child.once("exit", (code, signal) => {
          clearTimeout(timer);
          resolve({ code, signal });
        });
      }),
  };
  liveChildren.push(handle);
  return handle;
}

afterEach(() => {
  // Force-kill any straggler children before removing their temp dirs.
  for (const h of liveChildren.splice(0)) {
    h.killChild();
  }
});

// ─── Minimal env (no `process.env` inheritance) ─────────────────────────

/** Minimal env for an `_ensure-server` child. All three GRAVITY_*
 *  state paths are passed explicitly so the script's default-formula
 *  fallback (`$HOME/.local/state/...`) does NOT shadow our temp paths
 *  and accidentally fall through to the production server. */
function buildChildEnv(
  homeDir: string,
  paths: { sockPath: string; pidFile: string; versionFile: string },
  extra: Record<string, string> = {},
): Record<string, string> {
  return {
    PATH: process.env.PATH ?? "/usr/local/bin:/usr/bin:/bin",
    HOME: homeDir,
    GRAVITY_HOOK_SOCK: paths.sockPath,
    GRAVITY_PID_FILE: paths.pidFile,
    GRAVITY_SERVER_VERSION_FILE: paths.versionFile,
    ...extra,
  };
}

// ─── C1 — spawn-count log (restart-spawns only) ─────────────────────────

/** Number of restart-spawned stubs recorded in `spawnLog` (one line per
 *  stub that had `STUB_SPAWN_LOG` set at startup). 0 if the file was
 *  never created. */
function countSpawnLog(spawnLog: string): number {
  if (!existsSync(spawnLog)) return 0;
  return readFileSync(spawnLog, "utf-8").split("\n").filter((l) => l.trim() !== "")
    .length;
}

// ─── C2 — read-trace bindir (proves the hot path reads neither
//          plugin.json nor the marker) ──────────────────────────────────

/** Resolve the absolute path of a real external tool via `command -v`
 *  in a fresh `sh`, so the wrapper we write can `exec` the genuine
 *  binary after recording its own invocation. */
function resolveRealTool(name: string): string {
  const r = spawnSync("sh", ["-c", `command -v ${name}`], {
    env: { PATH: process.env.PATH ?? "/usr/local/bin:/usr/bin:/bin" },
    encoding: "utf-8",
  });
  const resolved = (r.stdout ?? "").trim();
  if (!resolved) throw new Error(`could not resolve real "${name}" via command -v`);
  return resolved;
}

interface ReadTraceBindir {
  binDir: string;
  traceFile: string;
}

/** Creates a per-test bindir with `cat`/`sed` wrapper scripts that
 *  append `"<tool> <args>\n"` to `$GRAVITY_READ_TRACE` then `exec` the
 *  real tool. Prepend `binDir` to the child's PATH and set
 *  `GRAVITY_READ_TRACE=traceFile` to wire it in. Other commands
 *  (`head`, `node`, `sh`, `rm`, `kill`, ...) are left unwrapped and
 *  resolve normally from the inherited PATH. */
function setupReadTraceBindir(): ReadTraceBindir {
  const binDir = freshTempDir("wp2-readtrace-bin-");
  const traceDir = freshTempDir("wp2-readtrace-log-");
  const traceFile = join(traceDir, "trace.log");

  for (const tool of ["cat", "sed"]) {
    const realTool = resolveRealTool(tool);
    const body = `#!/bin/sh\nprintf '%s\\n' "${tool} $*" >> "$GRAVITY_READ_TRACE"\nexec ${realTool} "$@"\n`;
    writeFileSync(join(binDir, tool), body, { mode: 0o755 });
  }

  return { binDir, traceFile };
}

// ─── C3 — setsid availability (for the T10 regression test skip guard) ──

const hasSetsid: boolean =
  spawnSync("sh", ["-c", "command -v setsid"], {
    env: { PATH: process.env.PATH ?? "/usr/local/bin:/usr/bin:/bin" },
  }).status === 0;

// ─── Per-test plugin (hooks dir + sibling .claude-plugin/plugin.json) ────

interface TestHooks {
  hooksDir: string;
  rootDir: string;
}

function setupTestHooksDir(opts: {
  /** `undefined` → omit plugin.json entirely (own version = "dev" via fallback). */
  ownVersion?: string;
}): TestHooks {
  const rootDir = freshTempDir("wp2-plugin-");
  const hooksDir = join(rootDir, "hooks");
  mkdirSync(hooksDir, { recursive: true });
  copyFileSync(
    join(HOOKS_SRC_DIR, "_ensure-server"),
    join(hooksDir, "_ensure-server"),
  );
  copyFileSync(
    join(HOOKS_SRC_DIR, "_version-compare"),
    join(hooksDir, "_version-compare"),
  );
  copyFileSync(
    join(HOOKS_SRC_DIR, "_spawn-server"),
    join(hooksDir, "_spawn-server"),
  );
  if (opts.ownVersion !== undefined) {
    const pluginDir = join(rootDir, ".claude-plugin");
    mkdirSync(pluginDir, { recursive: true });
    writeFileSync(
      join(pluginDir, "plugin.json"),
      JSON.stringify({
        name: "emacs-bridge",
        version: opts.ownVersion,
      }),
    );
  }
  return { hooksDir, rootDir };
}

// ─── Real Unix socket ────────────────────────────────────────────────────

interface BoundSocketHandle {
  server: Server;
  sockPath: string;
  stop: () => Promise<void>;
}

function bindUnixSocket(sockPath: string): Promise<BoundSocketHandle> {
  return new Promise((promResolve, promReject) => {
    const server = createServer(() => {});
    server.on("error", promReject);
    server.listen(sockPath, () => {
      promResolve({
        server,
        sockPath,
        stop: () =>
          new Promise<void>((res) => {
            server.close(() => res());
          }),
      });
    });
  });
}

// ─── Stub gravity server ────────────────────────────────────────────────

interface StubHandle {
  pid: number;
  sockPath: string;
  pidFile: string;
  versionFile: string;
  child: ChildProcess;
}

/** Copy the canonical stub script to a per-test temp path with a name
 *  that does NOT contain the substring `gravity-server.mjs` /
 *  `gravity-server.ts`. The production helper invokes
 *  `pkill -f "gravity-server\\.(mjs|ts)"` for best-effort orphan
 *  cleanup; that pattern matches any process whose argv contains
 *  `gravity-server.mjs`/`gravity-server.ts` — including the parent
 *  shell that's running `_spawn-server` itself, because the shell's
 *  argv contains the `GRAVITY_SERVER_BIN` path. By giving the stub a
 *  harmless filename (e.g. `daemon.mjs`) we make that pkill a true
 *  no-op during the test. The stub's behavior is unchanged — only the
 *  path string differs. */
function aliasStubAwayFromGravityServerName(): string {
  const stubDir = freshTempDir("wp2-stub-");
  const aliasPath = join(stubDir, "daemon.mjs");
  copyFileSync(STUB_PATH, aliasPath);
  return aliasPath;
}

async function startStubServer(opts: {
  version: string;
  sockPath: string;
  pidFile: string;
  versionFile: string;
  /** Override the binary path. Defaults to STUB_PATH. Pass
   *  `aliasStubAwayFromGravityServerName()` when the stub is also the
   *  restart target so `pkill` in `_spawn-server` doesn't match the
   *  parent shell's argv. */
  binPath?: string;
}): Promise<StubHandle> {
  const binPath = opts.binPath ?? STUB_PATH;
  const env: Record<string, string> = {
    PATH: process.env.PATH ?? "/usr/local/bin:/usr/bin:/bin",
    HOME: "/tmp", // stub doesn't read HOME
    STUB_SERVER_VERSION: opts.version,
    GRAVITY_HOOK_SOCK: opts.sockPath,
    GRAVITY_PID_FILE: opts.pidFile,
    GRAVITY_SERVER_VERSION_FILE: opts.versionFile,
  };
  const proc = trackedSpawn("node", [binPath], {
    env,
    stdio: ["ignore", "pipe", "pipe"],
  });
  // Wait for stub to bind its socket AND write its pid file.
  const deadline = Date.now() + 5000;
  while (Date.now() < deadline) {
    if (existsSync(opts.pidFile) && existsSync(opts.sockPath)) {
      const text = readFileSync(opts.pidFile, "utf-8").trim();
      const pid = parseInt(text, 10);
      if (!isNaN(pid) && pid > 0) {
        return {
          pid,
          sockPath: opts.sockPath,
          pidFile: opts.pidFile,
          versionFile: opts.versionFile,
          child: proc.child,
        };
      }
    }
    await new Promise((r) => setTimeout(r, 5));
  }
  throw new Error(
    `stub did not bind socket + write pid file within 5s ` +
      `(sock=${existsSync(opts.sockPath)}, pid=${existsSync(opts.pidFile)})`,
  );
}

// ─── Fake-$0 _ensure-server invocation ──────────────────────────────────

interface EnsureServerResult {
  exitInfo: { code: number | null; signal: NodeJS.Signals | null };
  stdout: string;
  stderr: string;
}

async function runEnsureServer(opts: {
  hooksDir: string;
  eventName: string;
  env: Record<string, string>;
}): Promise<EnsureServerResult> {
  const fakeArg0 = join(opts.hooksDir, opts.eventName);
  const proc = trackedSpawn(
    "sh",
    ["-c", '. "$(dirname "$0")/_ensure-server"', fakeArg0],
    { env: opts.env },
  );
  const stdoutBuf: Buffer[] = [];
  const stderrBuf: Buffer[] = [];
  proc.child.stdout?.on("data", (c: Buffer) => stdoutBuf.push(c));
  proc.child.stderr?.on("data", (c: Buffer) => stderrBuf.push(c));
  const exitInfo = await proc.awaitExit(CHILD_TIMEOUT_MS);
  return {
    exitInfo,
    stdout: Buffer.concat(stdoutBuf).toString(),
    stderr: Buffer.concat(stderrBuf).toString(),
  };
}

// ─── Unlink-race watcher (T7) ────────────────────────────────────────────

interface OrderingEvents {
  socketGoneAt: number;
  pidChangedAt: number;
  newPid: number;
}

/** Watch the socket + pid file and record (a) when the socket file
 *  first disappears (old stub's SIGTERM-shutdown unlink fired) and
 *  (b) when the pid file first changes to a different non-empty pid
 *  (new stub's listen-callback pid-write fired). The new stub writes
 *  its pid file AFTER binding its socket (per stub design), so the
 *  pid-file change strictly follows the old socket unlink. */
async function watchSocketLifecycle(opts: {
  sockPath: string;
  pidFile: string;
  oldPid: number;
  timeoutMs?: number;
}): Promise<OrderingEvents> {
  const start = Date.now();
  const deadline = start + (opts.timeoutMs ?? 5000);
  let socketGoneAt = -1;
  let pidChangedAt = -1;
  let newPid = 0;
  let lastPid = String(opts.oldPid);
  let socketSeen = existsSync(opts.sockPath);

  while (Date.now() < deadline) {
    const sockExists = existsSync(opts.sockPath);
    if (socketSeen && !sockExists) {
      socketGoneAt = Date.now();
      socketSeen = false;
    }
    if (existsSync(opts.pidFile)) {
      const cur = readFileSync(opts.pidFile, "utf-8").trim();
      if (cur !== lastPid && cur !== "") {
        const parsed = parseInt(cur, 10);
        if (!isNaN(parsed) && parsed > 0) {
          pidChangedAt = Date.now();
          newPid = parsed;
          lastPid = cur;
        }
      }
    }
    if (socketGoneAt > 0 && pidChangedAt > 0) break;
    // Tight polling — old stub waits 50ms before unlinking, so we have
    // plenty of headroom, but a missing event would manifest as a
    // timeout, which is still an actionable failure.
    await new Promise((r) => setTimeout(r, 1));
  }
  if (socketGoneAt < 0 || pidChangedAt < 0) {
    throw new Error(
      `watchSocketLifecycle timed out after ${(Date.now() - start)}ms ` +
        `(socketGoneAt=${socketGoneAt}, pidChangedAt=${pidChangedAt}, ` +
        `lastPid=${lastPid}, socketSeen=${socketSeen})`,
    );
  }
  return { socketGoneAt, pidChangedAt, newPid };
}

// ─── T5 — semver compare unit (shell fn via `sh -c`) ─────────────────────

describe("T5: _version-compare helpers", () => {
  // We drive the helpers via the same `sh -c SCRIPT -- hooksDir ...`
  // pattern the plan documents. `--` makes `$0 = "--"`, `$1 = hooksDir`,
  // `$2/$3/...` = positional args (cleaner than relying on shell
  // variable naming conventions).

  function runCompare(
    script: string,
    hooksDir: string,
    ...args: string[]
  ): Promise<{ stdout: string; stderr: string; code: number | null }> {
    return new Promise((resolve, reject) => {
      const proc = trackedSpawn(
        "sh",
        ["-c", script, "--", hooksDir, ...args],
        { env: { PATH: process.env.PATH ?? "/usr/local/bin:/usr/bin:/bin" } },
      );
      const stdoutBuf: Buffer[] = [];
      const stderrBuf: Buffer[] = [];
      proc.child.stdout?.on("data", (c: Buffer) => stdoutBuf.push(c));
      proc.child.stderr?.on("data", (c: Buffer) => stderrBuf.push(c));
      proc.awaitExit(CHILD_TIMEOUT_MS)
        .then((info) =>
          resolve({
            stdout: Buffer.concat(stdoutBuf).toString().trim(),
            stderr: Buffer.concat(stderrBuf).toString(),
            code: info.code,
          }),
        )
        .catch(reject);
    });
  }

  it("_gravity_version_gt: 4.5.2 > 4.5.1 → gt", async () => {
    const r = await runCompare(
      '. "$1/_version-compare"; _gravity_version_gt "$2" "$3" && echo gt || echo not-gt',
      HOOKS_SRC_DIR,
      "4.5.2",
      "4.5.1",
    );
    expect(r.stdout).toBe("gt");
  });

  it("_gravity_version_gt: 4.5.1 > 4.5.2 → not-gt", async () => {
    const r = await runCompare(
      '. "$1/_version-compare"; _gravity_version_gt "$2" "$3" && echo gt || echo not-gt',
      HOOKS_SRC_DIR,
      "4.5.1",
      "4.5.2",
    );
    expect(r.stdout).toBe("not-gt");
  });

  it("_gravity_version_gt: 4.5.2 > 4.5.2 → not-gt (equal is not strictly greater)", async () => {
    const r = await runCompare(
      '. "$1/_version-compare"; _gravity_version_gt "$2" "$3" && echo gt || echo not-gt',
      HOOKS_SRC_DIR,
      "4.5.2",
      "4.5.2",
    );
    expect(r.stdout).toBe("not-gt");
  });

  it("_gravity_version_gt: 4.10.0 > 4.9.0 → gt (multi-digit components)", async () => {
    const r = await runCompare(
      '. "$1/_version-compare"; _gravity_version_gt "$2" "$3" && echo gt || echo not-gt',
      HOOKS_SRC_DIR,
      "4.10.0",
      "4.9.0",
    );
    expect(r.stdout).toBe("gt");
  });

  it("_gravity_version_gt: 1.0.0 > 99.99.99 → not-gt (sanity, larger major wins)", async () => {
    const r = await runCompare(
      '. "$1/_version-compare"; _gravity_version_gt "$2" "$3" && echo gt || echo not-gt',
      HOOKS_SRC_DIR,
      "1.0.0",
      "99.99.99",
    );
    expect(r.stdout).toBe("not-gt");
  });

  it("_gravity_is_semver: 4.5.2 → true", async () => {
    const r = await runCompare(
      '. "$1/_version-compare"; _gravity_is_semver "$2" && echo true || echo false',
      HOOKS_SRC_DIR,
      "4.5.2",
    );
    expect(r.stdout).toBe("true");
  });

  it("_gravity_is_semver: dev → false", async () => {
    const r = await runCompare(
      '. "$1/_version-compare"; _gravity_is_semver "$2" && echo true || echo false',
      HOOKS_SRC_DIR,
      "dev",
    );
    expect(r.stdout).toBe("false");
  });

  it("_gravity_is_semver: 4.5 (incomplete) → false", async () => {
    const r = await runCompare(
      '. "$1/_version-compare"; _gravity_is_semver "$2" && echo true || echo false',
      HOOKS_SRC_DIR,
      "4.5",
    );
    expect(r.stdout).toBe("false");
  });

  it("_gravity_own_version: reads sibling plugin.json when present", async () => {
    const hooks = setupTestHooksDir({ ownVersion: "9.9.9" });
    const r = await runCompare(
      '. "$1/_version-compare"; _gravity_own_version "$1"',
      hooks.hooksDir,
    );
    expect(r.stdout).toBe("9.9.9");
  });

  it("_gravity_own_version: returns 'dev' when plugin.json is absent", async () => {
    const hooks = setupTestHooksDir({}); // no ownVersion → no plugin.json
    const r = await runCompare(
      '. "$1/_version-compare"; _gravity_own_version "$1"',
      hooks.hooksDir,
    );
    expect(r.stdout).toBe("dev");
  });
});

// ─── T6 — gated fast path: PostToolUse skips case body, no spawn ────────

describe("T6: gated fast path", () => {
  let stub: StubHandle | undefined;
  let boundSock: BoundSocketHandle | undefined;

  afterEach(async () => {
    // Stub SIGTERM so it cleans up its own socket + marker (matches
    // production-like teardown).
    if (stub && stub.child.exitCode === null) {
      try {
        stub.child.kill("SIGTERM");
      } catch {
        /* ignore */
      }
      // give stub ~200ms to unlink socket + exit (it waits 50ms then
      // exits synchronously; this is a safety margin).
      await new Promise((r) => setTimeout(r, 200));
    }
    if (boundSock) await boundSock.stop();
  });

  it("PostToolUse: case body NOT entered — no spawn, no reads of plugin.json/marker", async () => {
    const home = freshTempDir("wp2-t6a-home-");
    const sockPath = join(home, "gravity-hooks.sock");
    const pidFile = join(home, "gravity-server.pid");
    const versionFile = join(home, "gravity-server.version");
    const spawnLog = join(home, "spawn.log");

    // Pre-launch OLD stub: marker "4.5.1".
    stub = await startStubServer({
      version: "4.5.1",
      sockPath,
      pidFile,
      versionFile,
    });

    // Snapshot marker content so we can assert it was never re-read or
    // re-written by a hot-path that shouldn't touch it.
    const markerContentBefore = readFileSync(versionFile, "utf-8");
    expect(markerContentBefore).toBe("4.5.1");

    const { binDir, traceFile } = setupReadTraceBindir();

    // Use the REAL production hooks dir but with a sibling
    // .claude-plugin/plugin.json set to "4.5.2" — the gated branch
    // would consider this stale IF entered, but PostToolUse skips the
    // case body entirely, so no reads happen.
    const realPluginDir = join(HOOKS_SRC_DIR, "..", ".claude-plugin");
    const realPluginJson = join(realPluginDir, "plugin.json");
    const realPluginJsonBackup = `${realPluginJson}.t6-backup`;
    copyFileSync(realPluginJson, realPluginJsonBackup);
    let restored = false;
    const restorePluginJson = () => {
      if (restored) return;
      restored = true;
      try {
        copyFileSync(realPluginJsonBackup, realPluginJson);
        rmSync(realPluginJsonBackup, { force: true });
      } catch {
        /* ignore */
      }
    };

    try {
      // Set own version to "4.5.2" (would be stale if reached).
      writeFileSync(
        realPluginJson,
        JSON.stringify({
          name: "emacs-bridge",
          version: "4.5.2",
        }),
      );

      const env = buildChildEnv(home, { sockPath, pidFile, versionFile }, {
        PATH: `${binDir}:${process.env.PATH ?? "/usr/local/bin:/usr/bin:/bin"}`,
        GRAVITY_READ_TRACE: traceFile,
        STUB_SPAWN_LOG: spawnLog,
      });
      const result = await runEnsureServer({
        hooksDir: HOOKS_SRC_DIR,
        eventName: "PostToolUse",
        env,
      });

      expect(result.exitInfo.code).toBe(0);

      // Marker file unchanged.
      expect(readFileSync(versionFile, "utf-8")).toBe("4.5.1");

      // C2: the wrappers were genuinely on PATH and exercised (the hot
      // path always `cat`s the pid file) — but never read the marker
      // or plugin.json.
      const trace = existsSync(traceFile) ? readFileSync(traceFile, "utf-8") : "";
      expect(trace).toContain(pidFile);
      expect(trace).not.toContain(versionFile);
      expect(trace).not.toContain("plugin.json");

      // C1: no restart-spawn happened.
      expect(countSpawnLog(spawnLog)).toBe(0);

      // Pid file still points at the OLD stub.
      const pidNow = readFileSync(pidFile, "utf-8").trim();
      expect(parseInt(pidNow, 10)).toBe(stub.pid);

      // OLD stub still alive (kill -0 succeeds).
      // (Process.kill with signal 0 returns true if alive.)
      expect(() => process.kill(stub.pid, 0)).not.toThrow();

      // Socket still bound (by OLD stub).
      expect(existsSync(sockPath)).toBe(true);
    } finally {
      restorePluginJson();
    }
  });

  it("SessionStart with own == marker: case body entered, no restart", async () => {
    const home = freshTempDir("wp2-t6b-home-");
    const sockPath = join(home, "gravity-hooks.sock");
    const pidFile = join(home, "gravity-server.pid");
    const versionFile = join(home, "gravity-server.version");
    const spawnLog = join(home, "spawn.log");

    // Pre-launch OLD stub: marker "4.5.1". Own will be "4.5.1" too —
    // case body enters, computes own > marker → 1>1 → false, so
    // restart_needed stays 0. No spawn.
    stub = await startStubServer({
      version: "4.5.1",
      sockPath,
      pidFile,
      versionFile,
    });

    const hooks = setupTestHooksDir({ ownVersion: "4.5.1" });
    const { binDir, traceFile } = setupReadTraceBindir();

    const env = buildChildEnv(home, { sockPath, pidFile, versionFile }, {
      PATH: `${binDir}:${process.env.PATH ?? "/usr/local/bin:/usr/bin:/bin"}`,
      GRAVITY_READ_TRACE: traceFile,
      STUB_SPAWN_LOG: spawnLog,
    });
    const result = await runEnsureServer({
      hooksDir: hooks.hooksDir,
      eventName: "SessionStart",
      env,
    });

    expect(result.exitInfo.code).toBe(0);

    // Pid file still OLD.
    const pidNow = readFileSync(pidFile, "utf-8").trim();
    expect(parseInt(pidNow, 10)).toBe(stub.pid);

    // Marker unchanged.
    expect(readFileSync(versionFile, "utf-8")).toBe("4.5.1");

    // OLD stub still alive.
    expect(() => process.kill(stub.pid, 0)).not.toThrow();

    // Socket still bound.
    expect(existsSync(sockPath)).toBe(true);

    // C2 positive control: the case body WAS entered — trace shows
    // both the marker read (`cat`) and the plugin.json read (`sed`).
    // Proves the read-trace mechanism is not vacuously green.
    const trace = existsSync(traceFile) ? readFileSync(traceFile, "utf-8") : "";
    expect(trace).toContain(versionFile);
    expect(trace).toContain("plugin.json");

    // C1: entered the case body, but own == marker → no restart.
    expect(countSpawnLog(spawnLog)).toBe(0);
  });
});

// ─── T7 — stale restart: old killed, new spawned, ordering preserved ────

describe("T7: stale restart", () => {
  let stub: StubHandle | undefined;

  afterEach(async () => {
    if (stub && stub.child.exitCode === null) {
      try {
        stub.child.kill("SIGTERM");
      } catch {
        /* ignore */
      }
      await new Promise((r) => setTimeout(r, 200));
    }
  });

  it("SessionStart with own > marker: old exits, new binds, ordering preserved", async () => {
    const home = freshTempDir("wp2-t7-home-");
    const sockPath = join(home, "gravity-hooks.sock");
    const pidFile = join(home, "gravity-server.pid");
    const versionFile = join(home, "gravity-server.version");

    // Use an alias path that does NOT contain the substring
    // `gravity-server.mjs` — see `aliasStubAwayFromGravityServerName`
    // for the rationale (pkill in `_spawn-server` would otherwise
    // match the parent shell's argv and kill the helper mid-flight).
    const stubBin = aliasStubAwayFromGravityServerName();

    // Pre-launch OLD stub with version "4.5.1".
    stub = await startStubServer({
      version: "4.5.1",
      sockPath,
      pidFile,
      versionFile,
      binPath: stubBin,
    });
    const oldPid = stub.pid;
    expect(readFileSync(versionFile, "utf-8")).toBe("4.5.1");
    const spawnLog = join(home, "spawn.log");

    // Per-test plugin layout: hooks dir + sibling .claude-plugin with
    // own version "4.5.2" (strictly newer than marker).
    const hooks = setupTestHooksDir({ ownVersion: "4.5.2" });

    // Set GRAVITY_SERVER_BIN to the same alias — the NEW stub writes
    // the newer marker (4.5.2) inherited via `STUB_SERVER_VERSION`.
    const env = buildChildEnv(
      home,
      { sockPath, pidFile, versionFile },
      {
        GRAVITY_SERVER_BIN: stubBin,
        STUB_SERVER_VERSION: "4.5.2",
        STUB_SPAWN_LOG: spawnLog,
      },
    );

    // Watcher must be running BEFORE we invoke _ensure-server, so
    // we don't miss the OLD socket's disappearance.
    const watcherPromise = watchSocketLifecycle({
      sockPath,
      pidFile,
      oldPid,
    });

    const result = await runEnsureServer({
      hooksDir: hooks.hooksDir,
      eventName: "SessionStart",
      env,
    });

    expect(result.exitInfo.code).toBe(0);
    expect(result.stderr).toBe(""); // no unexpected output

    const ordering = await watcherPromise;
    expect(ordering.newPid).not.toBe(oldPid);
    expect(ordering.newPid).toBeGreaterThan(0);
    // Unlink-race proof: old socket must have been unlinked BEFORE
    // the new stub wrote its pid file.
    expect(ordering.socketGoneAt).toBeGreaterThan(0);
    expect(ordering.pidChangedAt).toBeGreaterThan(ordering.socketGoneAt);

    // Old stub is dead.
    expect(() => process.kill(oldPid, 0)).toThrow();

    // C1: exactly one restart-spawn.
    expect(countSpawnLog(spawnLog)).toBe(1);

    // New pid is alive.
    expect(() => process.kill(ordering.newPid, 0)).not.toThrow();

    // Marker now reads the NEW version.
    expect(readFileSync(versionFile, "utf-8")).toBe("4.5.2");

    // Pid file contains the NEW pid.
    expect(parseInt(readFileSync(pidFile, "utf-8").trim(), 10)).toBe(
      ordering.newPid,
    );

    // Socket still exists (rebound by the new stub).
    expect(existsSync(sockPath)).toBe(true);

    // Stop tracking OLD stub (it's already dead — guarded by exitCheck).
    stub = undefined;
  });
});

// ─── T8 — absent marker triggers exactly one restart; dev marker does not ──

describe("T8: absent marker vs dev marker", () => {
  let stub: StubHandle | undefined;
  let boundSock: BoundSocketHandle | undefined;

  afterEach(async () => {
    if (stub && stub.child.exitCode === null) {
      try {
        stub.child.kill("SIGTERM");
      } catch {
        /* ignore */
      }
      await new Promise((r) => setTimeout(r, 200));
    }
    if (boundSock) await boundSock.stop();
  });

  it("SessionStart + absent marker: exactly one restart happens", async () => {
    const home = freshTempDir("wp2-t8a-home-");
    const sockPath = join(home, "gravity-hooks.sock");
    const pidFile = join(home, "gravity-server.pid");
    const versionFile = join(home, "gravity-server.version");

    // We need the script to enter the gated branch (socket + pid
    // present) but with marker ABSENT. Easiest path: bind the socket
    // ourselves and write a real pid file pointing at a sleeper
    // process — no marker file is created at all.
    boundSock = await bindUnixSocket(sockPath);
    const sleeper = trackedSpawn("sh", ["-c", "sleep 60"], {
      env: { PATH: process.env.PATH ?? "/usr/local/bin:/usr/bin:/bin" },
    });
    const sleeperPid = sleeper.child.pid ?? 0;
    expect(sleeperPid).toBeGreaterThan(0);
    writeFileSync(pidFile, String(sleeperPid));
    // Confirm sleeper is alive.
    expect(() => process.kill(sleeperPid, 0)).not.toThrow();
    expect(existsSync(versionFile)).toBe(false);

    const hooks = setupTestHooksDir({ ownVersion: "4.5.2" });
    // Use the alias path so `pkill` in `_spawn-server` doesn't match
    // its own parent shell's argv.
    const stubBin = aliasStubAwayFromGravityServerName();
    const spawnLog = join(home, "spawn.log");
    const env = buildChildEnv(
      home,
      { sockPath, pidFile, versionFile },
      {
        GRAVITY_SERVER_BIN: stubBin,
        STUB_SERVER_VERSION: "4.5.2",
        STUB_SPAWN_LOG: spawnLog,
      },
    );

    const result = await runEnsureServer({
      hooksDir: hooks.hooksDir,
      eventName: "SessionStart",
      env,
    });
    expect(result.exitInfo.code).toBe(0);

    // Sleeper should be dead (killed during restart).
    expect(() => process.kill(sleeperPid, 0)).toThrow();

    // NEW marker exists with own version.
    expect(readFileSync(versionFile, "utf-8")).toBe("4.5.2");

    // NEW pid is alive and != sleeperPid.
    const newPidText = readFileSync(pidFile, "utf-8").trim();
    const newPid = parseInt(newPidText, 10);
    expect(newPid).not.toBe(sleeperPid);
    expect(newPid).toBeGreaterThan(0);
    expect(() => process.kill(newPid, 0)).not.toThrow();

    // F4/C1: exactly one restart-spawn.
    expect(countSpawnLog(spawnLog)).toBe(1);
  });

  it("SessionStart + dev marker: NO restart, old still alive", async () => {
    const home = freshTempDir("wp2-t8b-home-");
    const sockPath = join(home, "gravity-hooks.sock");
    const pidFile = join(home, "gravity-server.pid");
    const versionFile = join(home, "gravity-server.version");

    // Pre-launch stub with STUB_SERVER_VERSION=dev → writes "dev" to
    // marker. Own version (from per-test plugin.json) is "4.5.2",
    // strictly newer than marker, BUT marker is non-semver so
    // restart_needed stays 0. No restart.
    stub = await startStubServer({
      version: "dev",
      sockPath,
      pidFile,
      versionFile,
    });
    expect(readFileSync(versionFile, "utf-8")).toBe("dev");

    const hooks = setupTestHooksDir({ ownVersion: "4.5.2" });
    const spawnLog = join(home, "spawn.log");
    const env = buildChildEnv(home, { sockPath, pidFile, versionFile }, {
      STUB_SPAWN_LOG: spawnLog,
    });

    const result = await runEnsureServer({
      hooksDir: hooks.hooksDir,
      eventName: "SessionStart",
      env,
    });
    expect(result.exitInfo.code).toBe(0);

    // OLD stub still alive.
    expect(() => process.kill(stub.pid, 0)).not.toThrow();

    // Marker still reads "dev".
    expect(readFileSync(versionFile, "utf-8")).toBe("dev");

    // Pid file unchanged (still OLD).
    expect(parseInt(readFileSync(pidFile, "utf-8").trim(), 10)).toBe(stub.pid);

    // Socket still bound.
    expect(existsSync(sockPath)).toBe(true);

    // C1: no restart-spawn.
    expect(countSpawnLog(spawnLog)).toBe(0);
  });
});

// ─── T9 — two concurrent staleness detectors → exactly one restart ──────

describe("T9: lock serialization under concurrent detectors", () => {
  let stub: StubHandle | undefined;

  afterEach(async () => {
    if (stub && stub.child.exitCode === null) {
      try {
        stub.child.kill("SIGTERM");
      } catch {
        /* ignore */
      }
      await new Promise((r) => setTimeout(r, 200));
    }
  });

  it("two concurrent SessionStart invocations produce exactly one restart", async () => {
    const home = freshTempDir("wp2-t9-home-");
    const sockPath = join(home, "gravity-hooks.sock");
    const pidFile = join(home, "gravity-server.pid");
    const versionFile = join(home, "gravity-server.version");

    // Pre-launch OLD stub.
    stub = await startStubServer({
      version: "4.5.1",
      sockPath,
      pidFile,
      versionFile,
    });
    const oldPid = stub.pid;

    const hooks = setupTestHooksDir({ ownVersion: "4.5.2" });
    // Reuse the OLD stub's alias path (already safe by construction —
    // no `gravity-server.mjs` substring) for GRAVITY_SERVER_BIN.
    const stubBin = aliasStubAwayFromGravityServerName();
    const spawnLog = join(home, "spawn.log");
    const env = buildChildEnv(
      home,
      { sockPath, pidFile, versionFile },
      {
        GRAVITY_SERVER_BIN: stubBin,
        STUB_SERVER_VERSION: "4.5.2",
        STUB_SPAWN_LOG: spawnLog,
      },
    );

    // Fire two _ensure-server invocations concurrently against the
    // same env + same hooks dir + same marker/pid/socket paths.
    const [r1, r2] = await Promise.all([
      runEnsureServer({
        hooksDir: hooks.hooksDir,
        eventName: "SessionStart",
        env,
      }),
      runEnsureServer({
        hooksDir: hooks.hooksDir,
        eventName: "SessionStart",
        env,
      }),
    ]);

    expect(r1.exitInfo.code).toBe(0);
    expect(r2.exitInfo.code).toBe(0);

    // After both invocations: marker must read the newer version
    // (not corrupted to something else, not double-bumped).
    expect(readFileSync(versionFile, "utf-8")).toBe("4.5.2");

    // Exactly one new pid, alive, different from old.
    const finalPid = parseInt(readFileSync(pidFile, "utf-8").trim(), 10);
    expect(finalPid).not.toBe(oldPid);
    expect(finalPid).toBeGreaterThan(0);
    expect(() => process.kill(finalPid, 0)).not.toThrow();

    // The lock file MUST be cleaned up by the time both invocations
    // exit (this is the loser-falls-through-to-no-op contract).
    expect(existsSync(join(home, ".local", "state", "gravity-server.lock"))).toBe(
      false,
    );

    // OLD stub is dead.
    expect(() => process.kill(oldPid, 0)).toThrow();

    // F2/C1: despite two concurrent detectors, exactly ONE restart-spawn.
    expect(countSpawnLog(spawnLog)).toBe(1);

    stub = undefined;
  });
});

// ─── T10 — restarted server's descendant receives group TERM (F3/C3) ────

describe("T10: restart group-TERM reaches server descendants", () => {
  let oldStubPid: number | undefined;
  let newStubPid: number | undefined;
  let descendantPid: number | undefined;

  afterEach(() => {
    // Defensive force-kill — a failed assertion must not leave a
    // `sleep 600` (or stub node process) straggler.
    for (const pid of [oldStubPid, newStubPid, descendantPid]) {
      if (pid === undefined) continue;
      try {
        process.kill(pid, "SIGKILL");
      } catch {
        /* already dead */
      }
    }
    oldStubPid = undefined;
    newStubPid = undefined;
    descendantPid = undefined;
  });

  it.skipIf(!hasSetsid)(
    "restarted server's descendant receives group TERM",
    async () => {
      const home = freshTempDir("wp2-t10-home-");
      const sockPath = join(home, "gravity-hooks.sock");
      const pidFile = join(home, "gravity-server.pid");
      const versionFile = join(home, "gravity-server.version");
      const childPidFile = join(home, "stub-child.pid");

      const stubBin = aliasStubAwayFromGravityServerName();
      const hooks = setupTestHooksDir({ ownVersion: "4.5.2" });

      // Cold-spawn the "old server" through the REAL `_spawn-server`
      // (socket/pid absent → the unconditional cold-spawn path runs,
      // for any event name) so it becomes a setsid group leader with
      // a descendant child in its process group.
      const coldEnv = buildChildEnv(
        home,
        { sockPath, pidFile, versionFile },
        {
          GRAVITY_SERVER_BIN: stubBin,
          STUB_SERVER_VERSION: "4.5.1",
          STUB_CHILD_PID_FILE: childPidFile,
        },
      );
      const coldResult = await runEnsureServer({
        hooksDir: hooks.hooksDir,
        eventName: "PostToolUse",
        env: coldEnv,
      });
      expect(coldResult.exitInfo.code).toBe(0);

      // Poll for pid file + socket + child-pid file (cold-spawn already
      // waits up to 2s for the socket internally, but poll defensively
      // for all three to avoid a startup-ordering flake).
      const deadline = Date.now() + 5000;
      while (
        Date.now() < deadline &&
        !(existsSync(pidFile) && existsSync(sockPath) && existsSync(childPidFile))
      ) {
        await new Promise((r) => setTimeout(r, 10));
      }
      expect(existsSync(pidFile)).toBe(true);
      expect(existsSync(sockPath)).toBe(true);
      expect(existsSync(childPidFile)).toBe(true);

      const oldPid = parseInt(readFileSync(pidFile, "utf-8").trim(), 10);
      const childPid = parseInt(readFileSync(childPidFile, "utf-8").trim(), 10);
      expect(oldPid).toBeGreaterThan(0);
      expect(childPid).toBeGreaterThan(0);
      oldStubPid = oldPid;
      descendantPid = childPid;

      // Descendant is alive before the restart.
      expect(() => process.kill(childPid, 0)).not.toThrow();

      // Trigger a stale restart via SessionStart: own version (4.5.2,
      // from the per-test plugin.json) is strictly newer than the
      // marker the old stub wrote (4.5.1) → restart path → group-TERM.
      const restartEnv = buildChildEnv(
        home,
        { sockPath, pidFile, versionFile },
        {
          GRAVITY_SERVER_BIN: stubBin,
          STUB_SERVER_VERSION: "4.5.2",
        },
      );
      const restartResult = await runEnsureServer({
        hooksDir: hooks.hooksDir,
        eventName: "SessionStart",
        env: restartEnv,
      });
      expect(restartResult.exitInfo.code).toBe(0);

      const newPid = parseInt(readFileSync(pidFile, "utf-8").trim(), 10);
      expect(newPid).not.toBe(oldPid);
      newStubPid = newPid;
      expect(readFileSync(versionFile, "utf-8")).toBe("4.5.2");

      // Poll briefly for the descendant to die (group-TERM propagation
      // + process teardown is not instant).
      const deathDeadline = Date.now() + 2000;
      let descendantAlive = true;
      while (Date.now() < deathDeadline) {
        try {
          process.kill(childPid, 0);
        } catch {
          descendantAlive = false;
          break;
        }
        await new Promise((r) => setTimeout(r, 20));
      }

      // Counterfactual this guards: without `setsid` in `_spawn-server`,
      // the old stub would not be a process-group leader, the
      // group-TERM `kill -TERM "-$old_pid"` would be a no-op, and this
      // descendant would survive the restart.
      expect(descendantAlive).toBe(false);
    },
  );
});

// ─── T11 — concurrent cold spawns with NO pre-existing server → exactly
//           one restart-spawn (the cold-path lock re-check) ─────────────

describe("T11: concurrent cold spawn with no pre-existing server", () => {
  let coldPid: number | undefined;

  afterEach(async () => {
    if (coldPid !== undefined) {
      try {
        process.kill(coldPid, "SIGTERM");
      } catch {
        /* already dead */
      }
    }
    coldPid = undefined;
    // Cold-spawned stub is a grandchild (spawned by `_spawn-server`, not
    // via `trackedSpawn`), so the file-level `trackedSpawn` afterEach
    // does not reap it. Give it a moment to settle after SIGTERM.
    await new Promise((r) => setTimeout(r, 200));
  });

  it("two concurrent PostToolUse invocations racing a cold start produce exactly one spawn", async () => {
    const home = freshTempDir("wp2-t11-home-");
    const sockPath = join(home, "gravity-hooks.sock");
    const pidFile = join(home, "gravity-server.pid");
    const versionFile = join(home, "gravity-server.version");
    const spawnLog = join(home, "spawn.log");

    // Socket, pid file, and marker must all be ABSENT so `_ensure-server`
    // reaches the bottom cold-spawn call for both invocations.
    expect(existsSync(sockPath)).toBe(false);
    expect(existsSync(pidFile)).toBe(false);
    expect(existsSync(versionFile)).toBe(false);

    const hooks = setupTestHooksDir({ ownVersion: "4.5.2" });
    const stubBin = aliasStubAwayFromGravityServerName();
    const env = buildChildEnv(
      home,
      { sockPath, pidFile, versionFile },
      {
        GRAVITY_SERVER_BIN: stubBin,
        STUB_SERVER_VERSION: "4.5.2",
        STUB_SPAWN_LOG: spawnLog,
      },
    );

    const [r1, r2] = await Promise.all([
      runEnsureServer({
        hooksDir: hooks.hooksDir,
        eventName: "PostToolUse",
        env,
      }),
      runEnsureServer({
        hooksDir: hooks.hooksDir,
        eventName: "PostToolUse",
        env,
      }),
    ]);

    expect(r1.exitInfo.code).toBe(0);
    expect(r2.exitInfo.code).toBe(0);

    // The load-bearing assertion: without the fix, the loser kills the
    // winner's freshly cold-spawned stub and respawns → 2 spawns.
    expect(countSpawnLog(spawnLog)).toBe(1);

    // Exactly one live pid.
    const finalPid = parseInt(readFileSync(pidFile, "utf-8").trim(), 10);
    expect(finalPid).toBeGreaterThan(0);
    expect(() => process.kill(finalPid, 0)).not.toThrow();
    coldPid = finalPid;

    // Lock file cleaned up.
    expect(existsSync(join(home, ".local", "state", "gravity-server.lock"))).toBe(
      false,
    );
  });
});

// ─── T11 — the helper must survive its own orphan-reaper ────────────────
//
// `_spawn-server` takes SERVER_BIN as an argv argument, so its own command
// line contains the server path. An unanchored `pkill -f
// "gravity-server\.(mjs|ts)"` therefore matched the helper itself and
// SIGTERMed it before it could spawn — and, because it died before its
// cleanup, stranded the lock, which then wedged every later spawn.
// Every other test here aliases the stub away from the production
// filename, so none of them can reproduce it; this one uses the REAL name
// on purpose. Note: the reaper is global, so this test also kills any
// gravity-server running on the host.

/** Copy the stub to a temp path named exactly like the shipped bundle, so
 *  the helper's argv carries the production `gravity-server.mjs` string. */
function stubUnderProductionName(): string {
  const dir = freshTempDir("prod-name-");
  const p = join(dir, "gravity-server.mjs");
  copyFileSync(STUB_PATH, p);
  return p;
}

describe("_spawn-server with a production-named SERVER_BIN", () => {
  it("still cold-spawns (does not SIGTERM itself via its own reaper)", async () => {
    const { hooksDir } = setupTestHooksDir({ ownVersion: "4.6.1" });
    const home = freshTempDir("prod-home-");
    const stateDir = join(home, ".local", "state");
    mkdirSync(stateDir, { recursive: true });
    const paths = {
      sockPath: join(stateDir, "gravity-hooks.sock"),
      pidFile: join(stateDir, "gravity-server.pid"),
      versionFile: join(stateDir, "gravity-server.version"),
    };

    await runEnsureServer({
      hooksDir,
      eventName: "PostToolUse",
      env: buildChildEnv(home, paths, {
        GRAVITY_SERVER_BIN: stubUnderProductionName(),
        STUB_SERVER_VERSION: "4.6.1",
      }),
    });

    // Reaching the spawn at all is the assertion: socket bound + live pid.
    expect(existsSync(paths.sockPath)).toBe(true);
    expect(existsSync(paths.pidFile)).toBe(true);
    const pid = Number(readFileSync(paths.pidFile, "utf-8").trim());
    expect(Number.isInteger(pid) && pid > 0).toBe(true);

    // And the lock must not be stranded — that is what turned the one-shot
    // self-kill into a permanent wedge.
    expect(existsSync(join(stateDir, "gravity-server.lock"))).toBe(false);

    try {
      process.kill(pid, "SIGTERM");
    } catch {
      /* already gone */
    }
  });
});
