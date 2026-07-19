// hook-socket-passthrough.test.ts — WP2 gate tests for the bridge early-exit
// fix. Drives the REAL bridge entry point (`src/index.ts`) as a subprocess
// rather than reimplementing the gate. Two scenarios:
//
//   A. Live hook socket present, no legacy socket → envelope reaches the
//      hook socket.
//   B. Hook socket absent → fast pass-through + visible log line.
//
// Every test sets its own per-test temp env (PATH, HOME, GRAVITY_HOOK_SOCK,
// CLAUDE_GRAVITY_SOCK) so it never touches the real
// `~/.local/state/claude-gravity.sock` symlink on this host (which would
// produce a false pass because that symlink actually points at a live
// hook socket path).
//
// Cleanup: every test removes its temp socket + temp dir, and SIGKILLs
// its child on any failure path (including timeout).

import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { spawn, type ChildProcess } from "node:child_process";
import {
  existsSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  unlinkSync,
} from "node:fs";
import { createServer, type Server, type Socket } from "node:net";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";

const BRIDGE_ROOT = resolve(__dirname, "..");
const BRIDGE_SRC = join(BRIDGE_ROOT, "src", "index.ts");

/** Generous bound for the bridge to do its work (connect, send NDJSON,
 *  ack, exit). It completes in well under 1s on a healthy host; the
 *  floor is wide enough to absorb CI jitter but tight enough to fail
 *  fast on a regression that hangs the child. */
const BRIDGE_CHILD_TIMEOUT_MS = 10_000;

/** Minimal env for a spawned bridge child. We deliberately do NOT
 *  inherit `process.env` — the plan explicitly forbids the real
 *  `~/.local/state/claude-gravity.sock` symlink leaking into a test,
 *  and inheriting env-vars also risks pulling in unrelated
 *  `CLAUDE_*`/`*_DUMP*` settings.
 *
 *  The optional `extraEnv` is merged last so individual tests can
 *  inject per-test overrides (e.g. `EMACS_BRIDGE_LOG_FILE`) without
 *  widening Test A's call site. */
function buildChildEnv(
  hookSockPath: string,
  legacySockPath: string,
  homeDir: string,
  extraEnv: Record<string, string> = {},
): Record<string, string> {
  return {
    PATH: process.env.PATH ?? "/usr/local/bin:/usr/bin:/bin",
    HOME: homeDir,
    GRAVITY_HOOK_SOCK: hookSockPath,
    CLAUDE_GRAVITY_SOCK: legacySockPath,
    ...extraEnv,
  };
}

/** Spawn the bridge as `npx tsx src/index.ts <eventName>`. The harness
 *  resolves `tsx` from the workspace's `node_modules/.bin/`. */
function spawnBridge(
  eventName: string,
  env: Record<string, string>,
  stdinPayload: string,
): {
  child: ChildProcess;
  stdoutBuf: Buffer[];
  stderrBuf: Buffer[];
  killChild: (signal?: NodeJS.Signals) => void;
} {
  const child = spawn("npx", ["tsx", BRIDGE_SRC, eventName], {
    cwd: BRIDGE_ROOT,
    env,
    stdio: ["pipe", "pipe", "pipe"],
  });
  const stdoutBuf: Buffer[] = [];
  const stderrBuf: Buffer[] = [];
  child.stdout?.on("data", (chunk: Buffer) => stdoutBuf.push(chunk));
  child.stderr?.on("data", (chunk: Buffer) => stderrBuf.push(chunk));
  // Feed stdin synchronously so the child isn't blocked on its read.
  child.stdin?.write(stdinPayload);
  child.stdin?.end();

  const killChild = (signal: NodeJS.Signals = "SIGKILL"): void => {
    if (!child.killed && child.exitCode === null) {
      try {
        child.kill(signal);
      } catch {
        /* ignore */
      }
    }
  };
  return { child, stdoutBuf, stderrBuf, killChild };
}

/** Wait for the spawned bridge to exit. Resolves with `{ code, signal }`
 *  on natural exit, rejects with a timeout error AND SIGKILLs the child
 *  so the test never leaves a stuck process. */
function awaitExit(
  child: ChildProcess,
  timeoutMs: number,
  killChild: (signal?: NodeJS.Signals) => void,
): Promise<{ code: number | null; signal: NodeJS.Signals | null }> {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      killChild("SIGKILL");
      reject(
        new Error(
          `bridge child did not exit within ${timeoutMs} ms (SIGKILL'd)`,
        ),
      );
    }, timeoutMs);
    timer.unref();
    child.once("exit", (code, signal) => {
      clearTimeout(timer);
      resolve({ code, signal });
    });
  });
}

// ─── Test A scaffolding ──────────────────────────────────────────────────

interface HookServerHandle {
  server: Server;
  sockPath: string;
  received: string[]; // full NDJSON lines accumulated
  /** Resolves to the first NDJSON line the listener receives. */
  firstMessage: Promise<string>;
}

function startHookServer(): Promise<HookServerHandle> {
  const dir = mkdtempSync(join(tmpdir(), "passthrough-hook-"));
  const sockPath = join(dir, "hook.sock");
  const received: string[] = [];
  let resolveFirst!: (line: string) => void;
  const firstMessage = new Promise<string>((res) => {
    resolveFirst = res;
  });
  let firstResolved = false;

  return new Promise((promResolve, promReject) => {
    const server = createServer((sock: Socket) => {
      let buf = "";
      sock.on("data", (chunk: Buffer) => {
        buf += chunk.toString();
        const newlineIdx = buf.indexOf("\n");
        if (newlineIdx >= 0 && !firstResolved) {
          const line = buf.substring(0, newlineIdx);
          received.push(line);
          firstResolved = true;
          resolveFirst(line);
        }
      });
    });
    server.on("error", (err) => promReject(err));
    server.listen(sockPath, () => {
      promResolve({ server, sockPath, received, firstMessage });
    });
  });
}

function stopHookServer(handle: HookServerHandle): Promise<void> {
  return new Promise((resolve) => {
    handle.server.close(() => {
      // After close, also unlink the socket file (defensive — Node
      // usually does this for Unix sockets on close, but be sure).
      try {
        if (existsSync(handle.sockPath)) unlinkSync(handle.sockPath);
      } catch {
        /* ignore */
      }
      resolve();
    });
  });
}

// ─── Per-test temp dirs (cleanup tracking) ──────────────────────────────

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

// ─── Test A — live hook socket present, no legacy socket ─────────────────

describe("bridge early-exit gate — hook socket present", () => {
  let hookHandle: HookServerHandle | undefined;
  let child: ChildProcess | undefined;
  let killChild: ((signal?: NodeJS.Signals) => void) | undefined;

  beforeEach(() => {
    if (hookHandle) {
      // Defensive — should be clean between tests, but be safe.
      void stopHookServer(hookHandle);
      hookHandle = undefined;
    }
  });

  afterEach(async () => {
    if (child && killChild) killChild("SIGKILL");
    if (hookHandle) {
      await stopHookServer(hookHandle);
      hookHandle = undefined;
    }
  });

  it("forwards the SessionStart envelope to the hook socket (no legacy socket)", async () => {
    hookHandle = await startHookServer();
    const tempHome = freshTempDir("passthrough-home-A-");
    const absentLegacy = join(freshTempDir("passthrough-legacy-A-"), "no-legacy.sock");

    const sessionId = `test-sess-A-${Date.now()}-${process.pid}`;
    const cwd = "/tmp/test-proj-A";
    const envelope = JSON.stringify({
      session_id: sessionId,
      cwd,
      transcript_path: null,
    });

    const env = buildChildEnv(hookHandle.sockPath, absentLegacy, tempHome);
    const spawnRes = spawnBridge("SessionStart", env, envelope);
    child = spawnRes.child;
    killChild = spawnRes.killChild;

    // Wait for the first NDJSON line on our listener (with overall
    // timeout via the bridge's exit race) — the fire-and-forget branch
    // writes to the hook socket and then writes its hookResponse JSON
    // to stdout and exits. We bound everything on the child-exit
    // timeout so a stuck child fails fast.
    const firstLine = await Promise.race([
      hookHandle.firstMessage,
      awaitExit(spawnRes.child, BRIDGE_CHILD_TIMEOUT_MS, spawnRes.killChild).then(
        (info) => {
          throw new Error(
            `bridge exited (code=${info.code}, signal=${info.signal}) before sending the hook envelope; ` +
              `stdout=${Buffer.concat(spawnRes.stdoutBuf).toString()} ` +
              `stderr=${Buffer.concat(spawnRes.stderrBuf).toString()}`,
          );
        },
      ),
    ]);

    // Parse + assert wire shape.
    const parsed = JSON.parse(firstLine);
    expect(parsed.event).toBe("SessionStart");
    expect(parsed.session_id).toBe(sessionId);
    expect(parsed.cwd).toBe(cwd);
    expect(parsed.source).toBe("bridge");
    expect(parsed.needs_response).toBe(false);
    expect(parsed.data).toBeDefined();
    expect(parsed.data.session_id).toBe(sessionId);

    // The bridge should also exit cleanly on its own after writing
    // its hookResponse. Wait for it (bounded) so a leak is visible.
    const exitInfo = await awaitExit(spawnRes.child, BRIDGE_CHILD_TIMEOUT_MS, spawnRes.killChild);
    expect(exitInfo.code).toBe(0);
  });
});

// ─── Test B — hook socket absent → pass-through + visible log ────────────

describe("bridge early-exit gate — hook socket absent", () => {
  let child: ChildProcess | undefined;
  let killChild: ((signal?: NodeJS.Signals) => void) | undefined;
  let absentHookPath: string | undefined;
  let logPath: string | undefined;

  afterEach(() => {
    if (child && killChild) killChild("SIGKILL");
    child = undefined;
    killChild = undefined;
    absentHookPath = undefined;
    logPath = undefined;
  });

  it("writes {} to stdout and a new WARN log line into the per-test temp log file", async () => {
    // Hook-socket-absent: a fresh mkdtempSync dir + a socket filename
    // we never create/bind. The bridge's hookSocketExists() must
    // return false, triggering the pass-through.
    const dir = freshTempDir("passthrough-hook-absent-");
    absentHookPath = join(dir, "no-listener.sock");
    const absentLegacy = join(freshTempDir("passthrough-legacy-B-"), "no-legacy.sock");
    const tempHome = freshTempDir("passthrough-home-B-");

    // Per-test log file path. The FILE itself is NOT pre-created — it
    // is created on first write by the bridge child. We point the
    // bridge at it via EMACS_BRIDGE_LOG_FILE so we can assert on the
    // full contents (no shared-file byte-delta trick needed and no
    // touching of the host's shared log path).
    logPath = join(freshTempDir("passthrough-log-B-"), "bridge.log");

    // Sanity: neither path exists before we start.
    expect(existsSync(absentHookPath)).toBe(false);
    expect(existsSync(logPath)).toBe(false);

    const envelope = JSON.stringify({
      session_id: `test-sess-B-${Date.now()}-${process.pid}`,
      cwd: "/tmp/test-proj-B",
      transcript_path: null,
    });

    const env = buildChildEnv(absentHookPath, absentLegacy, tempHome, {
      EMACS_BRIDGE_LOG_FILE: logPath,
    });
    const spawnRes = spawnBridge("SessionStart", env, envelope);
    child = spawnRes.child;
    killChild = spawnRes.killChild;

    const exitInfo = await awaitExit(spawnRes.child, BRIDGE_CHILD_TIMEOUT_MS, spawnRes.killChild);
    expect(exitInfo.code).toBe(0);

    // --- stdout assertion: must be exactly `{}` followed by a newline.
    const stdoutStr = Buffer.concat(spawnRes.stdoutBuf).toString();
    expect(stdoutStr).toBe("{}\n");

    // --- log assertion: the per-test temp file must exist after the
    // child ran (the bridge writes the pass-through WARN line into
    // it via EMACS_BRIDGE_LOG_FILE) and contain the reason string.
    // The file starts absent; we read the whole thing because no
    // other writer touches this temp path.
    if (!existsSync(logPath)) {
      throw new Error(
        `expected per-test log file to exist after child exit, but it does not: ` +
          `${logPath}. child stderr=${Buffer.concat(spawnRes.stderrBuf).toString()}`,
      );
    }
    const logContents = readFileSync(logPath, "utf-8");
    expect(logContents).toContain(
      `Hook socket not found at ${absentHookPath}, passing through`,
    );
  });
});
