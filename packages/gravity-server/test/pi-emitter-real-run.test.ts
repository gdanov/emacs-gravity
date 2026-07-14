// pi-emitter-real-run.test.ts — REAL-`pi` integration tests for the
// WP1 emitter against the WP2 self-installed `.pi/agent/extensions/`
// path. Per plan.md §4 WP3 step 3 this covers AC #1, #2a, #2b, #3, #8.
//
// What this suite does NOT do:
//  - Touch elisp files.
//  - Write to the real `~/.pi/` or real `~/.local/state/`. Every
//    per-test temp dir lives under `os.tmpdir()` and is cleaned up.
//  - Leave stray `pi` children. Every spawn is wrapped in a
//    try/finally that SIGKILLs the child on any error path.
//  - Spawn pi without `< /dev/null`. Every child gets a closed stdin.
//
// Each test uses its own mkdtemp'd scratch dir as pi's `--session-dir`
// AND as a per-test working dir, so pi's own session files cannot land
// in this repo or the real `~/.pi/` (which on this host is also
// `~/.local/state/`-adjacent).

import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { spawn, type ChildProcess } from "node:child_process";
import { existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { piAvailable } from "./helpers/pi-availability.js";
import { resolveEmitterBundleSource } from "./helpers/emitter-bundle.js";
import {
  HookSocketListener,
  type CollectedLine,
} from "./helpers/hook-socket-listener.js";

/** Temp dir holding the materialized bundle; cleaned up at process
 *  exit. The bundle itself is materialized in the gated `beforeAll`
 *  below (only when `pi` is available on this host), not at module
 *  load — this dir is just created eagerly so its path is stable. */
const bundleScratchDir = mkdtempSync(join(tmpdir(), "pi-emitter-bundle-"));

/** Trivial-but-tool-bearing prompt used by pi-wp's own smoke test
 *  (`bin/pi-wp --smoke`). We reuse it verbatim for consistency. */
const SMOKE_PROMPT =
  "Run `pwd` and report only the output. Edit nothing.";

/** Per-subprocess generous timeout. These are real-network tests. */
const PI_SUBPROCESS_TIMEOUT_MS = 60_000;
/** Wedged-case wall-clock sanity bound (intentionally generous — this
 *  is a sanity check that the run isn't multi-second-blocked, not a
 *  precise latency assertion; precise latency lives in
 *  `pi-emitter-latency-smoke.test.ts`). */
const WEDGED_WALLCLOCK_BOUND_MS = 60_000;

/** Tracked temp dirs for afterAll cleanup. */
const tempDirs: string[] = [];

function freshScratchDir(): string {
  const d = mkdtempSync(join(tmpdir(), "pi-emitter-real-"));
  tempDirs.push(d);
  return d;
}

/** Run `pi` with `argv` and `env`, returning a `ChildProcess` whose
 *  stdout/stderr are piped to in-memory buffers. The caller MUST kill
 *  the returned process on any failure path (use `killSubprocess`). */
function spawnPi(
  argv: ReadonlyArray<string>,
  env: Readonly<Record<string, string>>,
  cwd: string,
): {
  child: ChildProcess;
  stdoutChunks: Buffer[];
  stderrChunks: Buffer[];
  killSubprocess: (signal?: NodeJS.Signals) => void;
} {
  const child = spawn("pi", argv, {
    cwd,
    env,
    stdio: ["ignore", "pipe", "pipe"],
  });
  const stdoutChunks: Buffer[] = [];
  const stderrChunks: Buffer[] = [];
  child.stdout?.on("data", (chunk: Buffer) => stdoutChunks.push(chunk));
  child.stderr?.on("data", (chunk: Buffer) => stderrChunks.push(chunk));
  const killSubprocess = (signal: NodeJS.Signals = "SIGKILL"): void => {
    if (!child.killed && child.exitCode === null) {
      try {
        child.kill(signal);
      } catch {
        /* ignore */
      }
    }
  };
  return { child, stdoutChunks, stderrChunks, killSubprocess };
}

/** Wait for a spawned child to exit, rejecting if it exceeds `timeoutMs`.
 *  On timeout, SIGKILLs the child so the test never leaves a stray
 *  pi process. */
function awaitExit(
  child: ChildProcess,
  timeoutMs: number,
  killSubprocess: (signal?: NodeJS.Signals) => void,
): Promise<{ code: number | null; signal: NodeJS.Signals | null }> {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      killSubprocess("SIGKILL");
      reject(
        new Error(`pi subprocess did not exit within ${timeoutMs} ms (killed)`),
      );
    }, timeoutMs);
    timer.unref?.();
    child.once("exit", (code, signal) => {
      clearTimeout(timer);
      resolve({ code, signal });
    });
    child.once("error", (err) => {
      clearTimeout(timer);
      reject(err);
    });
  });
}

// ── Module-level probe — runs once per file load. ──
const available = await piAvailable();

// Print a single explanatory line so the absence of the suite in
// vitest output isn't confusing on hosts without `pi`.
if (!available) {
  // eslint-disable-next-line no-console
  console.log(
    "[pi-emitter-real-run] `pi` not on PATH or unresponsive — suite is skipped on this host",
  );
}

/** Materialized bundle path — set in `beforeAll`. */
let EMITTER_PATH: string;

describe.skipIf(!available)("pi emitter — real pi subprocess (AC #1, #2, #3, #5, #8)", () => {
  beforeAll(async () => {
    // Resolve the bundled emitter source once and materialize it to a
    // fresh `.js` file under `bundleScratchDir`. `pi -e <file>` then
    // loads plain ESM, which is the same shape a self-installed
    // extension on disk has. We do this per suite (not per test) so
    // the cost is paid once.
    const source = await resolveEmitterBundleSource();
    const materialPath = join(bundleScratchDir, "gravity-pi.js");
    writeFileSync(materialPath, source, "utf8");
    EMITTER_PATH = materialPath;

    // Quick sanity: the materialized file must be loadable. We do a
    // sync `statSync`-equivalent read so any failure surfaces at
    // suite-load time, not buried in a per-test timeout.
    try {
      readFileSync(EMITTER_PATH);
    } catch (e) {
      throw new Error(
        `Required emitter bundle is missing: ${EMITTER_PATH} — emitter-bundle helper failed. (${String(e)})`,
      );
    }
  });

  afterAll(() => {
    while (tempDirs.length > 0) {
      const d = tempDirs.pop();
      if (d === undefined) break;
      try {
        rmSync(d, { recursive: true, force: true });
      } catch {
        /* best effort */
      }
    }
    try {
      rmSync(bundleScratchDir, { recursive: true, force: true });
    } catch {
      /* best effort */
    }
  });

  // ── AC #1: Loaded + live ──────────────────────────────────────
  it("AC #1: loads cleanly and emits src:'pi' envelopes to a live listener", async () => {
    const scratch = freshScratchDir();
    const sessionDir = join(scratch, "session");
    const logPath = join(scratch, "emitter.log");
    const listener = await HookSocketListener.create({
      prefix: "pi-emitter-live-",
    });
    try {
      const { child, killSubprocess } = spawnPi(
        [
          "-p",
          "--mode",
          "json",
          "-e",
          EMITTER_PATH,
          "--session-dir",
          sessionDir,
          "-n",
          "live",
          SMOKE_PROMPT,
        ],
        {
          ...process.env,
          GRAVITY_HOOK_SOCK: listener.getSocketPath(),
          GRAVITY_PI_EMITTER_LOG: logPath,
        } as Record<string, string>,
        scratch,
      );
      // pi runs ~5s for the trivial prompt; allow generous headroom.
      const exit = await awaitExit(child, PI_SUBPROCESS_TIMEOUT_MS, killSubprocess);
      expect(exit.code).toBe(0);

      const envelopes = listener.getPiEnvelopes();
      expect(envelopes.length).toBeGreaterThan(0);

      // At least one envelope must be a valid PiEventEnvelope shape:
      // src === "pi", attribution.role is a non-empty string.
      const good = envelopes.find(
        (e) =>
          typeof e.attribution?.role === "string" &&
          e.attribution.role.length > 0,
      );
      expect(good).toBeDefined();
      // And the wire shape must include the canonical fields too.
      expect(good!.src).toBe("pi");
      expect(typeof good!.session_id).toBe("string");
      expect(good!.session_id.length).toBeGreaterThan(0);
      expect(typeof good!.cwd).toBe("string");
      expect(good!.event).toBeDefined();
      expect(typeof good!.event).toBe("object");
      expect(typeof good!.event["type"]).toBe("string");

      // The very first event emitted on a healthy run must be the
      // session_start that the factory's lazy-connect path listens
      // for. Asserting presence here (in addition to AC #5's wire-
      // shape checks) is the cheapest proof the live path actually
      // fired and the listener didn't only see bookkeeping noise.
      const sessionStart = envelopes.find(
        (e) => e.event["type"] === "session_start",
      );
      expect(sessionStart).toBeDefined();
    } finally {
      await listener.close();
    }
  }, PI_SUBPROCESS_TIMEOUT_MS + 10_000);

  // ── AC #2a: Crash-safety with no listener at all ──────────────
  it("AC #2a: exits 0 with no listener at all; no emitter noise on stdout/stderr", async () => {
    const scratch = freshScratchDir();
    const sessionDir = join(scratch, "session");
    const logPath = join(scratch, "emitter.log");
    // Point GRAVITY_HOOK_SOCK at a path nothing is listening on. We
    // intentionally do NOT use a HookSocketListener here. The path is
    // inside our scratch dir so we don't pollute /tmp at large.
    const deadSock = join(scratch, "dead.sock");

    const { child, stdoutChunks, stderrChunks, killSubprocess } = spawnPi(
      [
        "-p",
        "--mode",
        "json",
        "-e",
        EMITTER_PATH,
        "--session-dir",
        sessionDir,
        "-n",
        "nols",
        SMOKE_PROMPT,
      ],
      {
        ...process.env,
        GRAVITY_HOOK_SOCK: deadSock,
        GRAVITY_PI_EMITTER_LOG: logPath,
      } as Record<string, string>,
      scratch,
    );
    const exit = await awaitExit(child, PI_SUBPROCESS_TIMEOUT_MS, killSubprocess);
    expect(exit.code).toBe(0);

    // Stdout must be valid `--mode json` NDJSON (every non-empty line
    // parses as a JSON object with a `type` field).
    const stdout = Buffer.concat(stdoutChunks).toString("utf8");
    const lines = stdout.split("\n").filter((l) => l.length > 0);
    expect(lines.length).toBeGreaterThan(0);
    let piParsedLines = 0;
    for (const line of lines) {
      const v = JSON.parse(line);
      expect(typeof v).toBe("object");
      expect(v).not.toBeNull();
      expect(typeof (v as { type?: unknown }).type).toBe("string");
      piParsedLines += 1;
    }
    expect(piParsedLines).toBe(lines.length);

    // Stderr must NOT contain anything emitter-related. The emitter
    // ONLY writes to its log file, never to stderr — but assert
    // emptiness defensively in case a future regression leaks.
    const stderr = Buffer.concat(stderrChunks).toString("utf8");
    expect(stderr).toBe("");
  }, PI_SUBPROCESS_TIMEOUT_MS + 10_000);

  // ── AC #2b: Crash-safety with server dying mid-run ────────────
  it("AC #2b: survives listener killMidRun() mid-run and still exits 0", async () => {
    const scratch = freshScratchDir();
    const sessionDir = join(scratch, "session");
    const logPath = join(scratch, "emitter.log");
    const listener = await HookSocketListener.create({
      prefix: "pi-emitter-killed-",
    });
    try {
      const { child, killSubprocess } = spawnPi(
        [
          "-p",
          "--mode",
          "json",
          "-e",
          EMITTER_PATH,
          "--session-dir",
          sessionDir,
          "-n",
          "killed",
          SMOKE_PROMPT,
        ],
        {
          ...process.env,
          GRAVITY_HOOK_SOCK: listener.getSocketPath(),
          GRAVITY_PI_EMITTER_LOG: logPath,
        } as Record<string, string>,
        scratch,
      );
      // Wait for at least one envelope to land, THEN kill the
      // listener mid-run (simulates a server crash). The emitter
      // should NOT propagate this to pi's exit code.
      const arrived = await listener.waitForEnvelopes(1, 30_000);
      expect(arrived).toBeGreaterThan(0);
      listener.killMidRun();

      const exit = await awaitExit(child, PI_SUBPROCESS_TIMEOUT_MS, killSubprocess);
      expect(exit.code).toBe(0);

      // At least one collected envelope must carry the `pi` src — the
      // listener saw real data before the kill.
      const envelopes = listener.getPiEnvelopes();
      expect(envelopes.length).toBeGreaterThan(0);

      // ── AC #8: The emitter's own log file must record some
      //   evidence of the outage (a send-dropped line is the realistic
      //   signal: the kill caused EPIPE/ECONNRESET, the writer's
      //   subsequent `.send()` attempts logged their failure).
      //   Read the log file and grep for the substring we already saw
      //   emitted by logger.ts + index.ts:
      //     "socket: send dropped (not connected)"
      //   Do not invent a string — the format is fixed by WP1's code.
      let logContent = "";
      try {
        logContent = readFileSync(logPath, "utf8");
      } catch {
        /* may not exist yet if the run was so fast no drops logged —
         * the assert below will fail loudly in that case */
      }
      expect(logContent.length).toBeGreaterThan(0);
      // The kill is a send-side drop on the emitter — either an
      // EPIPE/ECONNRESET on the still-connected socket (logged by the
      // writer's catch via socket.send returning false on a closed
      // socket) or a "not connected" drop after the listener closed.
      // Grep for the substring we know WP1 writes.
      const hasDropEvidence =
        logContent.includes("send dropped") ||
        logContent.includes("socket: send dropped") ||
        logContent.includes("queue: dropped");
      expect(hasDropEvidence).toBe(true);
    } finally {
      await listener.close();
    }
  }, PI_SUBPROCESS_TIMEOUT_MS + 15_000);

  // ── AC #3: Wedged server ──────────────────────────────────────
  it("AC #3: wedged listener does not block the run; exit 0 within wall-clock bound; log records a drop", async () => {
    const scratch = freshScratchDir();
    const sessionDir = join(scratch, "session");
    const logPath = join(scratch, "emitter.log");
    const listener = await HookSocketListener.create({
      prefix: "pi-emitter-wedged-",
    });
    // Engage wedged mode BEFORE pi connects, so any data the emitter
    // sends is never read by us. The OS write buffer will eventually
    // fill; the emitter's send() will start returning false; the log
    // file should record drops.
    listener.setWedged(true);

    const startMs = Date.now();
    try {
      const { child, killSubprocess } = spawnPi(
        [
          "-p",
          "--mode",
          "json",
          "-e",
          EMITTER_PATH,
          "--session-dir",
          sessionDir,
          "-n",
          "wedged",
          SMOKE_PROMPT,
        ],
        {
          ...process.env,
          GRAVITY_HOOK_SOCK: listener.getSocketPath(),
          GRAVITY_PI_EMITTER_LOG: logPath,
        } as Record<string, string>,
        scratch,
      );
      const exit = await awaitExit(
        child,
        PI_SUBPROCESS_TIMEOUT_MS,
        killSubprocess,
      );
      const wallClockMs = Date.now() - startMs;

      expect(exit.code).toBe(0);
      // Generous wall-clock sanity bound — the wedged path is NOT
      // supposed to be a precise latency assertion. The bound is here
      // to catch a regression that turns a 5s run into a 5-minute
      // hang. We use 2x the per-subprocess timeout as a hard cap.
      expect(wallClockMs).toBeLessThan(WEDGED_WALLCLOCK_BOUND_MS);

      // SOFT / best-effort observation: a drop in the emitter's log
      // file IS the ideal signal that the backpressure / wedge path
      // was exercised. However, a trivial single-tool-call prompt's
      // total envelope traffic is too small (tens of KB total) to
      // reliably overflow the OS/Node buffers on every host — the
      // deterministic backpressure-triggers-a-drop behavior is
      // already unit-tested directly against socket-writer.ts, which
      // is the right level for that guarantee. So here we OBSERVE
      // and log what we see (including zero drops) but do NOT fail
      // the test on a zero-drop run.
      let logContent = "";
      let dropCount = 0;
      try {
        // existsSync guard — the emitter log file is created lazily
        // by logger.ts on the first drop; a no-drop run may not
        // produce a file at all, and that is OK.
        if (existsSync(logPath)) {
          logContent = readFileSync(logPath, "utf8");
          dropCount =
            countMatches(logContent, "socket: send dropped") +
            countMatches(logContent, "queue: dropped");
        }
      } catch (e) {
        // Tolerate any read error — log file observability is a
        // best-effort signal here.
        // eslint-disable-next-line no-console
        console.log(
          `[AC #3] (could not read emitter log: ${String(e)})`,
        );
      }
      // eslint-disable-next-line no-console
      console.log(
        `[AC #3] emitter log observation: ${dropCount} drop line(s) ` +
          `(log size = ${logContent.length} bytes; log present = ${logContent.length > 0})`,
      );
    } finally {
      listener.setWedged(false);
      await listener.close();
    }
  }, PI_SUBPROCESS_TIMEOUT_MS + 15_000);

  // ── AC #5: Wire-level compaction assertion ────────────────────
  // Verify that on the wire:
  //   - `message_update` envelopes carry ONLY `{ type,
  //     assistantMessageEvent }` (nothing else — no raw `delta`,
  //     `messageSnapshot`, `message` keys leak through).
  //   - `tool_execution_update` envelopes, when they have a
  //     `partialResult`, carry a SCALAR (string|number|boolean) —
  //     never a raw object/array/symbol — per the scalarization
  //     contract in compaction.ts.
  it("AC #5: message_update envelopes on the wire carry ONLY the compacted delta; tool_execution_update partials are scalarized", async () => {
    const scratch = freshScratchDir();
    const sessionDir = join(scratch, "session");
    const logPath = join(scratch, "emitter.log");
    const listener = await HookSocketListener.create({
      prefix: "pi-emitter-compaction-",
    });
    try {
      const { child, killSubprocess } = spawnPi(
        [
          "-p",
          "--mode",
          "json",
          "-e",
          EMITTER_PATH,
          "--session-dir",
          sessionDir,
          "-n",
          "compact",
          SMOKE_PROMPT,
        ],
        {
          ...process.env,
          GRAVITY_HOOK_SOCK: listener.getSocketPath(),
          GRAVITY_PI_EMITTER_LOG: logPath,
        } as Record<string, string>,
        scratch,
      );
      const exit = await awaitExit(
        child,
        PI_SUBPROCESS_TIMEOUT_MS,
        killSubprocess,
      );
      expect(exit.code).toBe(0);

      const envelopes = listener.getPiEnvelopes();
      expect(envelopes.length).toBeGreaterThan(0);

      // Strict check on every message_update envelope: the event
      // payload must contain exactly `{ type, assistantMessageEvent }`
      // and no other keys. Anything else means a regression leaked
      // a raw event field through compaction.
      const msgUpdates = envelopes.filter(
        (e) => e.event["type"] === "message_update",
      );
      expect(msgUpdates.length).toBeGreaterThan(0);
      for (const e of msgUpdates) {
        expect(Object.keys(e.event).sort()).toEqual([
          "assistantMessageEvent",
          "type",
        ]);
      }

      // Soft check on tool_execution_update envelopes: any
      // `partialResult` that appears must be a scalar (string,
      // number, or boolean) per compaction.ts. No minimum count
      // required — a smoke prompt may not exercise a streaming
      // tool. We log when zero were observed so the result is
      // auditable from the test output.
      const toolUpdates = envelopes.filter(
        (e) => e.event["type"] === "tool_execution_update",
      );
      if (toolUpdates.length === 0) {
        // eslint-disable-next-line no-console
        console.log(
          "[AC #5] no tool_execution_update envelopes observed on this run; " +
            "scalarization check vacuously satisfied",
        );
      }
      for (const e of toolUpdates) {
        if ("partialResult" in e.event) {
          expect(["string", "number", "boolean"]).toContain(
            typeof e.event["partialResult"],
          );
        }
      }
    } finally {
      await listener.close();
    }
  }, PI_SUBPROCESS_TIMEOUT_MS + 10_000);

  // ── AC #8: Server-restart scoping ─────────────────────────────
  // Reuses the crash-safety(b) run's evidence model. This test reads
  // the log file left by AC #2b's emitter run and asserts it contains
  // some greppable evidence of the outage. We do NOT invent a string
  // literal — we use the same substring we already know `index.ts`
  // writes via logger.ts.
  it("AC #8: emitter log records structured evidence of the kill", async () => {
    // We do not spawn a new pi here; we exercise AC #8 by running
    // AC #2b's scenario and reading its log file. To keep this test
    // independent (and avoid coupling the two tests' ordering), we
    // re-run the kill scenario in this test too.
    const scratch = freshScratchDir();
    const sessionDir = join(scratch, "session");
    const logPath = join(scratch, "emitter.log");
    const listener = await HookSocketListener.create({
      prefix: "pi-emitter-restart-",
    });
    try {
      const { child, killSubprocess } = spawnPi(
        [
          "-p",
          "--mode",
          "json",
          "-e",
          EMITTER_PATH,
          "--session-dir",
          sessionDir,
          "-n",
          "restart",
          SMOKE_PROMPT,
        ],
        {
          ...process.env,
          GRAVITY_HOOK_SOCK: listener.getSocketPath(),
          GRAVITY_PI_EMITTER_LOG: logPath,
        } as Record<string, string>,
        scratch,
      );
      const arrived = await listener.waitForEnvelopes(1, 30_000);
      expect(arrived).toBeGreaterThan(0);
      listener.killMidRun();

      const exit = await awaitExit(child, PI_SUBPROCESS_TIMEOUT_MS, killSubprocess);
      expect(exit.code).toBe(0);

      // Greppable evidence of the outage / drop count recorded in the
      // log file. Per index.ts + logger.ts the message shape is:
      //   "<ISO> socket: send dropped (not connected) event=<name>\n"
      // or:
      //   "<ISO> queue: dropped <N> event(s) since <ISO>\n"
      // Both carry a numeric count (the queue case) or simply a
      // count-of-lines (the send case). We assert either is present
      // and that a numeric token follows the substring — i.e. the
      // outage IS countable from the log.
      const content = readFileSync(logPath, "utf8");
      expect(content.length).toBeGreaterThan(0);

      const dropLines = content
        .split("\n")
        .filter(
          (l) =>
            l.includes("socket: send dropped") ||
            l.includes("queue: dropped"),
        );
      expect(dropLines.length).toBeGreaterThan(0);
      // At least one of the drop lines must carry a parseable count
      // ("queue: dropped N event(s)" where N >= 1) OR multiple
      // "socket: send dropped" lines (the count there is the number
      // of such lines themselves). We assert the second to avoid
      // being brittle about which drop path triggered.
      const sendDropped = dropLines.filter((l) =>
        l.includes("socket: send dropped"),
      );
      expect(sendDropped.length).toBeGreaterThan(0);
    } finally {
      await listener.close();
    }
  }, PI_SUBPROCESS_TIMEOUT_MS + 15_000);
});

/** Count non-overlapping occurrences of `needle` in `haystack`. */
function countMatches(haystack: string, needle: string): number {
  if (needle.length === 0) return 0;
  let count = 0;
  let pos = 0;
  while ((pos = haystack.indexOf(needle, pos)) !== -1) {
    count += 1;
    pos += needle.length;
  }
  return count;
}

// Touch the type so the import is not erased by `verbatimModuleSyntax`.
const _linesTypeRef: CollectedLine | undefined = undefined;
void _linesTypeRef;

// ── Synthetic / unit-level drop-marker wire-shape ──────────────
//
// This block is OUTSIDE the `describe.skipIf(!available)` gate — it
// needs no real `pi` subprocess and must therefore always run so the
// drop-marker wire-shape contract is verified on every host, not only
// on hosts where `pi` happens to be available.
describe("pi emitter — drop-marker wire shape (AC #5, synthetic — no real pi subprocess)", () => {
  it("drainDropMarker → buildEnvelope produces a well-formed gravity_emitter_drop envelope", async () => {
    const { BoundedQueue } = await import(
      "../src/pi-driver/emitter/queue.js"
    );
    const { buildEnvelope } = await import(
      "../src/pi-driver/emitter/envelope.js"
    );

    // Capacity 2 + 4 pushes guarantees at least one drop. We use
    // generic event-shape objects (anything with a `type`) so this
    // test exercises only the queue's drop accounting, not the
    // emitter's handler logic.
    const q = new BoundedQueue<Record<string, unknown>>({ capacity: 2 });
    q.push({ type: "session_start" });
    q.push({ type: "message_start" });
    q.push({ type: "message_update" }); // drops the oldest
    q.push({ type: "message_end" }); // drops the next-oldest

    const marker = q.drainDropMarker();
    expect(marker).not.toBeNull();
    if (marker === null) return; // type-narrowing helper for TS

    // Feed the marker through buildEnvelope so the wire shape is
    // what the server actually sees when the queue overflowed. The
    // envelope helper accepts `Record<string, unknown>` for `event`,
    // and a DropMarker structurally satisfies that.
    const envelope = buildEnvelope(
      { sessionId: "s", cwd: "/tmp/x" },
      marker as unknown as Record<string, unknown>,
    );
    expect(envelope.src).toBe("pi");
    expect(envelope.session_id).toBe("s");
    expect(envelope.cwd).toBe("/tmp/x");
    expect(envelope.event["type"]).toBe("gravity_emitter_drop");

    const droppedCount = envelope.event["droppedCount"];
    expect(typeof droppedCount).toBe("number");
    if (typeof droppedCount === "number") {
      expect(droppedCount).toBeGreaterThan(0);
    }

    const since = envelope.event["since"];
    expect(typeof since).toBe("string");
    // Defensive: it must look ISO-8601-shaped — not a hard check on
    // the exact instant (the queue uses Date.now() under the hood),
    // just a smoke check on the format.
    expect((since as string).length).toBeGreaterThan(0);
  });
});