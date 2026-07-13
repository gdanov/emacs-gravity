// pi-emitter-latency-smoke.test.ts — AC #4 latency smoke comparison
// per plan.md §4 WP3 step 4.
//
// Goal: compare wall-clock of N identical runs WITH the emitter loaded
// against N runs WITHOUT it, and report the raw numbers + computed
// delta. This test does NOT hard-fail on exceeding the 2% p95 budget:
// 8 samples is not statistically rigorous enough to gate a build, and
// the spec is explicit that "an 8-sample p95 is not a reliable enough
// signal to gate the build on a strict ≤2% threshold — that would
// make the suite flaky by construction."
//
// Flakiness policy: each individual `pi` invocation is wrapped in its
// own try/catch so one slow / failed / timed-out sample does NOT fail
// the whole test. These are live LLM-provider round trips on a real
// network; an occasional slow sample is normal variance, not a sign
// the emitter is broken. A sample that fails is logged as such and
// skipped; the test then computes the delta from whatever samples
// DID succeed. The test only hard-fails when one of the conditions
// produced fewer than `MIN_SUCCESSES_PER_CONDITION` successful
// exit-0 samples — that's the "the emitter or `pi` itself is broken"
// signal, distinct from "one slow network call".
//
// The test always passes functionally when both conditions produced
// enough successful runs to compare (see `MIN_SUCCESSES_PER_CONDITION`
// below). The numbers and delta are printed via `console.log` and a
// written artifact under the swarm dir (NOT the repo) so a human / the
// conductor can read the real numbers from the test output.
//
// We sample N=8 per condition (16 total subprocess spawns, plus 2
// warm-ups). On a healthy network, each run takes ~5s, so the whole
// test takes ~80-100s. We bound total wall-clock at 300s and the
// per-subprocess timeout at 90s to allow headroom on a loaded host
// without invalidating the comparison.

import { describe, expect, it } from "vitest";
import { spawn, type ChildProcess } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { piAvailable } from "./helpers/pi-availability.js";
import { resolveEmitterBundleSource } from "./helpers/emitter-bundle.js";
import { HookSocketListener } from "./helpers/hook-socket-listener.js";

/** Worktree root — derived from this test file's location rather
 *  than hardcoded so the file works for any checkout. The file
 *  lives at `<worktree>/packages/gravity-server/test/pi-emitter-latency-smoke.test.ts`,
 *  so three levels up from its `dirname` is the worktree root. */
const WORKTREE_ROOT = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "..",
  "..",
  "..",
);

/** Temp dir holding the materialized bundle; cleaned up in a
 *  process-exit hook. Resolved/materailized unconditionally at module
 *  load (top-level await) so `EMITTER_PATH` is always a string — even
 *  on hosts where the `pi` real-subprocess suite skips, the
 *  resolution is cheap and harmless. */
const bundleScratchDir = mkdtempSync(join(tmpdir(), "pi-latency-bundle-"));

/** Trivial-but-tool-bearing prompt — same as pi-wp's own smoke prompt. */
const SMOKE_PROMPT =
  "Run `pwd` and report only the output. Edit nothing.";

/** Per-subprocess timeout (real-network test against a live LLM
 *  provider; 60s was tight on a loaded host, hence 90s). */
const PI_SUBPROCESS_TIMEOUT_MS = 90_000;
/** Number of runs per condition (with-emitter / without). 8 each = 16. */
const N_RUNS = 8;
/** Hard cap on total wall-clock for the whole suite (300s). */
const TOTAL_BUDGET_MS = 300_000;
/**
 * Minimum number of successful exit-0 samples required per condition
 * for the test to pass functionally. Anything less than this is
 * treated as "the emitter or `pi` itself is broken", distinct from
 * "one slow network call" (which is logged and skipped per sample).
 * Picked as 3 (out of 8) so a single slow sample — the realistic
 * failure mode — never fails the test, but a systemic outage that
 * drops most samples still trips the assertion.
 */
const MIN_SUCCESSES_PER_CONDITION = 3;

// ── Module-level probe — runs once per file load. ──
const available = await piAvailable();
if (!available) {
  // eslint-disable-next-line no-console
  console.log(
    "[pi-emitter-latency-smoke] `pi` not on PATH or unresponsive — suite is skipped on this host",
  );
}

// ── Materialize the emitter bundle once per file load. ──
//
// We unconditionally resolve the built bundle and write it to a
// fresh `.js` file so `pi -e <file>` can load plain ESM, matching
// the production self-installed path. This is paid once even on
// hosts where the `pi` real-subprocess suite is gated off — the
// cost is microseconds and it keeps `EMITTER_PATH` a real string
// for both the gated and ungated branches.
const EMITTER_SOURCE = await resolveEmitterBundleSource();
const EMITTER_PATH = join(bundleScratchDir, "gravity-pi.js");
writeFileSync(EMITTER_PATH, EMITTER_SOURCE, "utf8");

// Best-effort cleanup of the bundle scratch dir at process exit.
process.on("exit", () => {
  try {
    rmSync(bundleScratchDir, { recursive: true, force: true });
  } catch {
    /* best effort */
  }
});

describe.skipIf(!available)(
  "pi emitter — latency smoke: with vs without emitter (AC #4, informational)",
  () => {
    const baselineMs: number[] = [];
    const baselineFailures: number[] = [];
    const withEmitterMs: number[] = [];
    const withEmitterFailures: number[] = [];

    it(
      "measures N=8 wall-clock runs with and without the emitter; logs raw numbers + delta",
      async () => {
        const startedAt = Date.now();

        // Warm-up: one discarded run before each condition so first-call
        // JIT / DNS / model-route caching doesn't bias the first sample.
        // Warm-up is performed but its time is not recorded. Warm-up
        // failures are also swallowed (they don't bias the sample).
        // eslint-disable-next-line no-console
        console.log(
          "[pi-emitter-latency-smoke] warm-up: 1 run without emitter (discarded)",
        );
        await runOneSafe({ withEmitter: false, warmup: true });

        // ── Baseline (no emitter) ──
        // eslint-disable-next-line no-console
        console.log(
          `[pi-emitter-latency-smoke] baseline: ${N_RUNS} runs WITHOUT emitter`,
        );
        for (let i = 0; i < N_RUNS; i += 1) {
          if (Date.now() - startedAt > TOTAL_BUDGET_MS) {
            // eslint-disable-next-line no-console
            console.log(
              `[pi-emitter-latency-smoke] ABORTING baseline at i=${i}: total budget ${TOTAL_BUDGET_MS}ms exceeded`,
            );
            break;
          }
          const result = await runOneSafe({ withEmitter: false });
          if (result.kind === "ok") {
            baselineMs.push(result.ms);
            // eslint-disable-next-line no-console
            console.log(
              `[pi-emitter-latency-smoke]   baseline ${i + 1}/${N_RUNS}: ${result.ms.toFixed(0)} ms`,
            );
          } else {
            baselineFailures.push(i + 1);
            // eslint-disable-next-line no-console
            console.log(
              `[pi-emitter-latency-smoke]   baseline ${i + 1}/${N_RUNS}: FAILED (${result.reason}) — sample skipped`,
            );
          }
        }

        // Warm-up before the with-emitter condition.
        // eslint-disable-next-line no-console
        console.log(
          "[pi-emitter-latency-smoke] warm-up: 1 run with emitter (discarded)",
        );
        await runOneSafe({ withEmitter: true, warmup: true });

        // ── With emitter (live listener) ──
        // eslint-disable-next-line no-console
        console.log(
          `[pi-emitter-latency-smoke] with-emitter: ${N_RUNS} runs WITH emitter`,
        );
        for (let i = 0; i < N_RUNS; i += 1) {
          if (Date.now() - startedAt > TOTAL_BUDGET_MS) {
            // eslint-disable-next-line no-console
            console.log(
              `[pi-emitter-latency-smoke] ABORTING with-emitter at i=${i}: total budget ${TOTAL_BUDGET_MS}ms exceeded`,
            );
            break;
          }
          const result = await runOneSafe({ withEmitter: true });
          if (result.kind === "ok") {
            withEmitterMs.push(result.ms);
            // eslint-disable-next-line no-console
            console.log(
              `[pi-emitter-latency-smoke]   with-emitter ${i + 1}/${N_RUNS}: ${result.ms.toFixed(0)} ms`,
            );
          } else {
            withEmitterFailures.push(i + 1);
            // eslint-disable-next-line no-console
            console.log(
              `[pi-emitter-latency-smoke]   with-emitter ${i + 1}/${N_RUNS}: FAILED (${result.reason}) — sample skipped`,
            );
          }
        }

        // ── Compute and report ──
        // eslint-disable-next-line no-console
        console.log(
          "\n[pi-emitter-latency-smoke] ── RESULTS ─────────────────────────────────",
        );
        reportStats("baseline (no emitter)", baselineMs);
        reportStats("with emitter       ", withEmitterMs);
        // eslint-disable-next-line no-console
        console.log(
          `[pi-emitter-latency-smoke] baseline failures: ${baselineFailures.length} (samples ${baselineFailures.join(", ") || "none"})`,
        );
        // eslint-disable-next-line no-console
        console.log(
          `[pi-emitter-latency-smoke] with-emitter failures: ${withEmitterFailures.length} (samples ${withEmitterFailures.join(", ") || "none"})`,
        );

        // ── Hard assertions ──
        // Both conditions must have produced enough successful runs to
        // draw any meaningful comparison. This is the ONLY hard
        // failure path; one slow individual sample is NOT.
        if (
          baselineMs.length < MIN_SUCCESSES_PER_CONDITION ||
          withEmitterMs.length < MIN_SUCCESSES_PER_CONDITION
        ) {
          throw new Error(
            `[pi-emitter-latency-smoke] insufficient successful samples to compare: ` +
              `baseline=${baselineMs.length}/${N_RUNS}, ` +
              `with-emitter=${withEmitterMs.length}/${N_RUNS} ` +
              `(need >= ${MIN_SUCCESSES_PER_CONDITION} per condition). ` +
              `This usually means a systemic provider outage, not flaky network — ` +
              `inspect the per-sample failure log above.`,
          );
        }

        // Compute p95 (which with N=8 is really just the maximum).
        const baseP95 = percentile(baselineMs, 0.95);
        const withP95 = percentile(withEmitterMs, 0.95);
        const baseStats = computeStats(baselineMs);
        const withStats = computeStats(withEmitterMs);

        // Divide-by-zero guards: a 0 baseline (impossible if we got
        // here — every sample is >= the timeout, which is > 0) is
        // checked defensively. Skip the delta lines if either side is
        // empty/zero rather than reporting NaN/Infinity.
        let deltaP95Pct: number | null = null;
        let deltaMeanPct: number | null = null;
        if (baseP95 > 0 && baseStats.mean > 0) {
          deltaP95Pct = ((withP95 - baseP95) / baseP95) * 100;
          deltaMeanPct =
            ((withStats.mean - baseStats.mean) / baseStats.mean) * 100;
        }

        const fmtPct = (v: number | null): string =>
          v === null ? "n/a" : `${v >= 0 ? "+" : ""}${v.toFixed(2)}%`;

        // eslint-disable-next-line no-console
        console.log(
          `[pi-emitter-latency-smoke] baseline p95  = ${baseP95.toFixed(0)} ms  (max of ${baselineMs.length} samples)`,
        );
        // eslint-disable-next-line no-console
        console.log(
          `[pi-emitter-latency-smoke] with-em p95   = ${withP95.toFixed(0)} ms  (max of ${withEmitterMs.length} samples)`,
        );
        // eslint-disable-next-line no-console
        console.log(
          `[pi-emitter-latency-smoke] p95 delta     = ${fmtPct(deltaP95Pct)}`,
        );
        // eslint-disable-next-line no-console
        console.log(
          `[pi-emitter-latency-smoke] mean delta    = ${fmtPct(deltaMeanPct)}`,
        );

        const inBudget =
          deltaP95Pct !== null && deltaP95Pct <= 2.0;
        // eslint-disable-next-line no-console
        console.log(
          `[pi-emitter-latency-smoke] budget (2%): ${inBudget ? "INSIDE" : "OUTSIDE / n/a"} (informational, not asserted)`,
        );

        // Write an artifact under the swarm dir (NOT the repo) so the
        // conductor can read the raw numbers without the test
        // log being a required input.
        const artifactDir =
          process.env["PI_SESSION_DIR"] ??
          join(WORKTREE_ROOT, ".clanq", "swarm", "issue-20-pi-emitter");
        try {
          const artifact = join(artifactDir, "pi-emitter-latency-smoke.txt");
          const lines: string[] = [
            `baseline_ms = ${baselineMs.map((n) => n.toFixed(0)).join(", ")}`,
            `with_emitter_ms = ${withEmitterMs.map((n) => n.toFixed(0)).join(", ")}`,
            `baseline_failed_samples = ${baselineFailures.join(", ") || "(none)"}`,
            `with_emitter_failed_samples = ${withEmitterFailures.join(", ") || "(none)"}`,
            `baseline_p95_ms = ${baseP95.toFixed(0)}`,
            `with_emitter_p95_ms = ${withP95.toFixed(0)}`,
            `delta_p95_pct = ${deltaP95Pct === null ? "n/a" : deltaP95Pct.toFixed(2)}`,
            `delta_mean_pct = ${deltaMeanPct === null ? "n/a" : deltaMeanPct.toFixed(2)}`,
            `in_budget_2pct = ${inBudget ? "true" : "false"}`,
          ];
          writeFileSync(artifact, lines.join("\n") + "\n", "utf8");
          // eslint-disable-next-line no-console
          console.log(
            `[pi-emitter-latency-smoke] artifact: ${artifact}`,
          );
        } catch (e) {
          // Don't fail the test on artifact write errors.
          // eslint-disable-next-line no-console
          console.log(
            `[pi-emitter-latency-smoke] (could not write artifact: ${String(e)})`,
          );
        }

        // Intentionally no hard assertion on the latency delta here — see the file header's flakiness policy. The raw numbers and delta are already reported above via console.log and the written artifact file; this test's pass/fail depends solely on the MIN_SUCCESSES_PER_CONDITION guard above.
      },
      TOTAL_BUDGET_MS + 30_000,
    );
  },
);

/** Outcome of one `pi` invocation. */
type SampleResult =
  | { kind: "ok"; ms: number }
  | { kind: "fail"; reason: string };

/**
 * Wrapper around `runOne` that NEVER throws. Converts any thrown
 * error (timeout, spawn error, non-zero exit if we want to surface it)
 * into a `SampleResult`. This is the per-sample isolation layer that
 * prevents one slow / failed network call from killing the whole test.
 *
 * Warm-up runs discard failures entirely (warm-up only primes caches;
 * its outcome is irrelevant to the recorded samples).
 */
async function runOneSafe(opts: {
  withEmitter: boolean;
  warmup?: boolean;
}): Promise<SampleResult> {
  try {
    const ms = await runOne(opts);
    return { kind: "ok", ms };
  } catch (e) {
    if (opts.warmup) {
      // eslint-disable-next-line no-console
      console.log(
        `[pi-emitter-latency-smoke]   warm-up ${opts.withEmitter ? "with" : "without"} emitter: FAILED (${String(e)}) — warm-up discarded`,
      );
      // Return a synthetic 0-ok so the caller doesn't have to special-
      // case warm-ups; the result is never recorded.
      return { kind: "ok", ms: 0 };
    }
    return { kind: "fail", reason: String(e) };
  }
}

/** Run one `pi` subprocess and return its wall-clock duration in ms.
 *  Kills the child on timeout. Always cleans up its scratch dir and
 *  listener. THROWS on timeout, spawn error, or non-zero exit so the
 *  caller (`runOneSafe`) can convert it to a recorded failure. */
async function runOne(opts: {
  withEmitter: boolean;
  warmup?: boolean;
}): Promise<number> {
  void opts.warmup; // accepted but not used internally
  const scratch = mkdtempSync(join(tmpdir(), "pi-latency-"));
  const sessionDir = join(scratch, "session");
  const logPath = join(scratch, "emitter.log");

  let listener: HookSocketListener | null = null;
  if (opts.withEmitter) {
    listener = await HookSocketListener.create({ prefix: "pi-latency-" });
  }

  const env: Record<string, string> = {
    ...(process.env as Record<string, string>),
  };
  if (opts.withEmitter && listener) {
    env["GRAVITY_HOOK_SOCK"] = listener.getSocketPath();
    env["GRAVITY_PI_EMITTER_LOG"] = logPath;
  }

  const argv: string[] = [
    "-p",
    "--mode",
    "json",
    "--session-dir",
    sessionDir,
    "-n",
    "lat",
    SMOKE_PROMPT,
  ];
  if (opts.withEmitter) {
    // Insert -e after "-p --mode json" so it lives among the per-run
    // session config. Order doesn't matter to pi (parsed by name), but
    // keeping the production `pi-wp`-style ordering aids greppability.
    argv.splice(3, 0, "-e", EMITTER_PATH);
  }

  let child: ChildProcess;
  try {
    child = spawn("pi", argv, {
      cwd: scratch,
      env,
      stdio: ["ignore", "pipe", "pipe"],
    });
  } catch (e) {
    if (listener) await listener.close();
    try {
      rmSync(scratch, { recursive: true, force: true });
    } catch {
      /* ignore */
    }
    throw e;
  }

  // Drain stdout/stderr to keep the child's pipe buffer from blocking.
  child.stdout?.resume();
  child.stderr?.resume();

  const startMs = Date.now();
  let timer: NodeJS.Timeout | null = null;
  try {
    const exit = await new Promise<{ code: number | null }>(
      (resolve, reject) => {
        timer = setTimeout(() => {
          try {
            child.kill("SIGKILL");
          } catch {
            /* ignore */
          }
          reject(
            new Error(
              `pi subprocess did not exit within ${PI_SUBPROCESS_TIMEOUT_MS} ms (SIGKILLed)`,
            ),
          );
        }, PI_SUBPROCESS_TIMEOUT_MS);
        timer.unref?.();
        child.once("exit", (code) => resolve({ code: code ?? null }));
        child.once("error", (err) => reject(err));
      },
    );
    const elapsedMs = Date.now() - startMs;
    if (exit.code !== 0) {
      throw new Error(`pi exited with non-zero code ${exit.code}`);
    }
    if (opts.withEmitter && listener) {
      // Sanity: at least one envelope arrived on the listener. If
      // zero arrived, the emitter is suspiciously silent. We treat
      // this as a functional failure for the with-emitter condition
      // specifically — it means the wire path is broken even if `pi`
      // itself completed.
      const count = listener.getPiEnvelopes().length;
      if (count === 0) {
        throw new Error(
          `with-emitter run produced zero envelopes (elapsed=${elapsedMs}ms)`,
        );
      }
    }
    return elapsedMs;
  } finally {
    if (timer !== null) clearTimeout(timer);
    try {
      if (child.exitCode === null && !child.killed) {
        child.kill("SIGKILL");
      }
    } catch {
      /* ignore */
    }
    if (listener) await listener.close();
    try {
      rmSync(scratch, { recursive: true, force: true });
    } catch {
      /* ignore */
    }
  }
}

/** Append a one-line stats summary to the test log. */
function reportStats(label: string, samples: number[]): void {
  if (samples.length === 0) {
    // eslint-disable-next-line no-console
    console.log(`[pi-emitter-latency-smoke] ${label}: (no samples)`);
    return;
  }
  const stats = computeStats(samples);
  // eslint-disable-next-line no-console
  console.log(
    `[pi-emitter-latency-smoke] ${label}: n=${stats.n} mean=${stats.mean.toFixed(0)} ms median=${stats.median.toFixed(0)} ms min=${stats.min.toFixed(0)} max=${stats.max.toFixed(0)}`,
  );
}

interface BasicStats {
  n: number;
  mean: number;
  median: number;
  min: number;
  max: number;
}

function computeStats(samples: number[]): BasicStats {
  if (samples.length === 0) {
    return { n: 0, mean: 0, median: 0, min: 0, max: 0 };
  }
  const sorted = [...samples].sort((a, b) => a - b);
  const sum = sorted.reduce((acc, v) => acc + v, 0);
  const mid = Math.floor(sorted.length / 2);
  const median =
    sorted.length % 2 === 0
      ? (sorted[mid - 1] + sorted[mid]) / 2
      : sorted[mid];
  return {
    n: sorted.length,
    mean: sum / sorted.length,
    median,
    min: sorted[0],
    max: sorted[sorted.length - 1],
  };
}

/** Linear-interpolation percentile (for general N — with N=8 the
 *  result is approximately the maximum). */
function percentile(samples: number[], p: number): number {
  if (samples.length === 0) return 0;
  const sorted = [...samples].sort((a, b) => a - b);
  if (sorted.length === 1) return sorted[0];
  const rank = p * (sorted.length - 1);
  const lo = Math.floor(rank);
  const hi = Math.ceil(rank);
  if (lo === hi) return sorted[lo];
  const w = rank - lo;
  return sorted[lo] * (1 - w) + sorted[hi] * w;
}