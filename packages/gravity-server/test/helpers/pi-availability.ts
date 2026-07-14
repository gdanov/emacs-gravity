// pi-availability.ts — Probe whether a real `pi` binary is reachable
// AND can actually authenticate (i.e. list models) within a short
// timeout.
//
// Used by the WP3 integration/smoke test files as the gate for
// `describe.skipIf(!available)` so the suites stay present-and-inert on
// hosts without `pi`, and actually exercise the WP1/WP2 emitter against
// a real subprocess on hosts (like this one) where `pi` is on PATH.
//
// Hard rules enforced here:
//  - NEVER throws. Any error path → returns false.
//  - Short timeout (~5s) so a hung `pi` cannot stall the test runner.
//  - Uses `execFileSync` (no shell), passing argv directly — eliminates
//    shell-injection-style worries and matches the production-safety
//    pattern in `pi-wp` (which spawns `pi` with explicit argv).
//  - The probe MUST specifically validate "can this host actually run a
//    `pi` session end-to-end", NOT merely "does a `pi` binary exist on
//    PATH" or even "`pi --list-models` exits 0". Two failure modes
//    here matter:
//      1. `--version` only proves the binary exists — a host with `pi`
//         installed but no provider auth (a realistic CI / fresh-loop-
//         host scenario) would pass `--version` yet fail every real
//         `pi -p --mode json` run with an auth error.
//      2. `pi --list-models` exits 0 even when no provider is
//         configured — it just prints "No models available. Use /login
//         to log into a provider via OAuth or API key." to stdout. So
//         a successful exit code alone is insufficient; we must
//         inspect the output for actual model rows.
//    Using either as a sole gate would cause `piAvailable()` to
//    return true on a no-auth host and the real-run suite would then
//    attempt real subprocess invocations that fail for an unrelated
//    reason (missing provider key), producing real test failures
//    instead of a clean skip. The correct signal is "exit 0 AND stdout
//    contains actual model data".

import { execFileSync } from "node:child_process";

/** Probe timeout in ms. */
export const PI_AVAILABILITY_TIMEOUT_MS = 5_000;

/**
 * The exact phrase `pi --list-models` prints to stdout when no
 * provider is configured / no API key is loaded. We treat its
 * presence (anywhere in stdout, case-sensitive) as the "no models
 * available" signal — i.e. the binary is reachable but the host
 * cannot actually run a `pi` session. Verified directly against
 * pi 0.80.6.
 */
const NO_MODELS_AVAILABLE_PHRASE = "No models available";

/**
 * Resolve to true iff a `pi` binary is on PATH AND can actually
 * authenticate: `pi --list-models` exits 0 within
 * `PI_AVAILABILITY_TIMEOUT_MS` AND its stdout contains at least one
 * model row (i.e. does NOT contain the "No models available" phrase
 * that pi emits when no provider is configured).
 *
 * Never throws. Any failure path (ENOENT, EACCES, ETIMEDOUT, non-zero
 * exit, spawn error, no provider key) returns false.
 */
export async function piAvailable(): Promise<boolean> {
  let stdout: string;
  try {
    const buf = execFileSync("pi", ["--list-models"], {
      stdio: ["ignore", "pipe", "pipe"],
      timeout: PI_AVAILABILITY_TIMEOUT_MS,
      // Suppress the "Killed: ..." message Node prints on timeout.
      killSignal: "SIGKILL",
      // Give the binary room to format its table on stdout. 1 MiB is
      // wildly generous for the header + a few model rows; if pi ever
      // needs more the probe itself is the wrong shape.
      maxBuffer: 1024 * 1024,
    });
    stdout = buf.toString("utf8");
  } catch {
    // ENOENT, EACCES, timeout (SIGKILL), non-zero exit, spawn error →
    // all return false.
    return false;
  }
  // Distinguish "pi enumerated models" (real auth) from "pi reports
  // no provider configured" (the no-auth host scenario the suite
  // must skip on). The phrase is part of pi's own user-facing output
  // and has been stable across the 0.58.x → 0.80.6 line.
  if (stdout.includes(NO_MODELS_AVAILABLE_PHRASE)) return false;
  // Belt-and-suspenders: a successful probe also needs SOME non-empty
  // output (pi's table starts with "provider model ..." header). If
  // stdout is empty/whitespace-only we treat that as a non-result
  // too.
  if (stdout.trim().length === 0) return false;
  return true;
}