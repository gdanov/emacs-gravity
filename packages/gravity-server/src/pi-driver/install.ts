// install.ts — Atomically install the bundled pi extension into a
// directory on disk.
//
// Per the frozen contract in plan.md §3:
//
//   mkdir(installDir, { recursive: true }) first. Read existing target
//   file if present (tolerate ENOENT -> treat as "no existing file").
//   Compare byte-for-byte to `source`. Identical -> return
//   { action: "current", path } WITHOUT touching the file (no mtime
//   change, no temp file, no rename). Different or missing -> write a
//   temp file in the SAME directory as the target (name it
//   ".tmp-<process.pid>-<random suffix>") then rename() it onto the
//   final path (same-filesystem atomic rename). Return
//   { action: existed-before ? "refreshed" : "installed", path }.
//
// The function MUST NEVER throw — any caught error becomes
// { action: "failed", path, error: String(e) }. The caller (the
// server's startup sequence) decides whether/how to log.
//
// All paths are taken from caller-supplied options; nothing here
// reaches into the real ~/.pi/ or any other user-global location
// unless the caller passes such a path. Tests always inject a /tmp
// directory.

import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { randomBytes } from "node:crypto";

/** Default filename used inside `installDir`. */
export const DEFAULT_FILENAME = "gravity-pi.js";

export interface InstallEmitterOptions {
  /** Absolute directory in which to install the extension file. */
  readonly installDir: string;
  /** Exact source text of the extension (the bundled JS). */
  readonly source: string;
  /**
   * Filename to use inside `installDir`. Defaults to "gravity-pi.js".
   * The same filename is used both for the existing-file lookup and
   * for the final installed path.
   */
  readonly filename?: string;
}

export type InstallEmitterAction =
  | "installed"
  | "refreshed"
  | "current"
  | "failed";

export interface InstallEmitterResult {
  /**
   * - "installed": target did not exist before this call; we wrote a
   *   fresh file and renamed it into place.
   * - "refreshed": target existed but did not byte-equal `source`; we
   *   atomically replaced it.
   * - "current": target already byte-equals `source`; no filesystem
   *   modification occurred (no temp file, no rename, no mtime change).
   * - "failed": an error occurred. The target file is left in its
   *   prior state (or absent). `error` carries the message.
   */
  readonly action: InstallEmitterAction;
  /**
   * The full path that would be (or is) the installed file. Equals
   * `join(installDir, filename)` where filename defaults to
   * "gravity-pi.js".
   */
  readonly path: string;
  /** Present only on action: "failed". */
  readonly error?: string;
}

/**
 * Install the bundled pi extension into `installDir`/`filename`,
 * atomically, never throwing.
 *
 * Algorithm:
 *   1. mkdir(installDir, { recursive: true }).
 *   2. Compute target = join(installDir, filename).
 *   3. readFile(target) — tolerate ENOENT (treat as "no existing
 *      file"); any other read error is caught below and converted to
 *      { action: "failed", ... }.
 *   4. If the existing contents byte-equal `source`, return
 *      { action: "current", path } with no further filesystem
 *      activity.
 *   5. Otherwise write a temp file `.tmp-<pid>-<random>` in the SAME
 *      directory (so rename is same-filesystem atomic) and rename()
 *      it onto the target.
 *   6. Any thrown error from steps 1-5 is caught and returned as
 *      { action: "failed", path, error: String(e) }.
 */
export async function installEmitter(
  options: InstallEmitterOptions,
): Promise<InstallEmitterResult> {
  const filename = options.filename ?? DEFAULT_FILENAME;
  const targetPath = join(options.installDir, filename);

  try {
    await mkdir(options.installDir, { recursive: true });

    // Read existing target. ENOENT means "no existing file"; any
    // other error (EACCES, EISDIR, ...) propagates to the outer
    // catch and becomes { action: "failed", ... }.
    let existing: Buffer | null = null;
    try {
      existing = await readFile(targetPath);
    } catch (e) {
      const code = (e as NodeJS.ErrnoException).code;
      if (code !== "ENOENT") throw e;
      existing = null;
    }

    if (existing !== null) {
      // Compare byte-for-byte against the UTF-8 encoding of `source`.
      const sourceBytes = Buffer.from(options.source, "utf8");
      if (existing.equals(sourceBytes)) {
        return { action: "current", path: targetPath };
      }
    }

    // Different (or absent) → atomic replace via same-dir temp file.
    // The temp-file basename includes pid + random hex so concurrent
    // or repeated installs in the same directory never collide.
    const tmpName = `.tmp-${process.pid}-${randomBytes(8).toString("hex")}`;
    const tmpPath = join(options.installDir, tmpName);
    await writeFile(tmpPath, options.source, "utf8");
    await rename(tmpPath, targetPath);

    return {
      action: existing === null ? "installed" : "refreshed",
      path: targetPath,
    };
  } catch (e) {
    return {
      action: "failed",
      path: targetPath,
      error: e instanceof Error ? e.message : String(e),
    };
  }
}