// emitter-bundle.ts — Test helper that resolves the WP1 pi extension's
// bundled JS source for use in tests.
//
// Tries the in-tree generated bundle (`src/pi-driver/emitter-bundle.generated.ts`,
// produced by `make build-server`) first; on any import failure (file
// missing, build not yet run, etc.), falls back to running esbuild
// in-process with the same config as `build.mjs` pass 1.
//
// Never throws on the import path — failures there transparently fall
// through to esbuild. Throws only if the esbuild fallback also yields
// no output (an unrecoverable build error).

import * as esbuild from "esbuild";
import { fileURLToPath } from "node:url";
import { dirname, join, resolve } from "node:path";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
/** Absolute path to `packages/gravity-server/`. */
const SERVER_ROOT = resolve(__dirname, "..", "..");

/**
 * Resolve the bundled JS source of the WP1 pi extension.
 *
 * First attempts to import the generated bundle (relative to this
 * helper's parent directory). On any import failure — file missing,
 * build not yet run, parse error — falls back to running esbuild
 * in-process with the same config `build.mjs` uses for its pass 1
 * (`entryPoints: ["src/pi-driver/emitter/index.ts"]` resolved relative
 * to `packages/gravity-server/`, `bundle: true, platform: "node",
 * target: "node18", format: "esm", write: false`) and returns
 * `outputFiles[0].text`.
 *
 * Returns the bundled JS as a string. Never throws on the import
 * path; only an esbuild failure that produces zero output files
 * becomes an Error.
 */
export async function resolveEmitterBundleSource(): Promise<string> {
  // Fast path: the build.mjs-generated TS file. Importing the
  // .js-extension specifier lets vitest's TS resolver pick up the
  // .ts source transparently. Any failure here falls through.
  try {
    const mod = await import(
      "../../src/pi-driver/emitter-bundle.generated.js"
    );
    const source = (mod as { PI_EMITTER_SOURCE?: unknown }).PI_EMITTER_SOURCE;
    if (typeof source === "string" && source.length > 0) {
      return source;
    }
  } catch {
    // Fall through to the esbuild fallback.
  }

  // Fallback path: bundle the emitter entry in-process with esbuild.
  // Matches build.mjs pass 1 line-for-line so the output bytes are
  // equivalent regardless of which path produced them.
  const result = await esbuild.build({
    entryPoints: [join(SERVER_ROOT, "src", "pi-driver", "emitter", "index.ts")],
    bundle: true,
    platform: "node",
    target: "node18",
    format: "esm",
    write: false,
    absWorkingDir: SERVER_ROOT,
  });
  const files = result.outputFiles;
  if (!files || files.length === 0) {
    throw new Error(
      "resolveEmitterBundleSource: esbuild fallback produced no outputFiles",
    );
  }
  return files[0].text;
}