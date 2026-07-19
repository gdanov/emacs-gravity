// dist-bundle-imports.test.ts — Guards the dist bundle's bare
// `import "X"` / `import ... from "X"` specifiers so they stay Node
// built-ins (or relative) only. Catches the regression where a
// first-party dependency (historically `ws`) was added to esbuild's
// `external` array and shipped as a bare import — that breaks the
// distributed plugin at install sites that have no `node_modules`,
// because Node can't resolve `ws` and the entire server crashes at
// import time, before any `GRAVITY_GATEWAY_DISABLE` config check even
// runs. Pattern: read the dist file, regex out the import specifiers,
// filter to bare ones (not relative, not absolute, not node:-prefixed),
// then assert every remaining bare specifier is a Node built-in.

import { describe, expect, it } from "vitest";
import { readFileSync, existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { isBuiltin } from "node:module";

const HERE = dirname(fileURLToPath(import.meta.url));
const DIST_PATH = join(HERE, "..", "dist", "gravity-server.mjs");

// Match top-level static imports: `import ... from "spec"` or
// `import "spec"`. Captures the specifier in group 1.
const IMPORT_RE = /^\s*import\s+(?:type\s+)?(?:[\s\S]*?from\s+)?["']([^"']+)["']\s*;?\s*$/gm;

interface SpecCheck {
  spec: string;
  line: number;
}

function extractBareImports(source: string): SpecCheck[] {
  const out: SpecCheck[] = [];
  let match: RegExpExecArray | null;
  while ((match = IMPORT_RE.exec(source)) !== null) {
    const spec = match[1];
    if (spec.startsWith(".")) continue;          // relative
    if (spec.startsWith("/")) continue;          // absolute
    if (spec.startsWith("node:")) continue;      // explicit node: prefix
    // line number is approximate (1-based)
    const upto = source.slice(0, match.index);
    const line = upto.split("\n").length;
    out.push({ spec, line });
  }
  return out;
}

describe("dist/gravity-server.mjs bare imports", () => {
  it("exists (run `make build` first)", () => {
    if (!existsSync(DIST_PATH)) {
      throw new Error(
        `dist bundle missing at ${DIST_PATH} — run \`make build\` (or \`node build.mjs\` in packages/gravity-server) first.`,
      );
    }
  });

  it("contains no bare imports of non-Node-builtin modules", () => {
    if (!existsSync(DIST_PATH)) {
      throw new Error(
        `dist bundle missing at ${DIST_PATH} — run \`make build\` first.`,
      );
    }
    const source = readFileSync(DIST_PATH, "utf8");
    const bare = extractBareImports(source);

    // Sanity: at least one bare import is expected (e.g. `ws`, `net`).
    // If this ever goes to zero the bundle may have been rewritten as
    // a single IIFE — flag it for review rather than silently passing.
    expect(bare.length).toBeGreaterThan(0);

    const offenders: string[] = [];
    for (const { spec } of bare) {
      if (!isBuiltin(spec)) {
        offenders.push(spec);
      }
    }

    if (offenders.length > 0) {
      throw new Error(
        `dist/gravity-server.mjs has bare import(s) that are not Node built-ins: ${offenders.join(", ")}. ` +
          `These will ERR_MODULE_NOT_FOUND at runtime on installed plugin sites with no node_modules. ` +
          `Either inline them via esbuild (remove the spec from the build.mjs \`external\` array) or import via \`node:\` / relative path.`,
      );
    }
  });
});