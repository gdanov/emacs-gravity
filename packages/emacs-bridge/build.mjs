import * as esbuild from "esbuild";

await esbuild.build({
  entryPoints: ["src/index.ts"],
  bundle: true,
  platform: "node",
  target: "node18",
  format: "esm",
  outfile: "dist/index.mjs",
  external: [
    // Node builtins
    "net", "fs", "path", "child_process", "os", "crypto", "stream", "util", "events",
    // Heavy optional deps not needed for one-shot bridge
    "@anthropic-ai/claude-agent-sdk",
    "@mariozechner/pi-agent-core",
    "@mariozechner/pi-ai",
    "@mariozechner/pi-coding-agent",
    "bonjour-service",
  ],
  banner: {
    // ESM shim for __dirname and require.main
    js: `
import { createRequire as __createRequire } from "module";
import { fileURLToPath as __fileURLToPath } from "url";
import { dirname as __dirnameFn } from "path";
const require = __createRequire(import.meta.url);
const __filename = __fileURLToPath(import.meta.url);
const __dirname = __dirnameFn(__filename);
`,
  },
});

console.log("Built dist/index.mjs");
