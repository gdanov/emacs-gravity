import * as esbuild from "esbuild";

await esbuild.build({
  entryPoints: ["src/index.ts"],
  bundle: true,
  platform: "node",
  target: "node18",
  format: "esm",
  outfile: "dist/emacs-bridge.mjs",
  external: [
    "net", "fs", "path", "child_process", "os", "crypto", "stream", "util", "events",
  ],
  banner: {
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

console.log("Built dist/emacs-bridge.mjs");
