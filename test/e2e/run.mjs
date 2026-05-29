// E2E scenario runner / orchestrator.
//
// Brings up the FULL stack in an isolated run dir and drives gravity
// features end-to-end:
//
//   gravity-server (tsx)  <--unix-->  proxy (spy)  <--unix-->  emacs --daemon
//        ^ hook socket                  | jsonl log              (real client:
//        |                              v                         filter,
//   inject.mjs (synthetic hooks)   asserted by runner             timers,
//                                                                 render)
//
// Host- and Docker-runnable. Needs AF_UNIX bind (sandbox-disabled / CI).
//
// Usage: node run.mjs [scenarioNameFilter]

import { spawn, spawnSync } from "child_process";
import { mkdirSync, rmSync, existsSync, readFileSync, writeFileSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { scenarios } from "./scenarios.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = join(HERE, "..", "..");
const RUN = `/tmp/gravity-e2e-${process.pid}`;
const SRV_TERMINAL = join(RUN, "srv-terminal.sock");
const HOOK = join(RUN, "hooks.sock");
const EMACS_TERMINAL = join(RUN, "terminal.sock"); // proxy listens here
const PROXY_LOG = join(RUN, "proxy.jsonl");
const PIDFILE = join(RUN, "server.pid");
const PIWD = join(RUN, "piwd");
const PI_TRIGGER = join(RUN, "pi.trigger");
const PI_RESULT = join(RUN, "pi.result");
const FAKE_PI = join(HERE, "fake-pi.mjs");
const DAEMON = `gravity-e2e-${process.pid}`;

const procs = [];
let failures = 0;
const log = (m) => process.stderr.write(`[e2e] ${m}\n`);

function sh(cmd, args, opts = {}) {
  return spawnSync(cmd, args, { encoding: "utf8", ...opts });
}
function bg(cmd, args, env) {
  const p = spawn(cmd, args, {
    env: { ...process.env, ...env },
    stdio: ["ignore", "pipe", "pipe"],
  });
  procs.push(p);
  p.stderr.on("data", (d) => process.stderr.write(`  <${cmd}> ${d}`));
  return p;
}
async function waitFor(pred, timeoutMs, what) {
  const t0 = Date.now();
  while (Date.now() - t0 < timeoutMs) {
    if (pred()) return;
    await new Promise((r) => setTimeout(r, 100));
  }
  throw new Error(`timeout waiting for ${what}`);
}

// emacsclient --eval; returns trimmed stdout ("t" / "nil" / value).
function emacs(form) {
  const r = sh("emacsclient", ["-s", DAEMON, "--eval", form]);
  if (r.status !== 0) {
    throw new Error(`emacsclient failed: ${form}\n${r.stderr || r.stdout}`);
  }
  return (r.stdout || "").trim();
}
const emacsT = (form) => emacs(form) === "t";

function readJsonl(path) {
  if (!existsSync(path)) return [];
  return readFileSync(path, "utf8")
    .split("\n")
    .filter(Boolean)
    .map((l) => JSON.parse(l));
}

function assert(cond, msg) {
  if (cond) { log(`  ✓ ${msg}`); }
  else { failures++; log(`  ✗ FAIL: ${msg}`); }
}

async function setup() {
  if (existsSync(RUN)) rmSync(RUN, { recursive: true, force: true });
  mkdirSync(RUN, { recursive: true });
  mkdirSync(PIWD, { recursive: true });
  writeFileSync(PROXY_LOG, "");

  // `--pi` auto-starts a pi session at boot; PI_BINARY_PATH points the
  // pi-driver at our fake pi (no LLM). The hook socket still serves the
  // Claude-path scenarios — the extra pi session is inert until triggered.
  // PI_E2E_* are read by fake-pi (inherits the server's env).
  log("starting gravity-server (--pi → fake-pi)…");
  bg("npx", ["tsx", "packages/gravity-server/src/gravity-server.ts",
    "--pi", "--pi-cwd", PIWD], {
    GRAVITY_TERMINAL_SOCK: SRV_TERMINAL,
    GRAVITY_HOOK_SOCK: HOOK,
    GRAVITY_PID_FILE: PIDFILE,
    PI_BINARY_PATH: FAKE_PI,
    PI_E2E_TRIGGER: PI_TRIGGER,
    PI_E2E_RESULT: PI_RESULT,
  });
  await waitFor(() => existsSync(SRV_TERMINAL) && existsSync(HOOK),
    20000, "server sockets");

  log("starting proxy…");
  bg("node", [join(HERE, "proxy.mjs"), EMACS_TERMINAL, SRV_TERMINAL, PROXY_LOG]);
  await waitFor(() => existsSync(EMACS_TERMINAL), 10000, "proxy socket");

  log("starting emacs --daemon…");
  // Env must be set in the daemon's process env: claude-gravity reads the
  // socket path into a defvar AT LOAD time. Emacs → proxy (not real server).
  const r = sh("emacs", [
    `--daemon=${DAEMON}`, "-Q",
    "--eval", "(package-initialize)",
    "-L", REPO, "-L", HERE,
    "-l", join(HERE, "driver.el"),
  ], {
    env: {
      ...process.env,
      GRAVITY_TERMINAL_SOCK: EMACS_TERMINAL,
      GRAVITY_HOOK_SOCK: HOOK,
      GRAVITY_PID_FILE: PIDFILE,
    },
  });
  if (r.status !== 0) throw new Error(`emacs daemon failed:\n${r.stderr}`);
  await waitFor(() => {
    const c = sh("emacsclient", ["-s", DAEMON, "--eval", "(+ 1 1)"]);
    return c.status === 0 && c.stdout.trim() === "2";
  }, 15000, "emacs daemon");

  assert(emacsT("(cge-connect)"), "emacs client connected to proxy");
  emacs("(cge-pump 0.5)");
}

function teardown() {
  try { sh("emacsclient", ["-s", DAEMON, "--eval", "(kill-emacs)"]); } catch {}
  for (const p of procs) { try { p.kill("SIGTERM"); } catch {} }
  try { rmSync(RUN, { recursive: true, force: true }); } catch {}
}

async function main() {
  const filter = process.argv[2];
  await setup();
  const ctx = {
    RUN, HOOK, PROXY_LOG,
    emacs, emacsT, assert,
    piTrigger: () => writeFileSync(PI_TRIGGER, "go"),
    piResult: () => (existsSync(PI_RESULT)
      ? JSON.parse(readFileSync(PI_RESULT, "utf8")) : null),
    proxyMessages: () => readJsonl(PROXY_LOG),
    dump: (buffer) => {
      const f = join(RUN, "dump.txt");
      emacs(`(cge-dump ${JSON.stringify(buffer)} ${JSON.stringify(f)})`);
      return existsSync(f) ? readFileSync(f, "utf8") : "";
    },
  };
  for (const sc of scenarios) {
    if (filter && !sc.name.includes(filter)) continue;
    log(`── scenario: ${sc.name} ──`);
    try {
      await sc.run(ctx);
    } catch (e) {
      failures++;
      log(`  ✗ FAIL (threw): ${e.message}`);
    }
  }
}

main()
  .then(() => { teardown(); log(failures ? `FAILED (${failures})` : "ALL PASSED"); process.exit(failures ? 1 : 0); })
  .catch((e) => { log(`fatal: ${e.stack || e.message}`); teardown(); process.exit(2); });
