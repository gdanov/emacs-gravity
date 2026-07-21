#!/usr/bin/env node
// stub-gravity-server.mjs — minimal stand-in for gravity-server.mjs used
// only by ensure-server-version-marker.test.ts. Mirrors exactly the two
// behaviors _ensure-server/_spawn-server depend on: write-marker-before-
// listen, and an async (non-instant) SIGTERM shutdown that unlinks its
// socket — so the "old fully exits before new socket bound" assertion is
// exercised against real timing, not an idealized instant exit.
import { createServer } from "net";
import { writeFileSync, unlinkSync, appendFileSync } from "fs";
import { spawn } from "node:child_process";

const version = process.env.STUB_SERVER_VERSION || "dev";
const hookSock = process.env.GRAVITY_HOOK_SOCK;
const pidFile = process.env.GRAVITY_PID_FILE;
const versionFile = process.env.GRAVITY_SERVER_VERSION_FILE;
const spawnLog = process.env.STUB_SPAWN_LOG;
const childPidFile = process.env.STUB_CHILD_PID_FILE;

if (versionFile) writeFileSync(versionFile, version);
if (spawnLog) appendFileSync(spawnLog, `${process.pid} ${version}\n`);

if (childPidFile) {
  // Own process group (no `detached: true`) so a group-TERM sent to
  // this stub's pgid (when it is itself a setsid group leader) also
  // reaches this descendant — the C3 regression test's guard.
  const child = spawn("sleep", ["600"], { stdio: "ignore" });
  writeFileSync(childPidFile, String(child.pid));
}

const srv = createServer(() => {});
srv.listen(hookSock, () => {
  if (pidFile) writeFileSync(pidFile, String(process.pid));
});

const shutdown = () => {
  setTimeout(() => {
    try { if (hookSock) unlinkSync(hookSock); } catch {}
    try { if (versionFile) unlinkSync(versionFile); } catch {}
    process.exit(0);
  }, 50);
};
process.on("SIGTERM", shutdown);
process.on("SIGINT", shutdown);
