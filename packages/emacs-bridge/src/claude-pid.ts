// claude-pid.ts — Resolve the PID of the long-lived `claude` process a
// hook belongs to.
//
// The hook scripts pass CLAUDE_PID=$PPID, which assumes the hook's parent
// IS claude. That holds only when claude spawns the hook directly. When it
// spawns hooks through an intermediate shell, $PPID is that shell, which
// exits as soon as the hook returns — so the server sees a fresh,
// already-dead pid on every event and reaps live sessions. Walking the
// ancestry to the nearest `claude` forebear is correct in both layouts.

import { execSync } from "child_process";
import { basename } from "path";

export interface ProcInfo {
  readonly ppid: number;
  readonly comm: string;
}

export type ProcLookup = (pid: number) => ProcInfo | null;

const MAX_HOPS = 8;

function isClaude(comm: string): boolean {
  return basename(comm.trim()) === "claude";
}

/**
 * Walk up the process ancestry from `startPid` and return the pid of the
 * nearest ancestor named `claude`, or null if none is found within
 * `maxHops`. `startPid` itself is considered. Pure — all process
 * inspection goes through `lookup`.
 */
export function resolveClaudePid(
  startPid: number,
  lookup: ProcLookup,
  maxHops: number = MAX_HOPS,
): number | null {
  let pid = startPid;
  for (let hop = 0; hop < maxHops; hop++) {
    if (!Number.isInteger(pid) || pid <= 1) return null;
    const info = lookup(pid);
    if (!info) return null;
    if (isClaude(info.comm)) return pid;
    pid = info.ppid;
  }
  return null;
}

/**
 * `ps`-backed ProcLookup. Uses only POSIX-specified `ps` options so it
 * behaves the same on macOS and Linux; returns null for any pid it cannot
 * read (dead, permission-denied, or unparseable output).
 */
export const psLookup: ProcLookup = (pid) => {
  let out: string;
  try {
    out = execSync(`ps -o ppid=,comm= -p ${pid}`, {
      encoding: "utf-8",
      timeout: 1000,
      stdio: ["ignore", "pipe", "ignore"],
    });
  } catch {
    // Expected for a dead pid: `ps` exits non-zero when it matches nothing.
    return null;
  }
  const line = out.trim();
  if (!line) return null;
  const match = /^(\d+)\s+(.+)$/.exec(line);
  if (!match) return null;
  return { ppid: parseInt(match[1], 10), comm: match[2] };
};
