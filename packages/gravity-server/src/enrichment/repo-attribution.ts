// repo-attribution.ts — Server-side repo identity derivation for a session's cwd.
//
// Derived once per session lifetime and cached so repeated `updateMeta` calls
// and the overview's grouping pass never re-shell out. Never throws: any git
// failure (cwd is not a repo, git is missing, non-zero exit, timeout) falls
// back to the input cwd for all three fields, which keeps the grouping key
// deterministic even for sessions that live in ad-hoc scratch dirs.

import { execFileSync } from "child_process";
import { dirname, resolve } from "path";

export interface RepoAttribution {
  repoKey: string;
  repoRoot: string;
  worktree: string;
}

const cache = new Map<string, RepoAttribution>();

/**
 * Derive stable repo identity for a session's cwd, unifying a repo's main
 * checkout with all its worktrees. Server-side, synchronous, cached per cwd.
 * Never throws — falls back to { repoKey: cwd, repoRoot: cwd, worktree: cwd }
 * on any git failure (not a repo, git missing, non-zero exit).
 *
 * `repoKey` is the absolute path of the directory git considers the
 * "common" git dir (the main checkout's `.git` for worktrees, the
 * repo's own `.git` for the main checkout). `repoRoot` strips the
 * trailing `/.git` for display; `worktree` is the working tree path
 * reported by `--show-toplevel`.
 */
export function deriveRepoAttribution(cwd: string): RepoAttribution {
  const cached = cache.get(cwd);
  if (cached) return cached;

  let repoKey: string;
  let repoRoot: string;
  let worktree: string;
  try {
    const commonRaw = execFileSync(
      "git",
      ["-C", cwd, "rev-parse", "--git-common-dir"],
      { encoding: "utf-8", stdio: ["ignore", "pipe", "ignore"] },
    ).trim();
    const toplevelRaw = execFileSync(
      "git",
      ["-C", cwd, "rev-parse", "--show-toplevel"],
      { encoding: "utf-8", stdio: ["ignore", "pipe", "ignore"] },
    ).trim();

    repoKey = resolve(cwd, commonRaw);
    worktree = resolve(cwd, toplevelRaw);
    repoRoot = repoKey.endsWith("/.git") ? repoKey.slice(0, -5) : dirname(repoKey);
  } catch {
    repoKey = cwd;
    repoRoot = cwd;
    worktree = cwd;
  }

  const result: RepoAttribution = { repoKey, repoRoot, worktree };
  cache.set(cwd, result);
  return result;
}