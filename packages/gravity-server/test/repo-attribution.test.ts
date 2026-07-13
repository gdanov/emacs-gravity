// repo-attribution.test.ts — Unit tests for deriveRepoAttribution
//
// Two cases per the spec:
//   1. A real git repo fixture (process.cwd() inside this worktree counts
//      as one — git rev-parse from inside a worktree resolves to the
//      MAIN checkout's `.git` directory, which is the correct unifying
//      behavior this function exists for).
//   2. A non-git tmpdir via fs.mkdtempSync — falls back to the input
//      cwd for all three fields, never throws.

import { describe, it, expect } from "vitest";
import { mkdtempSync, rmSync } from "fs";
import { tmpdir } from "os";
import { join, resolve } from "path";
import { execFileSync } from "child_process";
import { deriveRepoAttribution } from "../src/enrichment/repo-attribution.js";

describe("deriveRepoAttribution", () => {
  it("returns the .git path as repoKey when cwd is inside a git repo", () => {
    // process.cwd() when this test runs is the gravity-server package
    // dir, which lives inside the worktree at .worktrees/issue-22-pi-ingest.
    // git rev-parse --git-common-dir from inside a worktree points at the
    // MAIN checkout's .git directory, which is exactly the identity that
    // unifies a worktree with its main checkout.
    const cwd = process.cwd();
    const attr = deriveRepoAttribution(cwd);

    // repoKey is the resolved git-common-dir — for a non-bare repo this
    // is an absolute path that ends with `/.git`.
    expect(attr.repoKey.endsWith("/.git")).toBe(true);
    expect(attr.repoKey.startsWith("/")).toBe(true);

    // repoRoot is the parent of repoKey when repoKey ends with `/.git`.
    expect(attr.repoRoot.endsWith("/.git")).toBe(false);
    expect(attr.repoRoot).toBe(attr.repoKey.slice(0, -5));

    // worktree is the absolute path reported by --show-toplevel.
    expect(attr.worktree.startsWith("/")).toBe(true);
    expect(attr.worktree).not.toBe("");
  });

  it("falls back to cwd for all three fields in a non-git directory", () => {
    const tmp = mkdtempSync(join(tmpdir(), "repo-attr-fallback-"));
    try {
      const attr = deriveRepoAttribution(tmp);
      expect(attr.repoKey).toBe(tmp);
      expect(attr.repoRoot).toBe(tmp);
      expect(attr.worktree).toBe(tmp);
    } finally {
      // mkdtempSync created the dir; clean it up so the test sandbox stays tidy.
      const { rmSync } = require("fs") as typeof import("fs");
      rmSync(tmp, { recursive: true, force: true });
    }
  });

  it("never throws when git is missing or cwd has no git metadata", () => {
    // A path that cannot exist (under /dev/null, which is not a dir and
    // cannot be chdir'd into for git purposes).
    const attr = deriveRepoAttribution("/this/path/does/not/exist/anywhere");
    // The fallback shape: all three fields equal the input.
    expect(attr.repoKey).toBe("/this/path/does/not/exist/anywhere");
    expect(attr.repoRoot).toBe("/this/path/does/not/exist/anywhere");
    expect(attr.worktree).toBe("/this/path/does/not/exist/anywhere");
  });

  it("caches by raw cwd string so a second call returns the same object", () => {
    const cwd = process.cwd();
    const first = deriveRepoAttribution(cwd);
    const second = deriveRepoAttribution(cwd);
    // Strict equality — the cache returns the exact same object, not a
    // re-derived copy.
    expect(second).toBe(first);
  });

  it("treats equivalent absolute cwds distinctly when they differ as raw strings", () => {
    // Two distinct non-git directories: the cache keys by raw string, so
    // each gets its own fallback entry. The function does NOT normalize
    // (no symlink resolution, no path canonicalization beyond what
    // path.resolve gives us for git's relative output).
    const tmpA = mkdtempSync(join(tmpdir(), "repo-attr-a-"));
    const tmpB = mkdtempSync(join(tmpdir(), "repo-attr-b-"));
    try {
      const a = deriveRepoAttribution(tmpA);
      const b = deriveRepoAttribution(tmpB);
      expect(a.repoKey).toBe(tmpA);
      expect(b.repoKey).toBe(tmpB);
      expect(a).not.toBe(b);
    } finally {
      const { rmSync } = require("fs") as typeof import("fs");
      rmSync(tmpA, { recursive: true, force: true });
      rmSync(tmpB, { recursive: true, force: true });
    }
  });

  it("resolves git's possibly-relative --git-common-dir output to an absolute path", () => {
    // When cwd IS the repo root, git may print a relative path like
    // `.git`. path.resolve(cwd, ".git") yields the absolute .git path.
    // We don't move cwd in this test; we verify that whatever the function
    // returned for a real-git-repo call has an absolute repoKey.
    const cwd = process.cwd();
    const attr = deriveRepoAttribution(cwd);
    expect(resolve(attr.repoKey)).toBe(attr.repoKey);
  });

  it("unifies a real repo + a real git worktree on the same repoKey (end-to-end)", () => {
    // Build a FRESH repo + worktree fixture inside tmpdir so the test does
    // not interact with this worktree's own `.git`. The whole point of
    // this test is that `deriveRepoAttribution` -- working ONLY off
    // `git -C cwd rev-parse {--git-common-dir,--show-toplevel}` --
    // produces the SAME `repoKey` for both the main checkout cwd and
    // the worktree cwd, because `--git-common-dir` resolves to the
    // main checkout's `.git` directory for both.
    const mainDir = mkdtempSync(join(tmpdir(), "repo-attr-main-"));
    const worktreeParent = mkdtempSync(join(tmpdir(), "repo-attr-wt-parent-"));
    const worktreeDir = join(worktreeParent, "wt-branch");
    const branch = "wt-test-branch";
    try {
      execFileSync("git", ["init", "--initial-branch=main", mainDir], { stdio: "ignore" });
      // git worktree add requires at least one commit on the repo so a
      // branch can be created from HEAD; an empty repo refuses the
      // command. Single empty commit is enough.
      execFileSync("git", ["-C", mainDir, "-c", "user.email=t@t", "-c", "user.name=t", "commit", "--allow-empty", "-m", "initial"], { stdio: "ignore" });

      // Add the worktree on a new branch rooted at the main checkout's
      // HEAD. `worktreeDir` lives OUTSIDE `mainDir` so it is a true
      // independent checkout with its own `git` pointer file.
      execFileSync("git", ["-C", mainDir, "worktree", "add", "-b", branch, worktreeDir], { stdio: "ignore" });

      // Sanity: the worktree really is a worktree (its own toplevel).
      expect(execFileSync("git", ["-C", worktreeDir, "rev-parse", "--show-toplevel"], { encoding: "utf-8" }).trim()).toBe(worktreeDir);
      // And it points back at mainDir as its git-common-dir.
      const wtCommon = execFileSync("git", ["-C", worktreeDir, "rev-parse", "--git-common-dir"], { encoding: "utf-8" }).trim();
      expect(resolve(worktreeDir, wtCommon)).toBe(join(mainDir, ".git"));

      const mainAttr = deriveRepoAttribution(mainDir);
      const wtAttr = deriveRepoAttribution(worktreeDir);

      // The unifying invariant this whole function exists for: both cwds
      // resolve to the SAME repoKey (the main checkout's .git dir).
      expect(wtAttr.repoKey).toBe(mainAttr.repoKey);
      expect(wtAttr.repoKey).toBe(join(mainDir, ".git"));
      // The two worktrees still report their own working tree as worktree.
      expect(mainAttr.worktree).toBe(mainDir);
      expect(wtAttr.worktree).toBe(worktreeDir);
    } finally {
      // Try the polite teardown first (git worktree remove). If that
      // fails (the test bailed mid-setup, etc.) the final rmSync with
      // force still guarantees no dangling state.
      try {
        execFileSync("git", ["-C", mainDir, "worktree", "remove", "--force", worktreeDir], { stdio: "ignore" });
      } catch { /* best effort */ }
      try {
        execFileSync("git", ["-C", mainDir, "worktree", "prune"], { stdio: "ignore" });
      } catch { /* best effort */ }
      rmSync(mainDir, { recursive: true, force: true });
      rmSync(worktreeParent, { recursive: true, force: true });
    }
  });
});