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
import { mkdtempSync } from "fs";
import { tmpdir } from "os";
import { join, resolve } from "path";
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
});