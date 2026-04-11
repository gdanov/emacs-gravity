#!/usr/bin/env node
// Read commits since the last `v*` tag, determine a semver bump level
// (patch by default, minor if any `feat:` commit, major on `BREAKING CHANGE:`
// or `!:` subject), write the new version back to the bridge plugin manifest,
// and emit it on GITHUB_OUTPUT for subsequent workflow steps.

import { execSync } from "node:child_process";
import { readFileSync, writeFileSync, appendFileSync } from "node:fs";

const PLUGIN_JSON = "packages/emacs-bridge/.claude-plugin/plugin.json";

const sh = (cmd) => execSync(cmd, { encoding: "utf8" }).trimEnd();

const lastTag = () => {
  try {
    return sh("git describe --tags --abbrev=0 --match 'v*'");
  } catch {
    return null;
  }
};

const commitsSince = (range) => {
  // Use a stable record terminator so commit bodies (which may contain
  // blank lines) don't collide with the outer split.
  const fmt = "%s%n%b%n==GRAVITY-END==";
  const raw = sh(`git log --format=${JSON.stringify(fmt)} ${range}`);
  return raw
    .split("==GRAVITY-END==")
    .map((s) => s.trim())
    .filter(Boolean)
    .map((s) => {
      const [subject, ...bodyLines] = s.split("\n");
      return { subject, body: bodyLines.join("\n") };
    });
};

const bumpLevel = (commits) => {
  let level = "patch";
  for (const { subject, body } of commits) {
    // Breaking change: `type!:` in subject OR `BREAKING CHANGE:` as a
    // conventional-commits footer — start of a line in the body, not
    // mid-sentence or inside backticks. Without the `^...m` anchor, the
    // v3.0.1 → v4.0.0 bump was accidentally triggered by a commit body
    // that *documented* this very regex inside backticks.
    if (/^[a-z]+(\([^)]*\))?!:/.test(subject) || /^BREAKING CHANGE:/m.test(body)) {
      return "major";
    }
    if (/^feat(\([^)]*\))?:/.test(subject)) {
      level = "minor";
    }
  }
  return level;
};

const bumpVersion = (version, level) => {
  const [major, minor, patch] = version.split(".").map(Number);
  if (level === "major") return `${major + 1}.0.0`;
  if (level === "minor") return `${major}.${minor + 1}.0`;
  return `${major}.${minor}.${patch + 1}`;
};

const tag = lastTag();
const range = tag ? `${tag}..HEAD` : "HEAD";
const commits = commitsSince(range);

if (commits.length === 0) {
  console.error(`No commits in range ${range} — nothing to release`);
  process.exit(0);
}

const manifest = JSON.parse(readFileSync(PLUGIN_JSON, "utf8"));
const current = manifest.version;
const level = bumpLevel(commits);
const next = bumpVersion(current, level);
manifest.version = next;
writeFileSync(PLUGIN_JSON, JSON.stringify(manifest, null, 2) + "\n");

console.log(
  `Bumping ${current} → ${next} (${level}) from ${commits.length} commit(s) since ${tag ?? "root"}`,
);

if (process.env.GITHUB_OUTPUT) {
  appendFileSync(
    process.env.GITHUB_OUTPUT,
    `version=${next}\nlevel=${level}\nprevious=${current}\n`,
  );
}
