#!/usr/bin/env node
// check-project-settings.mjs — Lint guard for enabledPlugins drift
//
// Walks every .claude/settings.json and .claude/settings.local.json under the
// repo (including .worktrees/) and fails if any contains an enabledPlugins key
// that is an empty object. An empty enabledPlugins silently disables all
// plugins at the project level under Claude Code's settings-merge semantics.

import { readdirSync, readFileSync, statSync, existsSync } from "fs";
import { join, relative } from "path";

const ROOT = new URL("..", import.meta.url).pathname.replace(/\/$/, "");
const SETTINGS_NAMES = ["settings.json", "settings.local.json"];
const errors = [];

function walk(dir) {
  let entries;
  try {
    entries = readdirSync(dir, { withFileTypes: true });
  } catch {
    return;
  }
  for (const entry of entries) {
    const full = join(dir, entry.name);
    if (entry.name === "node_modules" || entry.name === ".git") continue;
    if (entry.isDirectory()) {
      if (entry.name === ".claude") {
        for (const name of SETTINGS_NAMES) {
          const settingsPath = join(full, name);
          if (existsSync(settingsPath)) {
            checkFile(settingsPath);
          }
        }
      }
      walk(full);
    }
  }
}

function checkFile(path) {
  try {
    const content = JSON.parse(readFileSync(path, "utf8"));
    const ep = content.enabledPlugins;
    if (ep !== undefined && typeof ep === "object" && Object.keys(ep).length === 0) {
      const rel = relative(ROOT, path);
      errors.push(`${rel}: enabledPlugins is empty {} — this silently disables all plugins in this directory`);
    }
  } catch (e) {
    const rel = relative(ROOT, path);
    errors.push(`${rel}: failed to parse JSON: ${e.message}`);
  }
}

walk(ROOT);

if (errors.length > 0) {
  console.error("check-project-settings FAILED:");
  for (const e of errors) {
    console.error(`  - ${e}`);
  }
  process.exit(1);
} else {
  console.log("check-project-settings: OK (no enabledPlugins drift found)");
}
