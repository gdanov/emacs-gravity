import { describe, it, expect } from "vitest";
import { join } from "path";
import { readFileSync, existsSync } from "fs";

const BRIDGE_ROOT = join(__dirname, "..");
const HOOKS_JSON_PATH = join(BRIDGE_ROOT, "hooks", "hooks.json");
const PLUGIN_JSON_PATH = join(BRIDGE_ROOT, ".claude-plugin", "plugin.json");

const TOP_LEVEL_EVENTS = [
  "SessionStart",
  "SessionEnd",
  "Stop",
  "UserPromptSubmit",
  "SubagentStart",
  "SubagentStop",
  "Notification",
  "PreToolUse",
  "PostToolUse",
  "PostToolUseFailure",
  "PermissionRequest",
];

function collectCommands(hooksObj: any): string[] {
  const commands: string[] = [];
  for (const eventName of Object.keys(hooksObj)) {
    const matcherBlocks = hooksObj[eventName];
    for (const block of matcherBlocks) {
      for (const hook of block.hooks) {
        commands.push(hook.command);
      }
    }
  }
  return commands;
}

describe("hooks/hooks.json manifest", () => {
  it("exists and parses as JSON", () => {
    expect(existsSync(HOOKS_JSON_PATH)).toBe(true);
    const raw = readFileSync(HOOKS_JSON_PATH, "utf-8");
    expect(() => JSON.parse(raw)).not.toThrow();
  });

  it("has the top-level { hooks: {...} } wrapper", () => {
    const parsed = JSON.parse(readFileSync(HOOKS_JSON_PATH, "utf-8"));
    expect(Object.keys(parsed)).toEqual(["hooks"]);
    expect(typeof parsed.hooks).toBe("object");
    expect(parsed.hooks).not.toBeNull();
  });

  it("covers all 11 top-level hook events", () => {
    const parsed = JSON.parse(readFileSync(HOOKS_JSON_PATH, "utf-8"));
    for (const eventName of TOP_LEVEL_EVENTS) {
      expect(parsed.hooks).toHaveProperty(eventName);
    }
  });

  it("covers the 12th event, AskUserQuestionIntercept, nested under PreToolUse", () => {
    const parsed = JSON.parse(readFileSync(HOOKS_JSON_PATH, "utf-8"));
    const preToolUseStr = JSON.stringify(parsed.hooks.PreToolUse);
    expect(preToolUseStr).toContain("AskUserQuestionIntercept");

    // Exact-match assertions on the AskUserQuestion matcher block:
    // the matched block's first hook must carry the
    // AskUserQuestionIntercept command, the expected long-form
    // timeout, and the expected statusMessage. These are the
    // frozen-contract values from fix-pr27-plan §3 (transcribed
    // verbatim from hooks.json by the conductor) — assert them
    // against the LIVE parsed value, not against a hardcoded
    // duplicate, so a hooks.json drift fails this test loudly.
    const askBlock = parsed.hooks.PreToolUse.find(
      (b: any) => b.matcher === "AskUserQuestion",
    );
    expect(askBlock).toBeDefined();
    expect(askBlock.hooks[0].command).toBe(
      "${CLAUDE_PLUGIN_ROOT}/hooks/AskUserQuestionIntercept",
    );
    expect(askBlock.hooks[0].timeout).toBe(345600);
    expect(askBlock.hooks[0].statusMessage).toBe("gravity: question");
  });

  it("every command string uses ${CLAUDE_PLUGIN_ROOT}", () => {
    const parsed = JSON.parse(readFileSync(HOOKS_JSON_PATH, "utf-8"));
    const commands = collectCommands(parsed.hooks);
    expect(commands.length).toBeGreaterThan(0);
    for (const command of commands) {
      expect(command).toContain("${CLAUDE_PLUGIN_ROOT}");
    }
  });
});

describe("plugin.json has no inline hooks", () => {
  it("parses and has no hooks key, but retains name/version/description", () => {
    const parsed = JSON.parse(readFileSync(PLUGIN_JSON_PATH, "utf-8"));
    expect("hooks" in parsed).toBe(false);
    expect(parsed.name).toBeTruthy();
    expect(parsed.version).toBeTruthy();
    expect(parsed.description).toBeTruthy();
  });
});
