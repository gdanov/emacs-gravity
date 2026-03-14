import { Effect, Layer, ServiceMap } from "effect";
import { execSync } from "child_process";
import { ProcessIO } from "./process-io.js";
import { Fs } from "./fs.js";
import { join } from "path";

export interface BridgeConfigData {
  readonly socketPath: string;
  readonly dumpDir: string | undefined;
  readonly dumpEnabled: boolean;
  readonly noAutoApprove: boolean;
  readonly claudePid: number | null;
  readonly tempId: string | null;
  readonly tmuxSession: string | null;
  readonly effortLevel: string | null;
}

export const BridgeConfig = ServiceMap.Service<BridgeConfigData>("BridgeConfig");

export const BridgeConfigLive = Layer.effect(
  BridgeConfig,
  Effect.gen(function* () {
    const io = yield* Effect.service(ProcessIO);
    const fs = yield* Effect.service(Fs);

    // Socket path resolution
    const gravitySock = yield* io.getEnv("CLAUDE_GRAVITY_SOCK");
    const sockDir = yield* io.getEnv("CLAUDE_GRAVITY_SOCK_DIR");
    const home = (yield* io.getEnv("HOME")) ?? "/tmp";
    const socketPath = gravitySock
      ?? (sockDir ? join(sockDir, "claude-gravity.sock") : join(home, ".local", "state", "claude-gravity.sock"));

    const dumpDir = yield* io.getEnv("CLAUDE_GRAVITY_DUMP_DIR");
    const dumpEnabled = !!dumpDir || (yield* io.getEnv("CLAUDE_GRAVITY_DUMP")) === "1";
    const noAutoApprove = (yield* io.getEnv("CLAUDE_GRAVITY_NO_AUTO_APPROVE")) === "1";
    const claudePidStr = yield* io.getEnv("CLAUDE_PID");
    const claudePid = claudePidStr ? parseInt(claudePidStr, 10) || null : null;
    const tempId = (yield* io.getEnv("CLAUDE_GRAVITY_TEMP_ID")) ?? null;

    // Detect tmux session
    let tmuxSession: string | null = null;
    const tmuxEnv = yield* io.getEnv("TMUX");
    if (tmuxEnv) {
      try {
        tmuxSession = execSync('tmux display-message -p "#{session_name}"',
          { encoding: "utf-8", timeout: 1000 }).trim() || null;
      } catch {}
    }

    // Read effort level from settings
    let effortLevel: string | null = null;
    const settingsPath = join(home, ".claude", "settings.json");
    const settingsExists = yield* fs.exists(settingsPath);
    if (settingsExists) {
      const content = yield* fs.readFile(settingsPath).pipe(
        Effect.catch(() => Effect.succeed(""))
      );
      if (content) {
        try {
          const settings = JSON.parse(content);
          effortLevel = settings.effortLevel ?? null;
        } catch {}
      }
    }

    return {
      socketPath,
      dumpDir,
      dumpEnabled,
      noAutoApprove,
      claudePid,
      tempId,
      tmuxSession,
      effortLevel,
    };
  })
);

export const BridgeConfigTest = (overrides?: Partial<BridgeConfigData>) =>
  Layer.succeed(BridgeConfig, {
    socketPath: "/tmp/test.sock",
    dumpDir: undefined,
    dumpEnabled: false,
    noAutoApprove: false,
    claudePid: null,
    tempId: null,
    tmuxSession: null,
    effortLevel: null,
    ...overrides,
  });
