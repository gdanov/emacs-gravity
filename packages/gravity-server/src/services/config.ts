// config.ts — Server configuration service
//
// Extracts hardcoded socket/PID paths into a testable service.

import { Effect, Layer, ServiceMap } from "effect";
import { homedir } from "os";
import { join } from "path";

export interface ServerConfigData {
  readonly hookSocketPath: string;
  readonly terminalSocketPath: string;
  readonly pidFilePath: string;
  readonly logPath: string;
  readonly logMaxSize: number;
  // Pi driver options (when --pi flag is set)
  readonly piEnabled: boolean;
  readonly piCwd: string | undefined;
  readonly piThinkingLevel: string | undefined;
}

export const ServerConfig = ServiceMap.Service<ServerConfigData>("ServerConfig");

export const ServerConfigLive = Layer.effect(
  ServerConfig,
  Effect.sync(() => {
    const home = process.env.HOME || homedir();
    const stateDir = join(home, ".local", "state");

    // Check for --pi flag in command line arguments
    const args = process.argv.slice(2);
    const piIndex = args.indexOf("--pi");
    const piEnabled = piIndex >= 0;

    // Extract optional --pi-cwd and --pi-thinking flags
    let piCwd: string | undefined;
    let piThinkingLevel: string | undefined;

    for (let i = 0; i < args.length; i++) {
      if (args[i] === "--pi-cwd" && i + 1 < args.length) {
        piCwd = args[i + 1];
        i++;
      } else if (args[i] === "--pi-thinking" && i + 1 < args.length) {
        piThinkingLevel = args[i + 1];
        i++;
      }
    }

    return {
      hookSocketPath: process.env.GRAVITY_HOOK_SOCK ?? join(stateDir, "gravity-hooks.sock"),
      terminalSocketPath: process.env.GRAVITY_TERMINAL_SOCK ?? join(stateDir, "gravity-terminal.sock"),
      pidFilePath: process.env.GRAVITY_PID_FILE ?? join(stateDir, "gravity-server.pid"),
      logPath: process.env.GRAVITY_LOG_PATH || "/tmp/gravity-server.log",
      logMaxSize: parseInt(process.env.GRAVITY_LOG_MAX_SIZE || "2097152", 10),
      piEnabled,
      piCwd,
      piThinkingLevel,
    };
  }),
);

export const ServerConfigTest = (overrides?: Partial<ServerConfigData>) =>
  Layer.succeed(ServerConfig, {
    hookSocketPath: "/tmp/test-hooks.sock",
    terminalSocketPath: "/tmp/test-terminal.sock",
    pidFilePath: "/tmp/test-server.pid",
    logPath: "/tmp/test-gravity-server.log",
    logMaxSize: 2097152,
    piEnabled: false,
    piCwd: undefined,
    piThinkingLevel: undefined,
    ...overrides,
  });
