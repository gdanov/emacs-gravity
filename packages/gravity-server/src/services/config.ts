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
}

export const ServerConfig = ServiceMap.Service<ServerConfigData>("ServerConfig");

export const ServerConfigLive = Layer.effect(
  ServerConfig,
  Effect.sync(() => {
    const home = process.env.HOME || homedir();
    const stateDir = join(home, ".local", "state");
    return {
      hookSocketPath: process.env.GRAVITY_HOOK_SOCK ?? join(stateDir, "gravity-hooks.sock"),
      terminalSocketPath: process.env.GRAVITY_TERMINAL_SOCK ?? join(stateDir, "gravity-terminal.sock"),
      pidFilePath: process.env.GRAVITY_PID_FILE ?? join(stateDir, "gravity-server.pid"),
      logPath: process.env.GRAVITY_LOG_PATH || "/tmp/gravity-server.log",
      logMaxSize: parseInt(process.env.GRAVITY_LOG_MAX_SIZE || "2097152", 10),
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
    ...overrides,
  });
