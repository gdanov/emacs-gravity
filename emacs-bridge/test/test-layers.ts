// Test infrastructure for Effect-based integration tests.
// Provides pre-configured test layers that substitute real I/O
// with in-memory implementations.

import { Layer } from "effect";
import { ProcessIOTest } from "../src/services/process-io.js";
import { FsTest } from "../src/services/fs.js";
import { EmacsSocketTest } from "../src/services/emacs-socket.js";
import { BridgeConfigTest } from "../src/services/config.js";
import { LoggerLive } from "../src/services/logger.js";
import type { SocketPayload } from "../src/services/emacs-socket.js";
import type { BridgeConfigData } from "../src/services/config.js";

export interface TestLayerOpts {
  stdin?: string;
  argv?: string[];
  env?: Record<string, string>;
  fixtures?: Record<string, string>;
  socketExists?: boolean;
  socketResponses?: unknown[];
  configOverrides?: Partial<BridgeConfigData>;
}

export function testLayers(opts: TestLayerOpts) {
  const stdoutCapture: string[] = [];
  const socketSent: SocketPayload[] = [];

  const layers = Layer.mergeAll(
    ProcessIOTest({
      stdin: opts.stdin,
      argv: opts.argv,
      env: opts.env,
      stdoutCapture,
    }),
    FsTest(opts.fixtures ?? {}),
    EmacsSocketTest({
      exists: opts.socketExists ?? true,
      sent: socketSent,
      responses: opts.socketResponses,
    }),
    BridgeConfigTest(opts.configOverrides),
    LoggerLive,
  );

  return { layers, stdoutCapture, socketSent };
}
