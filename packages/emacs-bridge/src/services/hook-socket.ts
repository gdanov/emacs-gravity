import { Effect, Layer, ServiceMap } from "effect";
import { createConnection } from "net";
import { existsSync } from "fs";
import { join } from "path";
import type { HookSocketMessage } from "@gravity/shared";

// ── Service interface ─────────────────────────────────────────────────

export interface HookSocketClientService {
  /**
   * Send a HookSocketMessage to the gravity-server hook socket.
   * Fire-and-forget: connects, sends JSON + newline, disconnects.
   * Falls back gracefully if server is unavailable (logs, doesn't fail).
   */
  readonly send: (msg: HookSocketMessage) => Effect.Effect<void>;
}

export const HookSocketClient = ServiceMap.Service<HookSocketClientService>("HookSocketClient");

// ── Implementation ────────────────────────────────────────────────────

export function makeHookSocketClient(socketPath: string): HookSocketClientService {
  return {
    send: (msg: HookSocketMessage) =>
      Effect.callback<void>((resume) => {
        // If socket file doesn't exist, skip silently
        if (!existsSync(socketPath)) {
          resume(Effect.void);
          return;
        }

        const client = createConnection(socketPath);
        let settled = false;

        // Timeout: if we can't connect within 2s, give up
        const timer = setTimeout(() => {
          if (!settled) {
            settled = true;
            client.destroy();
            resume(Effect.void);
          }
        }, 2000);
        timer.unref();

        client.on("connect", () => {
          const payload = JSON.stringify(msg) + "\n";
          const flushed = client.write(payload);
          if (flushed) {
            client.end();
          } else {
            client.once("drain", () => client.end());
          }
        });

        client.on("error", () => {
          // Fail silently — server unavailable is fine
          if (!settled) {
            settled = true;
            clearTimeout(timer);
            resume(Effect.void);
          }
        });

        client.on("close", () => {
          if (!settled) {
            settled = true;
            clearTimeout(timer);
            resume(Effect.void);
          }
        });
      }),
  };
}

// ── Layer constructors ────────────────────────────────────────────────

/** Resolve hook socket path from environment, matching gravity-server defaults. */
function resolveHookSocketPath(): string {
  const fromEnv = process.env.GRAVITY_HOOK_SOCK;
  if (fromEnv) return fromEnv;
  const home = process.env.HOME || "/tmp";
  return join(home, ".local", "state", "gravity-hooks.sock");
}

export const HookSocketClientLive = Layer.succeed(
  HookSocketClient,
  makeHookSocketClient(resolveHookSocketPath()),
);

/** Test layer that captures sent messages. */
export const HookSocketClientTest = (opts?: { sent?: HookSocketMessage[] }) => {
  const sent = opts?.sent ?? [];
  return Layer.succeed(HookSocketClient, {
    send: (msg: HookSocketMessage) =>
      Effect.sync(() => { sent.push(msg); }),
  });
};
