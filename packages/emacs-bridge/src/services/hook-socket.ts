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

  /**
   * Send a HookSocketMessage and wait for a response from the server.
   * Used for bidirectional events (PermissionRequest, AskUserQuestionIntercept).
   * The connection stays open until the server writes a response or timeout.
   * Returns the parsed JSON response, or {} on error/timeout.
   */
  readonly sendAndWait: (msg: HookSocketMessage, timeoutMs?: number) => Effect.Effect<any>;
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

    sendAndWait: (msg: HookSocketMessage, timeoutMs: number = 345600000) =>
      Effect.callback<any>((resume) => {
        // If socket file doesn't exist, return empty response
        if (!existsSync(socketPath)) {
          resume(Effect.succeed({}));
          return;
        }

        const client = createConnection(socketPath);
        let responded = false;
        let buffer = "";

        const timer = setTimeout(() => {
          if (!responded) {
            responded = true;
            client.destroy();
            resume(Effect.succeed({}));
          }
        }, timeoutMs);
        timer.unref();

        client.on("connect", () => {
          // Mark as needing a response so the server keeps the socket open
          const payload = JSON.stringify({ ...msg, needs_response: true }) + "\n";
          client.write(payload);
          // Don't end — wait for response
        });

        client.on("data", (chunk) => {
          buffer += chunk.toString();
          const newlineIdx = buffer.indexOf("\n");
          if (newlineIdx >= 0 && !responded) {
            responded = true;
            clearTimeout(timer);
            const line = buffer.substring(0, newlineIdx);
            try {
              resume(Effect.succeed(JSON.parse(line)));
            } catch {
              resume(Effect.succeed({}));
            }
            client.destroy();
          }
        });

        client.on("error", () => {
          if (!responded) {
            responded = true;
            clearTimeout(timer);
            resume(Effect.succeed({}));
          }
        });

        client.on("close", () => {
          if (!responded) {
            responded = true;
            clearTimeout(timer);
            resume(Effect.succeed({}));
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
export const HookSocketClientTest = (opts?: {
  sent?: HookSocketMessage[];
  responses?: any[];
}) => {
  const sent = opts?.sent ?? [];
  const responses = [...(opts?.responses ?? [])];
  return Layer.succeed(HookSocketClient, {
    send: (msg: HookSocketMessage) =>
      Effect.sync(() => { sent.push(msg); }),
    sendAndWait: (msg: HookSocketMessage) =>
      Effect.sync(() => {
        sent.push(msg);
        return responses.shift() ?? {};
      }),
  });
};
