import { Effect, Layer, ServiceMap } from "effect";
import { createConnection } from "net";
import { existsSync } from "fs";
import { SocketError } from "./errors.js";

export interface SocketPayload {
  event: string;
  session_id: string;
  cwd: string;
  pid: number | null;
  data: unknown;
  needs_response?: boolean;
  hook_input?: unknown;
}

export interface EmacsSocketService {
  readonly send: (payload: SocketPayload) => Effect.Effect<void, SocketError>;
  readonly sendAndWait: (payload: SocketPayload, timeoutMs?: number) => Effect.Effect<any, SocketError>;
  readonly socketExists: () => Effect.Effect<boolean>;
}

export const EmacsSocket = ServiceMap.Service<EmacsSocketService>("EmacsSocket");

export function makeEmacsSocket(socketPath: string): EmacsSocketService {
  return {
    send: (payload: SocketPayload) =>
      Effect.callback<void, SocketError>((resume) => {
        const client = createConnection(socketPath);

        client.on("connect", () => {
          const message = JSON.stringify(payload) + "\n";
          const flushed = client.write(message);
          if (flushed) {
            client.end();
          } else {
            client.once("drain", () => client.end());
          }
        });

        client.on("error", (_err) => {
          // Fail silently like original — resolve on error
          resume(Effect.void);
        });

        client.on("close", () => {
          resume(Effect.void);
        });
      }),

    sendAndWait: (payload: SocketPayload, timeoutMs: number = 345600000) =>
      Effect.callback<any, SocketError>((resume) => {
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

        client.on("connect", () => {
          const p = { ...payload, needs_response: true };
          const message = JSON.stringify(p) + "\n";
          client.write(message);
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

        client.on("error", (_err) => {
          if (!responded) {
            responded = true;
            clearTimeout(timer);
            resume(Effect.succeed({})); // fail silently like original
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

    socketExists: () =>
      Effect.sync(() => existsSync(socketPath)),
  };
}

export const EmacsSocketLive = (socketPath: string) =>
  Layer.succeed(EmacsSocket, makeEmacsSocket(socketPath));

export const EmacsSocketTest = (opts?: {
  exists?: boolean;
  sent?: SocketPayload[];
  responses?: any[];
}) => {
  const sent = opts?.sent ?? [];
  const responses = [...(opts?.responses ?? [])];
  return Layer.succeed(EmacsSocket, {
    send: (payload: SocketPayload) =>
      Effect.sync(() => { sent.push(payload); }),
    sendAndWait: (payload: SocketPayload) =>
      Effect.sync(() => {
        sent.push(payload);
        return responses.shift() ?? {};
      }),
    socketExists: () =>
      Effect.succeed(opts?.exists ?? true),
  });
};
