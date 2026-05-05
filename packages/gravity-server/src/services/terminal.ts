// terminal.ts — Terminal server as Effect service
//
// Replaces protocol/terminal-server.ts class with a ServiceMap service.
// Manages long-lived terminal connections with backpressure handling.

import { Layer, ServiceMap } from "effect";
import type { ChangedArea, ServerMessage } from "@gravity/shared";
import type { Socket } from "net";

const MAX_QUEUED_MESSAGES = 200;

export interface TerminalConnection {
  readonly socket: Socket;
  readonly subscribedSessions: Set<string>;
  capabilities: Set<string>;
  /** Messages waiting for the socket to drain. */
  writeQueue: string[];
  /** True when socket.write() returned false (buffer full). */
  draining: boolean;
}

export interface TerminalService {
  readonly addConnection: (socket: Socket) => TerminalConnection;
  readonly broadcast: (message: ServerMessage) => void;
  readonly sendTo: (conn: TerminalConnection, message: ServerMessage) => void;
  readonly sendToSubscribers: (sessionId: string, message: ServerMessage) => void;
  readonly unsubscribeAll: (sessionId: string) => void;
  readonly hasCapableTerminal: (capability: string) => boolean;
  readonly connectionCount: () => number;

  // Pull mode: signals (lightweight, no payload)
  readonly signalChanged: (what: ChangedArea, sessionId?: string, seq?: number) => void;
  readonly signalChangedTo: (conn: TerminalConnection, what: ChangedArea, sessionId?: string, seq?: number) => void;
}

export const Terminal = ServiceMap.Service<TerminalService>("Terminal");

export function makeTerminal(logFn?: (msg: string, level?: string) => void): TerminalService {
  let connections: TerminalConnection[] = [];
  const doLog = logFn ?? (() => {});

  const writeToConnection = (conn: TerminalConnection, json: string): void => {
    if (conn.socket.destroyed || !conn.socket.writable) return;

    if (conn.draining) {
      conn.writeQueue.push(json);
      enforceQueueLimit(conn);
      return;
    }

    try {
      const flushed = conn.socket.write(json);
      if (!flushed) {
        conn.draining = true;
      }
    } catch (err) {
      doLog(`Terminal write error: ${(err as Error).message}`, "error");
    }
  };

  const flushQueue = (conn: TerminalConnection): void => {
    while (conn.writeQueue.length > 0) {
      if (conn.socket.destroyed || !conn.socket.writable) {
        conn.writeQueue.length = 0;
        return;
      }
      const json = conn.writeQueue.shift()!;
      try {
        const flushed = conn.socket.write(json);
        if (!flushed) {
          conn.draining = true;
          return;
        }
      } catch (err) {
        doLog(`Terminal flush error: ${(err as Error).message}`, "error");
        conn.writeQueue.length = 0;
        return;
      }
    }
  };

  const enforceQueueLimit = (conn: TerminalConnection): void => {
    if (conn.writeQueue.length > MAX_QUEUED_MESSAGES) {
      doLog(`Terminal write queue exceeded ${MAX_QUEUED_MESSAGES} — disconnecting stuck client`, "warn");
      conn.writeQueue.length = 0;
      conn.socket.destroy();
    }
  };

  return {
    addConnection: (socket) => {
      const conn: TerminalConnection = {
        socket,
        subscribedSessions: new Set(),
        capabilities: new Set(),
        writeQueue: [],
        draining: false,
      };
      connections.push(conn);

      socket.on("close", () => {
        connections = connections.filter((c) => c !== conn);
      });

      socket.on("error", (err) => {
        doLog(`Terminal connection error: ${err.message}`, "error");
        socket.destroy();
      });

      socket.on("drain", () => {
        conn.draining = false;
        flushQueue(conn);
      });

      return conn;
    },

    broadcast: (message) => {
      const json = JSON.stringify(message) + "\n";
      for (const conn of [...connections]) {
        writeToConnection(conn, json);
      }
    },

    sendTo: (conn, message) => {
      writeToConnection(conn, JSON.stringify(message) + "\n");
    },

    sendToSubscribers: (sessionId, message) => {
      const json = JSON.stringify(message) + "\n";
      for (const conn of [...connections]) {
        if (conn.subscribedSessions.has(sessionId)) {
          writeToConnection(conn, json);
        }
      }
    },

    unsubscribeAll: (sessionId) => {
      for (const conn of connections) {
        conn.subscribedSessions.delete(sessionId);
      }
    },

    hasCapableTerminal: (capability) =>
      connections.some((c) => c.capabilities.has(capability)),

    connectionCount: () => connections.length,

    // Pull mode: send lightweight signal (no payload)
    signalChanged: (what, sessionId, seq) => {
      const json = JSON.stringify({
        type: "state-changed",
        what,
        ...(sessionId ? { sessionId } : {}),
        seq: seq ?? 0,
      }) + "\n";
      for (const conn of [...connections]) {
        writeToConnection(conn, json);
      }
    },

    signalChangedTo: (conn, what, sessionId, seq) => {
      const json = JSON.stringify({
        type: "state-changed",
        what,
        ...(sessionId ? { sessionId } : {}),
        seq: seq ?? 0,
      }) + "\n";
      writeToConnection(conn, json);
    },
  };
}

export const TerminalLive = Layer.succeed(Terminal, makeTerminal());

export const TerminalTest = (logFn?: (msg: string, level?: string) => void) =>
  Layer.succeed(Terminal, makeTerminal(logFn));
