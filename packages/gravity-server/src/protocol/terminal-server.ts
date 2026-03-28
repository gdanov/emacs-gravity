// terminal-server.ts — Accept terminal connections, dispatch messages
//
// Manages long-lived connections from terminals (Emacs, web, etc.)
// Sends snapshots on connect, patches on state changes, inbox events.
//
// Write strategy: uses Node's built-in backpressure. When socket.write()
// returns false (buffer full), messages are queued and flushed on 'drain'.
// This prevents EPIPE storms where aggressive destroy() on transient write
// failures triggers reconnect cycles. See emacs-gravity-kdg.

import type { ServerMessage, TerminalMessage } from "@gravity/shared";
import type { Socket } from "net";
import { log } from "../util/log.js";

// Max queued messages per connection before we consider it stuck and disconnect.
const MAX_QUEUED_MESSAGES = 200;

export interface TerminalConnection {
  socket: Socket;
  subscribedSessions: Set<string>;
  capabilities: Set<string>;
  /** Messages waiting for the socket to drain. */
  writeQueue: string[];
  /** True when socket.write() returned false (buffer full). */
  draining: boolean;
}

export class TerminalServer {
  private connections: TerminalConnection[] = [];

  addConnection(socket: Socket): TerminalConnection {
    const conn: TerminalConnection = {
      socket,
      subscribedSessions: new Set(),
      capabilities: new Set(),
      writeQueue: [],
      draining: false,
    };
    this.connections.push(conn);

    socket.on("close", () => {
      this.connections = this.connections.filter((c) => c !== conn);
    });

    socket.on("error", (err) => {
      log(`Terminal connection error: ${err.message}`, "error");
      socket.destroy();
    });

    socket.on("drain", () => {
      conn.draining = false;
      this.flushQueue(conn);
    });

    return conn;
  }

  /** Broadcast a message to all connected terminals. */
  broadcast(message: ServerMessage): void {
    const json = JSON.stringify(message) + "\n";
    for (const conn of [...this.connections]) {
      this.writeToConnection(conn, json);
    }
  }

  /** Send a message to terminals subscribed to a specific session. */
  sendToSubscribers(sessionId: string, message: ServerMessage): void {
    const json = JSON.stringify(message) + "\n";
    for (const conn of [...this.connections]) {
      if (conn.subscribedSessions.has(sessionId)) {
        this.writeToConnection(conn, json);
      }
    }
  }

  /** Send a message to a specific connection. */
  sendTo(conn: TerminalConnection, message: ServerMessage): void {
    this.writeToConnection(conn, JSON.stringify(message) + "\n");
  }

  /** Remove a session from all connections' subscriptions. */
  unsubscribeAll(sessionId: string): void {
    for (const conn of this.connections) {
      conn.subscribedSessions.delete(sessionId);
    }
  }

  /** Check if any connected terminal has the given capability. */
  hasCapableTerminal(capability: string): boolean {
    return this.connections.some((c) => c.capabilities.has(capability));
  }

  /** Number of connected terminals. */
  get connectionCount(): number {
    return this.connections.length;
  }

  // ── Private helpers ──────────────────────────────────────────────

  /** Write a pre-serialized JSON line to a connection, queuing if backpressured. */
  private writeToConnection(conn: TerminalConnection, json: string): void {
    if (conn.socket.destroyed || !conn.socket.writable) return;

    // If already backpressured, queue instead of writing
    if (conn.draining) {
      conn.writeQueue.push(json);
      this.enforceQueueLimit(conn);
      return;
    }

    try {
      const flushed = conn.socket.write(json);
      if (!flushed) {
        // Kernel buffer full — pause and wait for drain
        conn.draining = true;
      }
    } catch (err) {
      // Actual write error (not backpressure) — e.g. socket reset.
      // The 'error' event handler will destroy the socket.
      log(`Terminal write error: ${(err as Error).message}`, "error");
    }
  }

  /** Flush queued messages after a drain event. */
  private flushQueue(conn: TerminalConnection): void {
    while (conn.writeQueue.length > 0) {
      if (conn.socket.destroyed || !conn.socket.writable) {
        conn.writeQueue.length = 0;
        return;
      }
      const json = conn.writeQueue.shift()!;
      try {
        const flushed = conn.socket.write(json);
        if (!flushed) {
          // Still backpressured — stop flushing, wait for next drain
          conn.draining = true;
          return;
        }
      } catch (err) {
        log(`Terminal flush error: ${(err as Error).message}`, "error");
        conn.writeQueue.length = 0;
        return;
      }
    }
  }

  /** If queue grows too large, the client is stuck — disconnect it. */
  private enforceQueueLimit(conn: TerminalConnection): void {
    if (conn.writeQueue.length > MAX_QUEUED_MESSAGES) {
      log(`Terminal write queue exceeded ${MAX_QUEUED_MESSAGES} — disconnecting stuck client`, "warn");
      conn.writeQueue.length = 0;
      conn.socket.destroy();
    }
  }
}
