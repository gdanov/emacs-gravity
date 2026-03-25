// terminal-server.ts — Accept terminal connections, dispatch messages
//
// Manages long-lived connections from terminals (Emacs, web, etc.)
// Sends snapshots on connect, patches on state changes, inbox events.

import type { ServerMessage, TerminalMessage } from "@gravity/shared";
import type { Socket } from "net";

export interface TerminalConnection {
  socket: Socket;
  subscribedSessions: Set<string>;
}

export class TerminalServer {
  private connections: TerminalConnection[] = [];

  addConnection(socket: Socket): TerminalConnection {
    const conn: TerminalConnection = {
      socket,
      subscribedSessions: new Set(),
    };
    this.connections.push(conn);

    socket.on("close", () => {
      this.connections = this.connections.filter((c) => c !== conn);
    });

    socket.on("error", (err) => {
      console.error(`Terminal connection error: ${err.message}`);
      socket.destroy();
    });

    return conn;
  }

  /** Broadcast a message to all connected terminals. */
  broadcast(message: ServerMessage): void {
    const json = JSON.stringify(message) + "\n";
    // Iterate over a copy to allow mutation during iteration
    for (const conn of [...this.connections]) {
      if (conn.socket.destroyed || !conn.socket.writable) continue;
      try {
        conn.socket.write(json);
      } catch (err) {
        console.error(`Terminal broadcast write error: ${(err as Error).message}`);
        conn.socket.destroy();
      }
    }
  }

  /** Send a message to terminals subscribed to a specific session. */
  sendToSubscribers(sessionId: string, message: ServerMessage): void {
    const json = JSON.stringify(message) + "\n";
    for (const conn of [...this.connections]) {
      if (conn.subscribedSessions.has(sessionId)) {
        if (conn.socket.destroyed || !conn.socket.writable) continue;
        try {
          conn.socket.write(json);
        } catch (err) {
          console.error(`Terminal subscriber write error: ${(err as Error).message}`);
          conn.socket.destroy();
        }
      }
    }
  }

  /** Send a message to a specific connection. */
  sendTo(conn: TerminalConnection, message: ServerMessage): void {
    if (conn.socket.destroyed || !conn.socket.writable) return;
    try {
      conn.socket.write(JSON.stringify(message) + "\n");
    } catch (err) {
      console.error(`Terminal sendTo write error: ${(err as Error).message}`);
      conn.socket.destroy();
    }
  }

  /** Remove a session from all connections' subscriptions. */
  unsubscribeAll(sessionId: string): void {
    for (const conn of this.connections) {
      conn.subscribedSessions.delete(sessionId);
    }
  }

  /** Number of connected terminals. */
  get connectionCount(): number {
    return this.connections.length;
  }
}
