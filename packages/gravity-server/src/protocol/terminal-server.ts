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

    return conn;
  }

  /** Broadcast a message to all connected terminals. */
  broadcast(message: ServerMessage): void {
    const json = JSON.stringify(message) + "\n";
    for (const conn of this.connections) {
      try {
        conn.socket.write(json);
      } catch {
        // Connection dead — will be cleaned up on close event
      }
    }
  }

  /** Send a message to terminals subscribed to a specific session. */
  sendToSubscribers(sessionId: string, message: ServerMessage): void {
    const json = JSON.stringify(message) + "\n";
    for (const conn of this.connections) {
      if (conn.subscribedSessions.has(sessionId)) {
        try {
          conn.socket.write(json);
        } catch {
          // Connection dead
        }
      }
    }
  }

  /** Send a message to a specific connection. */
  sendTo(conn: TerminalConnection, message: ServerMessage): void {
    try {
      conn.socket.write(JSON.stringify(message) + "\n");
    } catch {
      // Connection dead
    }
  }

  /** Number of connected terminals. */
  get connectionCount(): number {
    return this.connections.length;
  }
}
