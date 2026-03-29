// terminal-server.test.ts — Tests for TerminalServer dead-connection handling
//
// Verifies that broadcast, sendToSubscribers, and sendTo silently skip
// connections whose sockets are destroyed or not writable (EPIPE prevention).

import { describe, it, expect } from "vitest";
import type { Socket } from "net";
import { makeTerminal, type TerminalService } from "../src/services/terminal.js";
import type { ServerMessage } from "@gravity/shared";

/** Create a mock socket with controllable destroyed/writable state. */
function mockSocket(opts?: { destroyed?: boolean; writable?: boolean }): Socket & {
  written: string[];
} {
  const buf: string[] = [];
  const sock: Record<string, unknown> = {
    written: buf,
    destroyed: opts?.destroyed ?? false,
    writable: opts?.writable ?? true,
    write(data: string) {
      buf.push(data);
      return true;
    },
    on(_event: string, _cb: (...args: unknown[]) => void) {
      return sock;
    },
    destroy() {
      sock.destroyed = true;
      sock.writable = false;
    },
  };
  return sock as unknown as Socket & { written: string[] };
}

const sampleMessage: ServerMessage = {
  type: "overview.snapshot",
  projects: [],
};

describe("TerminalServer: dead connection handling", () => {
  describe("broadcast", () => {
    it("skips connections with destroyed sockets", () => {
      const ts = makeTerminal();
      const deadSocket = mockSocket({ destroyed: true });
      const liveSocket = mockSocket();
      ts.addConnection(deadSocket);
      ts.addConnection(liveSocket);

      // Should not throw
      ts.broadcast(sampleMessage);

      expect(deadSocket.written).toHaveLength(0);
      expect(liveSocket.written).toHaveLength(1);
    });

    it("skips connections with non-writable sockets", () => {
      const ts = makeTerminal();
      const nonWritable = mockSocket({ writable: false });
      const liveSocket = mockSocket();
      ts.addConnection(nonWritable);
      ts.addConnection(liveSocket);

      ts.broadcast(sampleMessage);

      expect(nonWritable.written).toHaveLength(0);
      expect(liveSocket.written).toHaveLength(1);
    });
  });

  describe("sendToSubscribers", () => {
    it("skips subscribed connections with destroyed sockets", () => {
      const ts = makeTerminal();
      const deadSocket = mockSocket({ destroyed: true });
      const liveSocket = mockSocket();
      const deadConn = ts.addConnection(deadSocket);
      const liveConn = ts.addConnection(liveSocket);
      deadConn.subscribedSessions.add("s1");
      liveConn.subscribedSessions.add("s1");

      ts.sendToSubscribers("s1", sampleMessage);

      expect(deadSocket.written).toHaveLength(0);
      expect(liveSocket.written).toHaveLength(1);
    });

    it("skips subscribed connections with non-writable sockets", () => {
      const ts = makeTerminal();
      const nonWritable = mockSocket({ writable: false });
      const liveSocket = mockSocket();
      const nwConn = ts.addConnection(nonWritable);
      const liveConn = ts.addConnection(liveSocket);
      nwConn.subscribedSessions.add("s1");
      liveConn.subscribedSessions.add("s1");

      ts.sendToSubscribers("s1", sampleMessage);

      expect(nonWritable.written).toHaveLength(0);
      expect(liveSocket.written).toHaveLength(1);
    });
  });

  describe("sendTo", () => {
    it("does not write to a destroyed socket", () => {
      const ts = makeTerminal();
      const deadSocket = mockSocket({ destroyed: true });
      const conn = ts.addConnection(deadSocket);

      // Should not throw
      ts.sendTo(conn, sampleMessage);

      expect(deadSocket.written).toHaveLength(0);
    });

    it("does not write to a non-writable socket", () => {
      const ts = makeTerminal();
      const nonWritable = mockSocket({ writable: false });
      const conn = ts.addConnection(nonWritable);

      ts.sendTo(conn, sampleMessage);

      expect(nonWritable.written).toHaveLength(0);
    });
  });
});
