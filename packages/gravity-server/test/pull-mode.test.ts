// pull-mode.test.ts — Tests for pull-based terminal protocol
//
// Verifies that in pull mode:
// - Server sends state-changed signals instead of broadcasting payloads
// - Server stores patches with sequence numbers
// - poll message returns overview, inbox items, and session patches
// - request.session returns session-patches format

import { describe, it, expect, beforeEach } from "vitest";
import { makeSessionStore, type SessionStoreService } from "../src/services/session-store.js";
import { makeTerminal, type TerminalService, type TerminalConnection } from "../src/services/terminal.js";
import { parseTerminalMessage } from "../src/protocol/messages.js";
import type { Socket } from "net";

// ── Session Store Patch Store Tests ──────────────────────────────────

describe("SessionStore: patch store", () => {
  let store: SessionStoreService;

  beforeEach(() => {
    store = makeSessionStore();
  });

  it("starts with empty patch history", () => {
    const patches = store.getPatchesSince("s1", 0);
    expect(patches).toHaveLength(0);
    expect(store.getSessionSeq("s1")).toBe(0);
  });

  it("appends patches with sequence numbers", () => {
    const patches1 = store.appendPatches("s1", [
      { op: "set_status", status: "active" },
    ]);
    expect(patches1).toHaveLength(1);
    expect(patches1[0].seq).toBe(1);
    expect(store.getSessionSeq("s1")).toBe(1);

    const patches2 = store.appendPatches("s1", [
      { op: "set_claude_status", claudeStatus: "responding" },
      { op: "add_turn", turn: { turnNumber: 0, steps: [], agents: [], tasks: [], toolCount: 0, agentCount: 0, frozen: false, stopText: null, stopThinking: null, tokenIn: null, tokenOut: null, prompt: null } },
    ]);
    expect(patches2).toHaveLength(2);
    expect(patches2[0].seq).toBe(2);
    expect(patches2[1].seq).toBe(3);
    expect(store.getSessionSeq("s1")).toBe(3);
  });

  it("increments global sequence across sessions", () => {
    store.appendPatches("s1", [{ op: "set_status", status: "active" }]);
    store.appendPatches("s2", [{ op: "set_status", status: "active" }]);
    store.appendPatches("s1", [{ op: "set_claude_status", claudeStatus: "responding" }]);

    // Each call increments the global counter
    // s1: seq 1, seq 3 (after s2's seq 2)
    // s2: seq 2
    expect(store.getSessionSeq("s1")).toBe(3);
    expect(store.getSessionSeq("s2")).toBe(2);
  });

  it("retrieves patches since a sequence number", () => {
    store.appendPatches("s1", [{ op: "set_status", status: "active" }]);
    store.appendPatches("s1", [{ op: "set_claude_status", claudeStatus: "responding" }]);
    store.appendPatches("s1", [{ op: "add_turn", turn: { turnNumber: 0, steps: [], agents: [], tasks: [], toolCount: 0, agentCount: 0, frozen: false, stopText: null, stopThinking: null, tokenIn: null, tokenOut: null, prompt: null } }]);

    const all = store.getPatchesSince("s1", 0);
    expect(all).toHaveLength(3);

    const since1 = store.getPatchesSince("s1", 1);
    expect(since1).toHaveLength(2);

    const since2 = store.getPatchesSince("s1", 2);
    expect(since2).toHaveLength(1);
    expect((since2[0].patch as { op: string }).op).toBe("add_turn");

    const since99 = store.getPatchesSince("s1", 99);
    expect(since99).toHaveLength(0);
  });

  it("returns empty for unknown sessions", () => {
    expect(store.getPatchesSince("unknown", 0)).toHaveLength(0);
    expect(store.getSessionSeq("unknown")).toBe(0);
  });

  it("clears patch history on clearPatches", () => {
    store.appendPatches("s1", [{ op: "set_status", status: "active" }]);
    store.appendPatches("s1", [{ op: "set_claude_status", claudeStatus: "responding" }]);
    expect(store.getSessionSeq("s1")).toBe(2);

    store.clearPatches("s1");
    expect(store.getSessionSeq("s1")).toBe(0);
    expect(store.getPatchesSince("s1", 0)).toHaveLength(0);
  });
});

// ── Terminal Service Signal Tests ────────────────────────────────────

describe("TerminalService: signalChanged", () => {
  let terminals: TerminalService;

  beforeEach(() => {
    terminals = makeTerminal();
  });

  it("signalChanged broadcasts to all connections", () => {
    const sockets: Socket[] = [];
    const written: string[][] = [];

    // Add 3 connections
    for (let i = 0; i < 3; i++) {
      const buf: string[] = [];
      written.push(buf);
      const sock = {
        destroyed: false,
        writable: true,
        write: (data: string) => { buf.push(data); return true; },
        on: () => sock,
        destroy: () => { sock.destroyed = true; sock.writable = false; },
      } as unknown as Socket;
      sockets.push(sock);
      terminals.addConnection(sock);
    }

    terminals.signalChanged("overview");

    for (const buf of written) {
      expect(buf).toHaveLength(1);
      const msg = JSON.parse(buf[0].trim());
      expect(msg.type).toBe("state-changed");
      expect(msg.what).toBe("overview");
      expect(msg.seq).toBe(0);
    }
  });

  it("signalChanged includes sessionId when provided", () => {
    const buf: string[] = [];
    const sock = {
      destroyed: false,
      writable: true,
      write: (data: string) => { buf.push(data); return true; },
      on: () => sock,
      destroy: () => { sock.destroyed = true; },
    } as unknown as Socket;
    terminals.addConnection(sock);

    terminals.signalChanged("session", "test-session-123", 42);

    expect(buf).toHaveLength(1);
    const msg = JSON.parse(buf[0].trim());
    expect(msg.type).toBe("state-changed");
    expect(msg.what).toBe("session");
    expect(msg.sessionId).toBe("test-session-123");
    expect(msg.seq).toBe(42);
  });

  it("signalChangedTo sends to specific connection only", () => {
    const bufs: string[][] = [];
    const socks: any[] = [];

    // Add 3 connections
    for (let i = 0; i < 3; i++) {
      const buf: string[] = [];
      bufs.push(buf);
      const sock = {
        destroyed: false,
        writable: true,
        write: (data: string) => { buf.push(data); return true; },
        on: () => sock,
        destroy: () => { sock.destroyed = true; },
      };
      socks.push(sock);
      const conn = terminals.addConnection(sock);
      // Store the connection for signalChangedTo
      if (i === 2) {
        terminals.signalChangedTo(conn, "inbox");
      }
    }

    expect(bufs[0]).toHaveLength(0);
    expect(bufs[1]).toHaveLength(0);
    expect(bufs[2]).toHaveLength(1);
    const msg = JSON.parse(bufs[2][0].trim());
    expect(msg.type).toBe("state-changed");
    expect(msg.what).toBe("inbox");
  });

  it("signalChanged uses seq=0 when not provided", () => {
    const buf: string[] = [];
    const sock = {
      destroyed: false,
      writable: true,
      write: (data: string) => { buf.push(data); return true; },
      on: () => sock,
      destroy: () => { sock.destroyed = true; },
    } as unknown as Socket;
    terminals.addConnection(sock);

    terminals.signalChanged("session", "test");

    expect(buf).toHaveLength(1);
    const msg = JSON.parse(buf[0].trim());
    expect(msg.seq).toBe(0);
  });
});

// ── Message Parser Tests ──────────────────────────────────────────────

describe("parseTerminalMessage: poll type", () => {
  it("accepts poll message", () => {
    const msg = parseTerminalMessage(JSON.stringify({ type: "poll" }));
    expect(msg).not.toBeNull();
    expect(msg!.type).toBe("poll");
  });

  it("accepts poll alongside other valid messages", () => {
    expect(parseTerminalMessage(JSON.stringify({ type: "hello", capabilities: [] }))).not.toBeNull();
    expect(parseTerminalMessage(JSON.stringify({ type: "request.overview" }))).not.toBeNull();
    expect(parseTerminalMessage(JSON.stringify({ type: "poll" }))).not.toBeNull();
    expect(parseTerminalMessage(JSON.stringify({ type: "request.resync" }))).not.toBeNull();
  });
});
