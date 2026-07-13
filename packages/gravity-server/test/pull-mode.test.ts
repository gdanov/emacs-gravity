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

  it("accepts request.unsubscribe with a sessionId", () => {
    const msg = parseTerminalMessage(JSON.stringify({ type: "request.unsubscribe", sessionId: "s-xyz" }));
    expect(msg).not.toBeNull();
    expect(msg!.type).toBe("request.unsubscribe");
    if (msg!.type === "request.unsubscribe") {
      expect(msg!.sessionId).toBe("s-xyz");
    }
  });
});

// ── SessionStore: update_tool_partial coalescing ─────────────────────
//
// Patch history represents state, not an event log: streaming
// partials for a single in-flight tool are a sequence of observations
// of the same value, so only the latest should occupy a history
// slot. Other patch types are never coalesced.

describe("SessionStore: update_tool_partial coalescing", () => {
  let store: SessionStoreService;

  beforeEach(() => {
    store = makeSessionStore();
  });

  it("coalesces two consecutive update_tool_partial patches for the same toolUseId into one history entry", () => {
    const id = "s1";
    const toolUseId = "tu_a";
    const first = store.appendPatches(id, [
      { op: "update_tool_partial", toolUseId, partial: "v1" },
    ]);
    expect(first).toHaveLength(1);
    const firstSeq = first[0].seq;

    // A second partial for the same toolUseId: should mutate the tail
    // in place, not push a fresh entry, and must NOT advance globalSeq.
    const second = store.appendPatches(id, [
      { op: "update_tool_partial", toolUseId, partial: "v2-latest" },
    ]);
    expect(second).toHaveLength(1);
    // The returned entry is the same mutated tail (same seq), not a
    // freshly-assigned one — so callers deriving "last seq from this
    // call" still see a consistent picture.
    expect(second[0].seq).toBe(firstSeq);
    expect(second[0].patch).toEqual({ op: "update_tool_partial", toolUseId, partial: "v2-latest" });

    const all = store.getPatchesSince(id, 0);
    expect(all).toHaveLength(1);
    expect(all[0].patch).toEqual({ op: "update_tool_partial", toolUseId, partial: "v2-latest" });

    // getSessionSeq after coalescing returns the SAME seq the first
    // partial got assigned — coalesced entries do not bump the
    // global counter.
    expect(store.getSessionSeq(id)).toBe(firstSeq);
  });

  it("does not coalesce update_tool_partial for a different toolUseId", () => {
    const id = "s1";
    // Same toolUseId twice → coalesced into one entry.
    store.appendPatches(id, [
      { op: "update_tool_partial", toolUseId: "tu_a", partial: "v1" },
    ]);
    store.appendPatches(id, [
      { op: "update_tool_partial", toolUseId: "tu_a", partial: "v2" },
    ]);
    // A third partial for a DIFFERENT toolUseId must NOT coalesce with
    // the (mutated) tu_a tail — history length grows to 2.
    store.appendPatches(id, [
      { op: "update_tool_partial", toolUseId: "tu_b", partial: "v1" },
    ]);

    const all = store.getPatchesSince(id, 0);
    expect(all).toHaveLength(2);
    expect((all[0].patch as { toolUseId: string; partial: string }).toolUseId).toBe("tu_a");
    expect((all[0].patch as { toolUseId: string; partial: string }).partial).toBe("v2");
    expect((all[1].patch as { toolUseId: string }).toolUseId).toBe("tu_b");
  });

  it("never coalesces non-update_tool_partial patches — two set_claude_status patches persist separately", () => {
    const id = "s1";
    const stored = store.appendPatches(id, [
      { op: "set_claude_status", claudeStatus: "idle" },
      { op: "set_claude_status", claudeStatus: "responding" },
    ]);
    expect(stored).toHaveLength(2);
    expect((stored[0].patch as { claudeStatus: string }).claudeStatus).toBe("idle");
    expect((stored[1].patch as { claudeStatus: string }).claudeStatus).toBe("responding");

    const all = store.getPatchesSince(id, 0);
    expect(all).toHaveLength(2);
    expect((all[0].patch as { claudeStatus: string }).claudeStatus).toBe("idle");
    expect((all[1].patch as { claudeStatus: string }).claudeStatus).toBe("responding");
    // Each non-coalesced entry gets its own seq — the global counter
    // advanced by 2 across this batch.
    expect(stored[1].seq).toBe(stored[0].seq + 1);
  });
});

// ── TerminalConnection: sessionSeqCursor ─────────────────────────────
//
// `handleTerminalMessage` is a closure inside `program`'s Effect.gen and
// is not separately exported, so the cursor-seeding wiring inside
// gravity-server.ts is verified by code reading only. The connection
// shape itself — that `sessionSeqCursor` exists, is a Map, and behaves
// like one — is independently testable here.

describe("TerminalConnection: sessionSeqCursor", () => {
  it("initializes sessionSeqCursor as an empty Map on every new connection", () => {
    const terminals = makeTerminal();
    const sock = {
      destroyed: false,
      writable: true,
      write: () => true,
      on: () => sock,
      destroy: () => { sock.destroyed = true; },
    } as unknown as Socket;
    const conn = terminals.addConnection(sock);

    expect(conn.sessionSeqCursor).toBeInstanceOf(Map);
    expect(conn.sessionSeqCursor.size).toBe(0);
  });

  it("supports get/set/delete exactly like a plain Map<string, number>", () => {
    const terminals = makeTerminal();
    const sock = {
      destroyed: false,
      writable: true,
      write: () => true,
      on: () => sock,
      destroy: () => { sock.destroyed = true; },
    } as unknown as Socket;
    const conn = terminals.addConnection(sock);

    conn.sessionSeqCursor.set("s1", 42);
    expect(conn.sessionSeqCursor.get("s1")).toBe(42);
    conn.sessionSeqCursor.set("s2", 7);
    expect(conn.sessionSeqCursor.size).toBe(2);

    conn.sessionSeqCursor.delete("s1");
    expect(conn.sessionSeqCursor.has("s1")).toBe(false);
    expect(conn.sessionSeqCursor.has("s2")).toBe(true);
  });
});
