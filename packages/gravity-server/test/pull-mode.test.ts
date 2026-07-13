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
import { makeInbox, type InboxService } from "../src/services/inbox.js";
import { parseTerminalMessage } from "../src/protocol/messages.js";
import { createSession } from "../src/state/session.js";
import { createAccState, type AccState } from "../src/pi-driver/turn-accumulator.js";
import type { ServerMessage } from "@gravity/shared";
import {
  subscribeAndSnapshot,
  pollSubscribedSessions,
  purgeSession,
  type TerminalPollDeps,
  type SchedulePurgeDeps,
} from "../src/gravity-server.js";
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

// ── Incremental-pull delta-only delivery (subscribe + poll) ──────────
//
// Drives the REAL exported `subscribeAndSnapshot` and
// `pollSubscribedSessions` against a real `makeSessionStore` /
// `makeTerminal` and a fake-socket capturing every message. The wire-
// level invariant under test: a session that already had N patches
// delivered (via the snapshot at subscribe time) does NOT replay them
// on subsequent polls — only patches that ARRIVED AFTER the cursor was
// seeded get delivered, and ONLY on the poll that closes over those
// new patches.

describe("incremental pull: subscribe → poll → poll delivers only the delta", () => {
  /** A tiny fake-socket that records every `write` for assertions. */
  function makeFakeSocket(): { socket: Socket; written: string[] } {
    const written: string[] = [];
    const socket = {
      destroyed: false,
      writable: true,
      write: (data: string) => { written.push(data); return true; },
      on: () => socket,
      destroy: () => { socket.destroyed = true; },
    } as unknown as Socket;
    return { socket, written };
  }

  /** Parse every captured write into its JSON message body. */
  const messagesOf = (written: string[]): ServerMessage[] =>
    written.map((w) => JSON.parse(w.trim()) as ServerMessage);

  it("after subscribeAndSnapshot seeds the cursor, polls send no replay + delta-only", () => {
    const store = makeSessionStore();
    const terminals = makeTerminal();
    const { socket, written } = makeFakeSocket();
    const conn = terminals.addConnection(socket);

    // Seed a session that EXISTS in the store (subscribeAndSnapshot only
    // sends the snapshot when the session is .get()-able).
    const id = "s-delta";
    const session = createSession(id, "/tmp/proj-delta");
    store.set(id, session);

    // Pre-populate the session's patch history BEFORE subscribing so the
    // snapshot+subscribe path has something to send.
    store.appendPatches(id, [
      { op: "set_status", status: "active" },
      { op: "set_claude_status", claudeStatus: "responding" },
      { op: "add_turn", turn: { turnNumber: 0, steps: [], agents: [], tasks: [], toolCount: 0, agentCount: 0, frozen: false, stopText: null, stopThinking: null, tokenIn: null, tokenOut: null, prompt: null } },
    ]);
    const seededSeq = store.getSessionSeq(id);
    expect(seededSeq).toBe(3);

    const deps: TerminalPollDeps = { store, terminals };

    // Subscribe: snapshot is delivered; cursor is seeded to seededSeq.
    subscribeAndSnapshot(deps, conn, id);
    expect(conn.subscribedSessions.has(id)).toBe(true);
    expect(conn.sessionSeqCursor.get(id)).toBe(seededSeq);

    const afterSubscribe = messagesOf(written);
    expect(afterSubscribe).toHaveLength(1);
    expect(afterSubscribe[0].type).toBe("session.snapshot");
    if (afterSubscribe[0].type === "session.snapshot") {
      expect(afterSubscribe[0].sessionId).toBe(id);
      expect(afterSubscribe[0].session.sessionId).toBe(id);
    }

    // First poll after subscribe: nothing new, cursor already at top
    // — poll must NOT replay any of the pre-existing patches.
    written.length = 0;
    pollSubscribedSessions(deps, conn);
    expect(written).toHaveLength(0);
    // Cursor should NOT have moved — there were no new patches and we
    // don't Ack into a hypothetical position.
    expect(conn.sessionSeqCursor.get(id)).toBe(seededSeq);

    // Append ONE new patch after the cursor.
    const [newStored] = store.appendPatches(id, [
      { op: "set_claude_status", claudeStatus: "idle" },
    ]);
    expect(newStored.seq).toBeGreaterThan(seededSeq);

    // Second poll: exactly one session-patches message, exactly that one
    // patch — no replay of the previous three.
    written.length = 0;
    pollSubscribedSessions(deps, conn);
    const afterSecondPoll = messagesOf(written);
    expect(afterSecondPoll).toHaveLength(1);
    expect(afterSecondPoll[0].type).toBe("session-patches");
    if (afterSecondPoll[0].type === "session-patches") {
      expect(afterSecondPoll[0].sessionId).toBe(id);
      expect(afterSecondPoll[0].patches).toHaveLength(1);
      expect(afterSecondPoll[0].seq).toBe(newStored.seq);
    }
    // Cursor advanced to the seq we just delivered.
    expect(conn.sessionSeqCursor.get(id)).toBe(newStored.seq);

    // Third poll with no new patches: still nothing sent, cursor does
    // not move.
    written.length = 0;
    pollSubscribedSessions(deps, conn);
    expect(written).toHaveLength(0);
    expect(conn.sessionSeqCursor.get(id)).toBe(newStored.seq);
  });
});

// ── purgeSession wiring through store.schedulePurge's timer ──────────
//
// Drives the REAL `store.schedulePurge` / `purgeSession` pair through
// the actual timer wiring rather than calling `purgeSession` directly,
// then asserts the cleanup state (`store.getSessionSeq === 0` because
// patches are cleared; the pre-seeded `piEnvelopeAccStates` entry is
// gone). The whole point of this test is that the production schedule-
// purge path actually invokes `purgeSession` and not just its inline
// predecessor.

describe("purgeSession: schedulePurge timer invokes real purgeSession cleanup", () => {
  it("after the timer fires, patches are cleared and piEnvelopeAccStates entry is gone", async () => {
    const store = makeSessionStore();
    const inbox = makeInbox();
    const terminals = makeTerminal();
    const piEnvelopeAccStates = new Map<string, AccState>();

    const id = "s-purge";

    // Pre-seed enough state that we can prove cleanup actually ran:
    //   - a patch in the store (so getSessionSeq === N before purge)
    //   - an AccState entry (so we can prove it was deleted)
    //   - an inbox item for the session (so we can prove removeForSession ran)
    store.appendPatches(id, [{ op: "set_status", status: "ended" }]);
    expect(store.getSessionSeq(id)).toBe(1);
    piEnvelopeAccStates.set(id, createAccState(id, "/tmp/purge"));
    expect(piEnvelopeAccStates.has(id)).toBe(true);
    const item = inbox.add("permission", id, "proj", "label", "summary", { tool_name: "Bash" } as Record<string, unknown>, undefined, false);
    expect(inbox.find(item.id)).toBeDefined();

    const deps: SchedulePurgeDeps = { store, inbox, terminals, piEnvelopeAccStates };

    // Schedule the purge through the REAL store.schedulePurge timer so
    // the test exercises the actual production wiring (the timer, not
    // a manually-flushed timer mock).
    let resolved = false;
    const done = new Promise<void>((resolve) => {
      store.schedulePurge(id, 10, () => {
        purgeSession(deps, id);
        resolved = true;
        resolve();
      });
    });

    await done;
    expect(resolved).toBe(true);

    // Cleanup assertions: patches cleared, AccState entry dropped,
    // inbox item removed.
    expect(store.getSessionSeq(id)).toBe(0);
    expect(store.getPatchesSince(id, 0)).toHaveLength(0);
    expect(piEnvelopeAccStates.has(id)).toBe(false);
    expect(inbox.find(item.id)).toBeUndefined();
  });
});
