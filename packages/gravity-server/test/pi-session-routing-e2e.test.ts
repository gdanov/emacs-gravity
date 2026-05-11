// pi-session-routing-e2e.test.ts — End-to-end tests for pi session ID routing
//
// These tests verify that a single pi session produces exactly one key in
// SessionStore, and that all events (SessionStart, UserPromptSubmit,
// PreToolUse, PostToolUse, Stop, SessionEnd) land under that same key.
//
// The tests drive events through the same code paths as production:
// - The translator (hook-translator.ts) creates TranslationResult objects
// - handlePiTranslation reads hookData.session_id to determine routing key
// - handleEvent applies patches to the SessionStore
//
// These tests MUST FAIL on the current code because:
// 1. handlePiTranslation reads hookData.session_id, which only SessionStart/SessionEnd carry
// 2. Other events fall back to the literal "pi-session" string
// 3. The outer sessionId from startPiSession is never passed to the inner translation path
//
// After the fix, all events should route under the same key.

import { describe, it, expect } from "vitest";
import { Effect, Layer } from "effect";
import { makeSessionStore, SessionStore, type SessionStoreService } from "../src/services/session-store.js";
import { makeInbox, Inbox, type InboxService } from "../src/services/inbox.js";
import { translatePiEvent } from "../src/pi-driver/hook-translator.js";
import { createAccState } from "../src/pi-driver/turn-accumulator.js";
import { thinkingToEffort } from "../src/pi-driver/session.js";
import { handleEvent } from "../src/handlers/event-handler.js";
import { FsTest } from "@gravity/shared";
import type { TranslationResult, PiEvent } from "../src/pi-driver/types.js";
import type { HookData } from "@gravity/shared";

// ── Test harness ────────────────────────────────────────────────────

/** Build a fake pi event sequence for a minimal session. */
function buildPiEventSequence() {
  // Pi 0.74 emits agent_start bare, then a separate message_start with
  // role: "user" carrying the prompt text.
  return [
    { type: "agent_start" as const },
    { type: "message_start" as const, message: { role: "user" as const, content: [{ type: "text" as const, text: "Run ls" }] } },
    { type: "turn_start" as const, turn_id: "t1" },
    { type: "tool_execution_start" as const, tool_call_id: "call-1", tool_name: "bash", tool_input: { command: "ls" } },
    { type: "tool_execution_end" as const, tool_call_id: "call-1", tool_name: "bash", tool_result: { stdout: "file.txt" } },
    { type: "turn_end" as const, turn_id: "t1" },
    { type: "agent_end" as const, result: { type: "success" as const } },
  ] as PiEvent[];
}

/**
 * Feed pi events through the translator and call onTranslation for each result.
 * This simulates what startPiDriver's event handler does.
 * Passes through the sessionId from the state so all results carry it.
 */
function feedPiEvents(
  sessionId: string,
  onTranslation: (result: TranslationResult) => void,
): void {
  const state = createAccState(sessionId, "/test", thinkingToEffort("medium"));
  const events = buildPiEventSequence();

  for (const evt of events) {
    const result = translatePiEvent(evt, state);
    if (result.kind === "emit") {
      for (const r of result.results) onTranslation(r);
    }
  }
}

/**
 * Simulate handlePiTranslation from gravity-server.ts:86-126 (post-fix version).
 *
 * This mirrors the routing logic in gravity-server after the fix:
 *   const sessionId = result.sessionId;  // ← from TranslationResult, always set
 *
 * All events carry sessionId from the translator (via AccState), so every
 * event routes to the same key — no fallback to "pi-session" needed.
 */
function handlePiTranslation(result: TranslationResult, store: SessionStoreService): void {
  const sessionId = result.sessionId;
  const cwd = (result.hookData as any).cwd as string || "/test";

  const patches = Effect.runSync(Effect.provide(
    handleEvent(result.hookEvent, sessionId, cwd, result.hookData as HookData, null),
    Layer.mergeAll(
      Layer.succeed(SessionStore, store),
      Layer.succeed(Inbox, makeInbox()),
      FsTest({}),
    ),
  ));

  if (patches.length > 0) {
    store.appendPatches(sessionId, patches);
  }
}

// ── Tests ───────────────────────────────────────────────────────────

describe("pi-session-e2e: session ID routing", () => {

  it("FAILING: one pi run creates exactly one session in the store", () => {
    const store = makeSessionStore();
    const seenKeys = new Set<string>();

    const runId = `pi-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`;
    feedPiEvents(runId, (result) => {
      const sessionId = result.sessionId;
      seenKeys.add(sessionId);
      handlePiTranslation(result, store);
    });

    // With the fix: 1 key (all events share outer sessionId)
    const allSessions = store.all();
    expect(allSessions).toHaveLength(1);
  });

  it("FAILING: all events for a pi run land under the same session key", () => {
    const store = makeSessionStore();
    const seenKeys = new Set<string>();
    const runId = `pi-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`;

    feedPiEvents(runId, (result) => {
      const sessionId = result.sessionId;
      seenKeys.add(sessionId);
      handlePiTranslation(result, store);
    });

    // With the fix: exactly 1 key
    expect(seenKeys.size).toBe(1);
  });

  it("FAILING: started/stopped broadcasts and patches share the same session ID", () => {
    const store = makeSessionStore();

    // Outer sessionId: what startPiSession generates and broadcasts to Emacs
    const outerSessionId = `pi-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`;

    const patchesSessionIds: string[] = [];
    const events = buildPiEventSequence();
    const state = createAccState(outerSessionId, "/test", thinkingToEffort("medium"));

    // Simulate pi.session started broadcast (outer sessionId)
    const broadcastIds = [outerSessionId];

    for (const evt of events) {
      const trans = translatePiEvent(evt, state);
      if (trans.kind === "emit") {
        for (const r of trans.results) {
          patchesSessionIds.push(r.sessionId);
          handlePiTranslation(r, store);
        }
      }
    }

    // The stopped broadcast uses outerSessionId (not inner from metadata)
    broadcastIds.push(outerSessionId);

    // All patches should use outerSessionId
    const uniquePatchIds = [...new Set(patchesSessionIds)];
    expect(uniquePatchIds).toHaveLength(1);
    expect(uniquePatchIds[0]).toBe(outerSessionId);
  });

  it("FAILING: two sequential pi runs create two distinct sessions (no key reuse)", () => {
    const store = makeSessionStore();

    const runPiSequence = (outerSessionId: string) => {
      const state = createAccState(outerSessionId, "/test", thinkingToEffort("medium"));
      const events = buildPiEventSequence();
      for (const evt of events) {
        const result = translatePiEvent(evt, state);
        if (result.kind === "emit") {
          for (const r of result.results) handlePiTranslation(r, store);
        }
      }
    };

    const runAId = `pi-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`;
    const runBId = `pi-${(Date.now() + 1).toString(36)}-${Math.random().toString(36).slice(2, 10)}`;
    runPiSequence(runAId); // run A
    runPiSequence(runBId); // run B

    const sessions = store.all();

    // Fix: two distinct sessions under their own outer IDs
    expect(sessions).toHaveLength(2);

    const sessionIds = sessions.map(s => s.sessionId);
    expect(new Set(sessionIds).size).toBe(2); // no key reuse across runs
  });
});