// session-store.ts — Session store as Effect service
//
// Replaces state/session-store.ts class with a ServiceMap service.
// Methods are synchronous (in-memory Map ops) — the service boundary provides DI.

import { Effect, Layer, ServiceMap } from "effect";
import type { Patch, Session, ProjectSummary, TurnNode } from "@gravity/shared";

export interface StoredPatch {
  readonly seq: number;
  readonly patch: Patch;
  readonly timestamp: number;
}

export interface SessionStoreService {
  readonly get: (sessionId: string) => Session | undefined;
  readonly set: (sessionId: string, session: Session) => void;
  readonly delete: (sessionId: string) => boolean;
  readonly has: (sessionId: string) => boolean;
  readonly getProjectSummaries: () => ProjectSummary[];
  readonly schedulePurge: (sessionId: string, delayMs: number, onPurge: () => void) => void;
  readonly cancelPurge: (sessionId: string) => void;
  readonly clearAllPurgeTimers: () => void;
  readonly all: () => Session[];

  // Patch store (pull mode): append patches and retrieve since a sequence number
  readonly appendPatches: (sessionId: string, patches: Patch[]) => StoredPatch[];
  readonly getPatchesSince: (sessionId: string, since: number) => StoredPatch[];
  readonly getSessionSeq: (sessionId: string) => number;
  readonly clearPatches: (sessionId: string) => void;
}

export const SessionStore = ServiceMap.Service<SessionStoreService>("SessionStore");

/** Extract the latest assistant message from a session's turn tree. */
function extractLatestMessage(s: Session): string | null {
  if (s.streamingText) return s.streamingText;
  for (let i = s.turns.length - 1; i >= 0; i--) {
    const turn: TurnNode = s.turns[i];
    if (turn.stopText) return turn.stopText;
    for (let j = turn.steps.length - 1; j >= 0; j--) {
      const step = turn.steps[j];
      for (let k = step.tools.length - 1; k >= 0; k--) {
        if (step.tools[k].postText) return step.tools[k].postText;
      }
      if (step.text) return step.text;
    }
  }
  return null;
}

/** Extract the latest user prompt from a session's turn tree. */
function extractLatestUserPrompt(s: Session): string | null {
  for (let i = s.turns.length - 1; i >= 0; i--) {
    if (s.turns[i].prompt?.text) return s.turns[i].prompt!.text;
  }
  return null;
}

export function makeSessionStore(): SessionStoreService {
  const sessions = new Map<string, Session>();
  const purgeTimers = new Map<string, ReturnType<typeof setTimeout>>();
  // Patch history per session for pull mode
  const patchHistories = new Map<string, StoredPatch[]>();
  // Global sequence counter
  let globalSeq = 0;

  const cancelPurge = (sessionId: string): void => {
    const timer = purgeTimers.get(sessionId);
    if (timer) {
      clearTimeout(timer);
      purgeTimers.delete(sessionId);
    }
  };

  return {
    get: (sessionId) => sessions.get(sessionId),

    set: (sessionId, session) => { sessions.set(sessionId, session); },

    delete: (sessionId) => {
      cancelPurge(sessionId);
      return sessions.delete(sessionId);
    },

    has: (sessionId) => sessions.has(sessionId),

    getProjectSummaries: () => {
      const byProject = new Map<string, Session[]>();
      for (const session of sessions.values()) {
        if (session.status === "ended") continue;
        const list = byProject.get(session.project) ?? [];
        list.push(session);
        byProject.set(session.project, list);
      }
      return Array.from(byProject.entries()).map(([project, ss]) => ({
        project,
        sessions: ss.map((s) => ({
          sessionId: s.sessionId,
          slug: s.slug,
          displayName: s.displayName,
          status: s.status,
          claudeStatus: s.claudeStatus,
          toolCount: s.totalToolCount,
          lastEventTime: s.lastEventTime,
          latestMessage: extractLatestMessage(s),
          latestUserPrompt: extractLatestUserPrompt(s),
        })),
      }));
    },

    schedulePurge: (sessionId, delayMs, onPurge) => {
      cancelPurge(sessionId);
      const timer = setTimeout(() => {
        purgeTimers.delete(sessionId);
        onPurge();
      }, delayMs);
      purgeTimers.set(sessionId, timer);
    },

    cancelPurge,

    clearAllPurgeTimers: () => {
      for (const timer of purgeTimers.values()) {
        clearTimeout(timer);
      }
      purgeTimers.clear();
    },

    all: () => Array.from(sessions.values()),

    appendPatches: (sessionId, patches) => {
      const history = patchHistories.get(sessionId) ?? [];
      const now = Date.now();
      const stored: StoredPatch[] = patches.map((patch) => ({
        seq: ++globalSeq,
        patch,
        timestamp: now,
      }));
      history.push(...stored);
      patchHistories.set(sessionId, history);
      return stored;
    },

    getPatchesSince: (sessionId, since) => {
      const history = patchHistories.get(sessionId) ?? [];
      // Binary search for first patch with seq > since
      let lo = 0;
      let hi = history.length;
      while (lo < hi) {
        const mid = (lo + hi) >>> 1;
        if (history[mid].seq <= since) {
          lo = mid + 1;
        } else {
          hi = mid;
        }
      }
      return history.slice(lo);
    },

    getSessionSeq: (sessionId) => {
      const history = patchHistories.get(sessionId) ?? [];
      return history.length > 0 ? history[history.length - 1].seq : 0;
    },

    clearPatches: (sessionId) => {
      patchHistories.delete(sessionId);
    },
  };
}

export const SessionStoreLive = Layer.succeed(SessionStore, makeSessionStore());

export const SessionStoreTest = () => Layer.succeed(SessionStore, makeSessionStore());
