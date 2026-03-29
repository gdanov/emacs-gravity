// session-store.ts — Central session state management
//
// Map<sessionId, Session> with project grouping.
// All mutations emit semantic patches for connected terminals.

import type { Session, ProjectSummary, TurnNode } from "@gravity/shared";

export class SessionStore {
  private sessions = new Map<string, Session>();
  private purgeTimers = new Map<string, ReturnType<typeof setTimeout>>();

  get(sessionId: string): Session | undefined {
    return this.sessions.get(sessionId);
  }

  set(sessionId: string, session: Session): void {
    this.sessions.set(sessionId, session);
  }

  delete(sessionId: string): boolean {
    this.cancelPurge(sessionId);
    return this.sessions.delete(sessionId);
  }

  has(sessionId: string): boolean {
    return this.sessions.has(sessionId);
  }

  /** Group active sessions by project for overview (excludes ended sessions). */
  getProjectSummaries(): ProjectSummary[] {
    const byProject = new Map<string, Session[]>();
    for (const session of this.sessions.values()) {
      if (session.status === "ended") continue;
      const list = byProject.get(session.project) ?? [];
      list.push(session);
      byProject.set(session.project, list);
    }
    return Array.from(byProject.entries()).map(([project, sessions]) => ({
      project,
      sessions: sessions.map((s) => ({
        sessionId: s.sessionId,
        slug: s.slug,
        displayName: s.displayName,
        status: s.status,
        claudeStatus: s.claudeStatus,
        toolCount: s.totalToolCount,
        lastEventTime: s.lastEventTime,
        latestMessage: extractLatestMessage(s),
      })),
    }));
  }

  /** Schedule purge of a session after a delay. Resets if already scheduled. */
  schedulePurge(sessionId: string, delayMs: number, onPurge: () => void): void {
    this.cancelPurge(sessionId);
    const timer = setTimeout(() => {
      this.purgeTimers.delete(sessionId);
      onPurge();
    }, delayMs);
    this.purgeTimers.set(sessionId, timer);
  }

  /** Cancel a scheduled purge. */
  cancelPurge(sessionId: string): void {
    const timer = this.purgeTimers.get(sessionId);
    if (timer) {
      clearTimeout(timer);
      this.purgeTimers.delete(sessionId);
    }
  }

  /** Clear all purge timers (for shutdown). */
  clearAllPurgeTimers(): void {
    for (const timer of this.purgeTimers.values()) {
      clearTimeout(timer);
    }
    this.purgeTimers.clear();
  }

  /** All sessions as array. */
  all(): Session[] {
    return Array.from(this.sessions.values());
  }
}

/** Extract the latest assistant message from a session's turn tree. */
function extractLatestMessage(s: Session): string | null {
  // Prefer streaming text if currently responding
  if (s.streamingText) return s.streamingText;

  // Walk turns backward to find the most recent text
  for (let i = s.turns.length - 1; i >= 0; i--) {
    const turn: TurnNode = s.turns[i];

    // Turn-level stop text (final assistant message after all tools)
    if (turn.stopText) return turn.stopText;

    // Last step's assistant text
    for (let j = turn.steps.length - 1; j >= 0; j--) {
      const step = turn.steps[j];
      // Check post-text on the last tool in the step
      for (let k = step.tools.length - 1; k >= 0; k--) {
        if (step.tools[k].postText) return step.tools[k].postText;
      }
      // Step-level assistant text
      if (step.text) return step.text;
    }
  }

  return null;
}
