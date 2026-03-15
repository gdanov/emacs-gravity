// session-store.ts — Central session state management
//
// Map<sessionId, Session> with project grouping.
// All mutations emit semantic patches for connected terminals.

import type { Session, ProjectSummary } from "@gravity/shared";

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
        status: s.status,
        claudeStatus: s.claudeStatus,
        toolCount: s.totalToolCount,
        lastEventTime: s.lastEventTime,
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
