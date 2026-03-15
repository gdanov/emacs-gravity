// session-store.ts — Central session state management
//
// Map<sessionId, Session> with project grouping.
// All mutations emit semantic patches for connected terminals.

import type { Session, ProjectSummary } from "@gravity/shared";

export class SessionStore {
  private sessions = new Map<string, Session>();

  get(sessionId: string): Session | undefined {
    return this.sessions.get(sessionId);
  }

  set(sessionId: string, session: Session): void {
    this.sessions.set(sessionId, session);
  }

  delete(sessionId: string): boolean {
    return this.sessions.delete(sessionId);
  }

  has(sessionId: string): boolean {
    return this.sessions.has(sessionId);
  }

  /** Group sessions by project for overview. */
  getProjectSummaries(): ProjectSummary[] {
    const byProject = new Map<string, Session[]>();
    for (const session of this.sessions.values()) {
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

  /** All sessions as array. */
  all(): Session[] {
    return Array.from(this.sessions.values());
  }
}
