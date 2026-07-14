// logic.ts — pure, DOM-free helpers for the gravity browser client.
//
// `shouldSyncPoll` decides whether a server message must immediately
// trigger a synchronous `poll` reply (background-tab timer throttling
// otherwise stalls every scheduled callback, so the poll must go out
// inside `ws.onmessage`). The reducer / classifier / grouper live here
// so a Node-side vitest can import them under no-DOM conditions.

import type {
  InboxItem,
  Patch,
  ProjectSummary,
  Session,
  StepNode,
} from "@gravity/shared";

export function shouldSyncPoll(msg: { type: string }): boolean {
  return msg.type === "state-changed";
}

/**
 * Bucket an inbox item into one of three render categories so the UI can
 * apply the right styling without ever having to re-implement the rules.
 * - "attention" overrides everything else (it is a distinct notification
 *   shape, per the plan's "Attention / non-actionable inbox rendering"
 *   pinned decision).
 * - "actionable" means the item is currently waiting on a yes/no answer
 *   from an action-capable terminal. The browser client never sends
 *   `action.*` messages, so this only governs CSS / iconography, not
 *   which buttons are wired up.
 * - "view-only" covers everything else: items the browser can SEE but
 *   not answer (actionable=false) and items that were never actionable
 *   to begin with (e.g. `idle` notifications).
 */
export function classifyInboxItem(
  item: InboxItem,
): "actionable" | "view-only" | "attention" {
  if (item.type === "attention") return "attention";
  if (item.actionable) return "actionable";
  return "view-only";
}

/**
 * Identity passthrough. The server already groups `SessionSummary[]` into
 * `ProjectSummary[]` keyed by `repoKey` (worktrees unify with their main
 * checkout server-side), so the client renders `project.repoKey` as the
 * group header and `project.sessions[]` as rows. This function exists as
 * a documented seam so a future client-side re-grouping has a single,
 * testable entry point instead of getting scattered across the renderer.
 */
export function groupedFleet(projects: ProjectSummary[]): ProjectSummary[] {
  return projects;
}

/**
 * Reduce one server patch onto a local session copy. The server holds the
 * canonical session; the browser keeps a renderable copy and applies
 * patches to it. Unhandled ops return the input unchanged so the caller
 * can detect the "I don't know this op, re-request a full snapshot" case
 * via reference equality.
 *
 * Handled ops are the subset a Phase-B drill-down actually needs:
 * `add_turn`, `add_step`, `add_tool`, `complete_tool`, `add_agent`,
 * `complete_agent`, `set_turn_stop`, `set_claude_status`. Everything else
 * is the protocol's contract for "opaque to me, please re-snapshot" per
 * the plan's "Drill-down" pinned row — explicitly NOT a bug to "fix" by
 * special-casing more ops here.
 */
export function applyPatch(session: Session, patch: Patch): Session {
  switch (patch.op) {
    case "add_turn": {
      if (session.turns.some((t) => t.turnNumber === patch.turn.turnNumber)) {
        return session;
      }
      const next = cloneSession(session);
      next.turns.push(cloneTurn(patch.turn));
      next.currentTurn = patch.turn.turnNumber;
      return next;
    }

    case "freeze_turn": {
      const target = session.turns.find((t) => t.turnNumber === patch.turnNumber);
      if (!target) return session;
      const next = cloneSession(session);
      const nt = next.turns.find((t) => t.turnNumber === patch.turnNumber);
      if (nt) nt.frozen = true;
      return next;
    }

    case "add_step": {
      const target = findTurn(session, patch.turnNumber);
      if (!target) return session;
      const next = cloneSession(session);
      const nt = next.turns.find((t) => t.turnNumber === patch.turnNumber);
      if (nt) nt.steps.push(cloneStep(patch.step));
      return next;
    }

    case "add_tool": {
      const target = findTurn(session, patch.turnNumber);
      if (!target) return session;
      if (patch.tool.toolUseId && session.toolIndex[patch.tool.toolUseId]) {
        return session;
      }
      const next = cloneSession(session);
      const nt = next.turns.find((t) => t.turnNumber === patch.turnNumber);
      if (!nt) return session;
      const step = nt.steps[patch.stepIndex] ?? nt.steps[nt.steps.length - 1];
      if (!step) return session;
      const tool = cloneTool(patch.tool);
      step.tools.push(tool);
      nt.toolCount += 1;
      next.totalToolCount += 1;
      if (tool.toolUseId) {
        next.toolIndex[tool.toolUseId] = {
          turnNumber: nt.turnNumber,
          stepIndex: nt.steps.indexOf(step),
          toolIndex: step.tools.length - 1,
          agentId: null,
        };
      }
      return next;
    }

    case "complete_tool": {
      const loc = session.toolIndex[patch.toolUseId];
      if (!loc) return session;
      const next = cloneSession(session);
      const nloc = next.toolIndex[patch.toolUseId];
      if (!nloc) return session;
      const turn = next.turns.find((t) => t.turnNumber === nloc.turnNumber);
      if (!turn) return session;
      const step = turn.steps[nloc.stepIndex];
      if (!step) return session;
      const tool = step.tools[nloc.toolIndex];
      if (!tool) return session;
      tool.status = patch.status;
      tool.result = patch.result;
      if (typeof patch.duration === "number") tool.duration = patch.duration;
      if (typeof patch.postText === "string") tool.postText = patch.postText;
      if (typeof patch.postThinking === "string") tool.postThinking = patch.postThinking;
      return next;
    }

    case "add_agent": {
      if (session.agentIndex[patch.agent.agentId]) return session;
      const turn = session.turns[session.turns.length - 1];
      if (!turn) return session;
      const next = cloneSession(session);
      const lastTurn = next.turns[next.turns.length - 1];
      if (!lastTurn) return session;
      const agent = cloneAgent(patch.agent);
      const agentIndex = lastTurn.agents.length;
      lastTurn.agents.push(agent);
      lastTurn.agentCount += 1;
      next.agentIndex[patch.agent.agentId] = {
        turnNumber: lastTurn.turnNumber,
        agentIndex,
      };
      return next;
    }

    case "complete_agent": {
      const loc = session.agentIndex[patch.agentId];
      if (!loc) return session;
      const next = cloneSession(session);
      const turn = next.turns.find((t) => t.turnNumber === loc.turnNumber);
      if (!turn) return session;
      const agent = turn.agents[loc.agentIndex];
      if (!agent) return session;
      agent.status = "done";
      if (typeof patch.stopText === "string") agent.stopText = patch.stopText;
      if (typeof patch.stopThinking === "string") agent.stopThinking = patch.stopThinking;
      if (typeof patch.duration === "number") agent.duration = patch.duration;
      if (typeof patch.transcriptPath === "string") agent.transcriptPath = patch.transcriptPath;
      return next;
    }

    case "set_turn_stop": {
      const target = findTurn(session, patch.turnNumber);
      if (!target) return session;
      const next = cloneSession(session);
      const nt = next.turns.find((t) => t.turnNumber === patch.turnNumber);
      if (!nt) return session;
      if (typeof patch.stopText === "string") nt.stopText = patch.stopText;
      if (typeof patch.stopThinking === "string") nt.stopThinking = patch.stopThinking;
      if (typeof patch.stopReason === "string") nt.stopReason = patch.stopReason;
      return next;
    }

    case "set_claude_status": {
      if (session.claudeStatus === patch.claudeStatus) return session;
      const next = cloneSession(session);
      next.claudeStatus = patch.claudeStatus;
      return next;
    }

    default:
      // Opaque to this reducer — caller decides to re-request a full
      // session.snapshot. Returning the input untouched signals that
      // decision via reference equality.
      return session;
  }
}

function findTurn(session: Session, turnNumber: number): unknown {
  return session.turns.find((t) => t.turnNumber === turnNumber);
}

function cloneSession(session: Session): Session {
  return structuredClone(session);
}

function cloneStep(step: StepNode): StepNode {
  return structuredClone(step);
}

function cloneTurn(turn: Session["turns"][number]): Session["turns"][number] {
  return structuredClone(turn);
}

function cloneTool(tool: Session["turns"][number]["steps"][number]["tools"][number]): Session["turns"][number]["steps"][number]["tools"][number] {
  return structuredClone(tool);
}

function cloneAgent(agent: Session["turns"][number]["agents"][number]): Session["turns"][number]["agents"][number] {
  return structuredClone(agent);
}
