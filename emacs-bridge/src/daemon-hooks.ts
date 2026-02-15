// Hook callback factory for daemon sessions.
// Creates SDK hook callbacks that forward events to Emacs via the gravity socket.
// Uses enrichment.ts for transcript data extraction.

import type {
  HookEvent,
  HookCallbackMatcher,
  HookCallback,
  HookInput,
  HookJSONOutput,
  CanUseTool,
  PermissionResult,
} from "@anthropic-ai/claude-agent-sdk";
import { enrichEvent, AgentState, readAgentState, writeAgentState } from "./enrichment";
import { log } from "./log";

import type { SendEventFn, SendAndWaitFn } from "./daemon-session";

// ============================================================================
// In-memory agent state (per-session, no file persistence needed)
// ============================================================================

const sessionAgents: Map<string, string[]> = new Map();

function getActiveAgents(sessionId: string): string[] {
  return sessionAgents.get(sessionId) || [];
}

function addAgent(sessionId: string, agentId: string): void {
  const agents = sessionAgents.get(sessionId) || [];
  if (!agents.includes(agentId)) agents.push(agentId);
  sessionAgents.set(sessionId, agents);
}

function removeAgent(sessionId: string, agentId: string): void {
  const agents = sessionAgents.get(sessionId) || [];
  sessionAgents.set(sessionId, agents.filter(id => id !== agentId));
}

export function clearAgents(sessionId: string): void {
  sessionAgents.delete(sessionId);
}

// ============================================================================
// Hook callback factory
// ============================================================================

/**
 * Create SDK hook callbacks that forward events to Emacs.
 * Returns a hooks map suitable for passing to query options.
 */
export function createDaemonHooks(
  sendEvent: SendEventFn,
  sendAndWait: SendAndWaitFn,
  getSessionId: () => string,
  getCwd: () => string,
): Partial<Record<HookEvent, HookCallbackMatcher[]>> {
  const makeCallback = (eventName: string, isBidirectional: boolean = false): HookCallback => {
    return async (input: HookInput, toolUseId: string | undefined, { signal }): Promise<HookJSONOutput> => {
      const sessionId = getSessionId();
      const cwd = getCwd();

      // Build payload from hook input
      const payload: any = { ...input };
      if (toolUseId) payload.tool_use_id = toolUseId;
      payload.session_id = sessionId;

      // Agent tracking for SubagentStart/SubagentStop
      if (eventName === "SubagentStart" && payload.agent_id) {
        addAgent(sessionId, payload.agent_id);
      }
      if (eventName === "SubagentStop" && payload.agent_id) {
        removeAgent(sessionId, payload.agent_id);
      }

      // Enrich with transcript data
      const agentState: AgentState = {};
      agentState[sessionId] = getActiveAgents(sessionId);
      try {
        enrichEvent(payload, eventName, { agentState });
      } catch (e: any) {
        log(`[daemon-hooks] enrichEvent error for ${eventName}: ${e.message}`, 'error');
      }

      if (isBidirectional) {
        const response = await sendAndWait(eventName, sessionId, cwd, null, payload);
        return response || {};
      } else {
        await sendEvent(eventName, sessionId, cwd, null, payload);
        return {};
      }
    };
  };

  // NOTE: SessionStart and SessionEnd are NOT forwarded here.
  // daemon-session.ts handles them natively via system/init and result messages,
  // adding temp_id for Emacs re-keying.  Forwarding them from hooks would cause
  // duplicates and race conditions with re-keying.
  const fireAndForget: HookEvent[] = [
    "PreToolUse",
    "PostToolUse",
    "PostToolUseFailure",
    "SubagentStart",
    "SubagentStop",
    "UserPromptSubmit",
    "Stop",
    "Notification",
  ];

  const hooks: Partial<Record<HookEvent, HookCallbackMatcher[]>> = {};

  for (const event of fireAndForget) {
    hooks[event] = [{
      hooks: [makeCallback(event, false)],
    }];
  }

  // PermissionRequest is bidirectional (but we handle it via canUseTool instead)
  // If the SDK routes it through hooks, handle it here too
  hooks["PermissionRequest"] = [{
    hooks: [makeCallback("PermissionRequest", true)],
  }];

  return hooks;
}

// ============================================================================
// canUseTool factory
// ============================================================================

/**
 * Create a canUseTool callback that routes permission requests to Emacs.
 */
export function createCanUseTool(
  sendAndWait: SendAndWaitFn,
  getSessionId: () => string,
  getCwd: () => string,
): CanUseTool {
  return async (toolName, input, { signal, suggestions, toolUseID, agentID, decisionReason }) => {
    const sessionId = getSessionId();
    const cwd = getCwd();

    const payload = {
      tool_name: toolName,
      tool_input: input,
      tool_use_id: toolUseID,
      agent_id: agentID,
      permission_suggestions: suggestions,
      decision_reason: decisionReason,
      session_id: sessionId,
      cwd,
    };

    log(`[daemon-hooks] canUseTool: ${toolName} (${toolUseID})`, 'info');

    const response = await sendAndWait("PermissionRequest", sessionId, cwd, null, payload);

    if (response?.hookSpecificOutput?.decision) {
      const decision = response.hookSpecificOutput.decision;
      if (decision.behavior === "allow") {
        return {
          behavior: "allow" as const,
          updatedInput: decision.updatedInput || input,
          updatedPermissions: decision.updatedPermissions,
        };
      }
      return {
        behavior: "deny" as const,
        message: decision.message || "Denied by user",
        interrupt: decision.interrupt,
      };
    }

    // No response or invalid — deny by default
    log(`[daemon-hooks] canUseTool: no valid response, denying ${toolName}`, 'warn');
    return {
      behavior: "deny" as const,
      message: "No response from Emacs",
    };
  };
}
