// event-handler.ts — Hook event → state mutations → patches
//
// Port of claude-gravity-events.el (~846 lines).
// Receives enriched HookData from the bridge shim, applies state mutations
// to Session, and returns Patch[] for terminals.
//
// Does NOT handle:
// - Tmux re-keying (Emacs-specific)
// - Daemon re-keying (Emacs-specific)
// - Resume picker flow (Emacs-specific)
// - OpenCode events (SessionStatus, MessagePart, etc.)
// - Buffer management (Emacs-specific)
// These are terminal-side concerns.

import type { HookData, HookEventName, Patch, Session, TokenUsage } from "@gravity/shared";
import { SessionStore } from "../state/session-store.js";
import { InboxManager } from "../state/inbox.js";
import {
  createSession,
  resetSession,
  sessionEnd,
  setClaudeStatus,
  setPermissionMode,
  setTokenUsage,
  setPlan,
  updateMeta,
  addPrompt,
  finalizeLastPrompt,
  addTool,
  completeTool,
  findTool,
  addAgent,
  completeAgent,
  trackFile,
  trackTask,
  updateTurnTokens,
  finalizeTurnTokens,
  updatePromptAnswer,
} from "../state/session.js";
import { log } from "../util/log.js";
import type { Socket } from "net";

/** Short model name mapping (mirrors claude-gravity--short-model-name). */
function shortModelName(modelId: string): string {
  if (modelId.includes("opus")) return "opus";
  if (modelId.includes("sonnet")) return "sonnet";
  if (modelId.includes("haiku")) return "haiku";
  return modelId;
}

/** Strip system XML from user prompt text. */
function stripSystemXml(raw: string | undefined): string | null {
  if (!raw) return null;
  // Remove <command-name>...</command-name> and <command-args>...</command-args>
  // and <system-reminder>...</system-reminder> blocks
  let text = raw.replace(/<system-reminder>[\s\S]*?<\/system-reminder>/g, "");
  text = text.replace(/<command-name>[\s\S]*?<\/command-name>/g, "");
  text = text.replace(/<command-args>[\s\S]*?<\/command-args>/g, "");
  text = text.trim();
  return text.length > 0 ? text : null;
}

/** Extract slash command as fallback display text. */
function extractSlashCommand(raw: string | undefined): string | null {
  if (!raw) return null;
  const nameMatch = raw.match(/<command-name>([^<]+)<\/command-name>/);
  if (!nameMatch) return null;
  const cmdName = nameMatch[1];
  const argsMatch = raw.match(/<command-args>([^<]*)<\/command-args>/);
  const cmdArgs = argsMatch?.[1];
  if (cmdArgs && cmdArgs.trim().length > 0) {
    return `/${cmdName} ${cmdArgs}`;
  }
  return `/${cmdName}`;
}

/** Extract answer from AskUserQuestion tool response. */
function extractAskAnswer(toolResponse: unknown): string {
  if (!toolResponse || typeof toolResponse !== "object") return "";
  const resp = toolResponse as Record<string, unknown>;
  // Claude Code returns answer in various formats
  if (typeof resp.result === "string") return resp.result;
  if (typeof resp.answer === "string") return resp.answer;
  if (typeof resp.content === "string") return resp.content;
  return JSON.stringify(resp);
}

export interface EventHandlerDeps {
  store: SessionStore;
  inbox: InboxManager;
}

/** Ensure a session exists, creating if needed. */
function ensureSession(store: SessionStore, sessionId: string, cwd: string, tmuxSession?: string): Session {
  let session = store.get(sessionId);
  if (!session) {
    session = createSession(sessionId, cwd);
    if (tmuxSession) session.tmuxSession = tmuxSession;
    store.set(sessionId, session);
  }
  return session;
}

/**
 * Handle a hook event, applying state mutations and returning patches.
 *
 * @param eventName - The hook event type
 * @param sessionId - Claude Code session ID
 * @param cwd - Working directory
 * @param data - Enriched hook data from the bridge
 * @param pid - Claude Code process PID
 * @param deps - SessionStore and InboxManager
 * @param hookSocket - Socket for bidirectional responses (PermissionRequest)
 * @returns Patches to send to terminals, or null for bidirectional events
 */
export function handleEvent(
  eventName: HookEventName,
  sessionId: string,
  cwd: string,
  data: HookData,
  pid: number | null,
  deps: EventHandlerDeps,
  hookSocket?: Socket,
): Patch[] {
  const { store, inbox } = deps;

  log(`Handling event: ${eventName} for session: ${sessionId}`, "debug");

  // Update meta on every event for existing sessions
  const existing = store.get(sessionId);
  if (existing) {
    // Self-heal: if session is ended but we're receiving events, it's alive
    if (existing.status === "ended" && eventName !== "SessionEnd") {
      existing.status = "active";
      log(`Session ${sessionId} self-healed to active on ${eventName}`, "info");
    }
    updateMeta(existing, {
      pid: pid ?? undefined,
      slug: data.slug ?? undefined,
      branch: data.branch ?? undefined,
      tmuxSession: data.tmux_session ?? undefined,
    });
  }

  // Dismiss stale inbox items on turn/session boundaries
  if (["UserPromptSubmit", "Stop", "SessionEnd"].includes(eventName)) {
    inbox.removeForSession(sessionId);
  }

  const patches: Patch[] = [];

  switch (eventName) {
    case "SessionStart": {
      const session = store.get(sessionId);
      if (session) {
        resetSession(session);
      }
      const s = ensureSession(store, sessionId, cwd, data.tmux_session);
      const metaPatches = updateMeta(s, {
        pid: pid ?? undefined,
        slug: data.slug ?? undefined,
        branch: data.branch ?? undefined,
        tmuxSession: data.tmux_session ?? undefined,
      });
      patches.push(...metaPatches);

      // Model name from SessionStart payload
      const modelId = data.model;
      if (modelId && typeof modelId === "string" && modelId.length > 0) {
        s.modelName = shortModelName(modelId);
        patches.push({ op: "set_meta", modelName: s.modelName });
      }

      break;
    }

    case "SessionEnd": {
      const session = store.get(sessionId);
      if (session) {
        patches.push(...sessionEnd(session));
      }
      inbox.removeForSession(sessionId);
      break;
    }

    case "UserPromptSubmit": {
      const session = ensureSession(store, sessionId, cwd, data.tmux_session);
      const rawPrompt = data.prompt as string | undefined;
      const promptText = stripSystemXml(rawPrompt);
      const displayText = promptText || extractSlashCommand(rawPrompt);

      if (displayText) {
        patches.push(
          ...addPrompt(session, {
            type: "user",
            text: displayText,
            submitted: Date.now(),
            elapsed: null,
            toolUseId: null,
            answer: null,
          }),
        );
      }
      patches.push(...setClaudeStatus(session, "responding"));
      // Remove idle inbox items
      inbox.removeForSession(sessionId, "idle");
      break;
    }

    case "Stop": {
      const session = store.get(sessionId);
      if (!session) break;

      patches.push(...setClaudeStatus(session, "idle"));
      patches.push(
        ...finalizeLastPrompt(
          session,
          data.stop_text as string | undefined,
          data.stop_thinking as string | undefined,
        ),
      );

      const tokenUsage = data.token_usage;
      if (tokenUsage) {
        patches.push(...setTokenUsage(session, tokenUsage));
        patches.push(...finalizeTurnTokens(session, tokenUsage));
      }

      // Add idle inbox item
      const turn = session.currentTurn;
      const stopText = data.stop_text as string | undefined;
      const snippet = stopText
        ? stopText.replace(/[\n\r\t]+/g, " ").substring(0, 80)
        : "idle";
      inbox.removeForSession(sessionId, "idle");
      inbox.add("idle", sessionId, session.project, session.slug || sessionId.substring(0, 8), snippet, { turn, snippet });
      break;
    }

    case "SubagentStart": {
      const session = ensureSession(store, sessionId, cwd, data.tmux_session);
      patches.push(
        ...addAgent(session, {
          agentId: (data.agent_id as string) || "unknown",
          type: (data as Record<string, unknown>).agent_type as string || "unknown",
          status: "running",
          steps: [],
          toolCount: 0,
          stopText: null,
          stopThinking: null,
          duration: null,
          timestamp: Date.now(),
          transcriptPath: data.agent_transcript_path ?? null,
          taskToolUseId: null,
        }),
      );
      break;
    }

    case "SubagentStop": {
      const session = ensureSession(store, sessionId, cwd, data.tmux_session);
      const agentId = data.agent_id as string;
      if (agentId) {
        patches.push(
          ...completeAgent(session, agentId, {
            stopText: data.agent_stop_text,
            stopThinking: data.agent_stop_thinking,
            transcriptPath: data.agent_transcript_path,
          }),
        );
      }
      break;
    }

    case "PreToolUse": {
      const session = ensureSession(store, sessionId, cwd, data.tmux_session);
      const parentAgentId = data.parent_agent_id ?? null;
      const toolName = data.tool_name || "unknown";
      const toolUseId = data.tool_use_id || `unknown_${Date.now()}`;

      const tool = {
        toolUseId,
        name: toolName,
        input: (data.tool_input as Record<string, unknown>) || {},
        status: "running" as const,
        result: null as unknown,
        timestamp: Date.now(),
        duration: null,
        turn: session.currentTurn,
        assistantText: data.assistant_text ?? null,
        assistantThinking: data.assistant_thinking ?? null,
        postText: null,
        postThinking: null,
        parentAgentId: parentAgentId,
        ambiguous: false,
        candidateAgentIds: null,
        agentId: null,
      };

      patches.push(
        ...addTool(session, tool, parentAgentId, data.candidate_agent_ids ?? null),
      );
      patches.push(
        ...setPermissionMode(session, (data as Record<string, unknown>).permission_mode as string ?? null),
      );
      patches.push(...setClaudeStatus(session, "responding"));

      // Model name from enrichment
      if (!parentAgentId) {
        const modelId = data.model;
        if (modelId && typeof modelId === "string" && modelId.length > 0) {
          session.modelName = shortModelName(modelId);
        }
      }

      // File tracking
      patches.push(...trackFile(session, toolName, data.tool_input as Record<string, unknown>));

      // Task tracking
      patches.push(
        ...trackTask(session, "PreToolUse", toolName, data.tool_input as Record<string, unknown>, toolUseId),
      );

      // AskUserQuestion creates a question prompt
      if (toolName === "AskUserQuestion") {
        const input = data.tool_input as Record<string, unknown>;
        const questions = input?.questions as Array<Record<string, unknown>> | undefined;
        const firstQ = questions?.[0];
        const qText = firstQ?.question as string | undefined;
        if (qText) {
          patches.push(
            ...addPrompt(session, {
              type: "question",
              text: qText,
              submitted: Date.now(),
              elapsed: null,
              toolUseId,
              answer: null,
            }),
          );
        }
      }
      break;
    }

    case "PostToolUse": {
      const session = ensureSession(store, sessionId, cwd, data.tmux_session);
      const toolUseId = data.tool_use_id as string;
      const postText = data.post_tool_text as string | undefined;
      const postThink = data.post_tool_thinking as string | undefined;
      const toolResponse = (data as Record<string, unknown>).tool_response;

      if (toolUseId) {
        patches.push(
          ...completeTool(session, toolUseId, toolResponse, "done", postText, postThink),
        );
      }

      const toolName = data.tool_name || "";
      patches.push(...trackFile(session, toolName, data.tool_input as Record<string, unknown>));
      patches.push(
        ...trackTask(session, "PostToolUse", toolName, data.tool_input as Record<string, unknown>, toolUseId || "", toolResponse),
      );

      // AskUserQuestion answer
      if (toolName === "AskUserQuestion" && toolUseId) {
        const answer = extractAskAnswer(toolResponse);
        patches.push(...updatePromptAnswer(session, toolUseId, answer));
      }

      // ExitPlanMode — detect plan and create phase boundary turn
      if (toolName === "ExitPlanMode") {
        const input = data.tool_input as Record<string, unknown>;
        const resp = toolResponse as Record<string, unknown> | undefined;
        const planContent = input?.plan as string;
        const filePath = resp?.filePath as string;
        const allowedPrompts = input?.allowedPrompts as string[];

        if (planContent) {
          patches.push(
            ...setPlan(session, {
              content: planContent,
              filePath: filePath ?? null,
              allowedPrompts: allowedPrompts ?? null,
            }),
          );
        }

        // Phase boundary turn
        patches.push(
          ...addPrompt(session, {
            type: "phase-boundary",
            text: "[Plan approved]",
            submitted: Date.now(),
            elapsed: null,
            toolUseId: null,
            answer: null,
          }),
        );
      }

      // Per-turn token delta
      const tokenUsage = data.token_usage;
      if (tokenUsage) {
        patches.push(...updateTurnTokens(session, tokenUsage));
      }
      break;
    }

    case "PostToolUseFailure": {
      const session = ensureSession(store, sessionId, cwd, data.tmux_session);
      const toolUseId = data.tool_use_id as string;
      const errorMsg = (data as Record<string, unknown>).error as string || "Unknown error";
      const postText = data.post_tool_text as string | undefined;
      const postThink = data.post_tool_thinking as string | undefined;

      if (toolUseId) {
        patches.push(
          ...completeTool(session, toolUseId, `[ERROR] ${errorMsg}`, "error", postText, postThink),
        );
      }

      const toolName = data.tool_name || "";
      patches.push(...trackFile(session, toolName, data.tool_input as Record<string, unknown>));
      patches.push(
        ...trackTask(session, "PostToolUseFailure", toolName, data.tool_input as Record<string, unknown>, toolUseId || "", errorMsg),
      );
      break;
    }

    case "Notification": {
      // Pass through — no state mutation needed
      break;
    }

    case "PermissionRequest": {
      // Bidirectional: add inbox item, terminal responds later
      const session = ensureSession(store, sessionId, cwd, data.tmux_session);
      const toolName = data.tool_name || "unknown";
      const summary = toolName; // TODO: tool signature like Emacs

      if (hookSocket) {
        const item = inbox.add(
          "permission",
          sessionId,
          session.project,
          session.slug || sessionId.substring(0, 8),
          summary,
          data as Record<string, unknown>,
          hookSocket,
        );
        // Return empty patches — the inbox.added message will be sent separately
        return [];
      }
      break;
    }

    case "AskUserQuestionIntercept": {
      // Bidirectional: add question inbox item, terminal responds later
      const session = ensureSession(store, sessionId, cwd, data.tmux_session);
      const toolName = data.tool_name || "AskUserQuestion";
      const input = data.tool_input as Record<string, unknown> | undefined;
      const questions = input?.questions as Array<Record<string, unknown>> | undefined;
      const firstQ = questions?.[0];
      const questionText = (firstQ?.question as string) || toolName;
      const summary = questionText.substring(0, 80);

      if (hookSocket) {
        inbox.add(
          "question",
          sessionId,
          session.project,
          session.slug || sessionId.substring(0, 8),
          summary,
          data as Record<string, unknown>,
          hookSocket,
        );
        return [];
      }
      break;
    }
  }

  return patches;
}
