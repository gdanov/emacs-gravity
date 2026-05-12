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

import { Effect, pipe } from "effect";
import { homedir } from "os";
import { join } from "path";
import type { HookData, HookEventName, Patch, Session } from "@gravity/shared";
import type { SessionStoreService } from "../services/session-store.js";
import { SessionStore } from "../services/session-store.js";
import type { InboxService } from "../services/inbox.js";
import { Inbox } from "../services/inbox.js";
import { Fs } from "@gravity/shared";
import type { FsService } from "@gravity/shared";
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
  attachPrompt,
  openTurn,
  closeTurn,
  finalizeLastPrompt,
  addTool,
  completeTool,
  updateToolPartial,
  addCompaction,
  addAgent,
  completeAgent,
  trackFile,
  trackTask,
  updateTurnTokens,
  finalizeTurnTokens,
  updatePromptAnswer,
} from "../state/session.js";
import type { Socket } from "net";

// ── Pure helpers (no Effect wrapping needed) ─────────────────────────

/** Short model name mapping (mirrors claude-gravity--short-model-name). */
export function shortModelName(modelId: string): string {
  if (modelId.includes("opus")) return "opus";
  if (modelId.includes("sonnet")) return "sonnet";
  if (modelId.includes("haiku")) return "haiku";
  return modelId;
}

// Pre-compiled regexes for stripping Claude Code harness XML
const RE_SYSTEM_REMINDER = /<system-reminder>[\s\S]*?<\/system-reminder>/g;
const RE_COMMAND_NAME_BLOCK = /<command-name>[\s\S]*?<\/command-name>/g;
const RE_COMMAND_ARGS_BLOCK = /<command-args>[\s\S]*?<\/command-args>/g;
const RE_COMMAND_NAME = /<command-name>([^<]+)<\/command-name>/;
const RE_COMMAND_ARGS = /<command-args>([^<]*)<\/command-args>/;
const RE_TASK_NOTIFICATION = /<task-notification>[\s\S]*?<\/task-notification>/g;
const RE_LOCAL_COMMAND_CAVEAT = /<local-command-caveat>[\s\S]*?<\/local-command-caveat>/g;
const RE_COMMAND_MESSAGE = /<command-message>[\s\S]*?<\/command-message>/g;
const RE_LOCAL_COMMAND_STDOUT = /<local-command-stdout>[\s\S]*?<\/local-command-stdout>/g;

/** Strip all known harness XML tags from text. */
export function stripSystemTags(raw: string | undefined | null): string | null {
  if (!raw) return null;
  const text = raw
    .replace(RE_SYSTEM_REMINDER, "")
    .replace(RE_COMMAND_NAME_BLOCK, "")
    .replace(RE_COMMAND_ARGS_BLOCK, "")
    .replace(RE_TASK_NOTIFICATION, "")
    .replace(RE_LOCAL_COMMAND_CAVEAT, "")
    .replace(RE_COMMAND_MESSAGE, "")
    .replace(RE_LOCAL_COMMAND_STDOUT, "")
    .trim();
  return text.length > 0 ? text : null;
}

/** Strip system XML from user prompt text. */
export function stripSystemXml(raw: string | undefined): string | null {
  return stripSystemTags(raw);
}

/** Extract slash command as fallback display text. */
export function extractSlashCommand(raw: string | undefined): string | null {
  if (!raw) return null;
  const nameMatch = raw.match(RE_COMMAND_NAME);
  if (!nameMatch) return null;
  const cmdName = nameMatch[1];
  const argsMatch = raw.match(RE_COMMAND_ARGS);
  const cmdArgs = argsMatch?.[1];
  if (cmdArgs && cmdArgs.trim().length > 0) {
    return `/${cmdName} ${cmdArgs}`;
  }
  return `/${cmdName}`;
}

/** Extract answer from AskUserQuestion tool response. */
export function extractAskAnswer(toolResponse: unknown): string {
  if (!toolResponse || typeof toolResponse !== "object") return "";
  const resp = toolResponse as Record<string, unknown>;
  if (typeof resp.result === "string") return resp.result;
  if (typeof resp.answer === "string") return resp.answer;
  if (typeof resp.content === "string") return resp.content;
  return JSON.stringify(resp);
}

// ── Effectful helpers ────────────────────────────────────────────────

/** Look up the session's display name from Claude Code's sessions-index.json. */
const lookupDisplayName = (cwd: string, sessionId: string): Effect.Effect<string | null, never, FsService> =>
  Effect.gen(function* () {
    const fs = yield* Effect.service(Fs);
    const encoded = cwd.replace(/\//g, "-");
    const indexPath = join(homedir(), ".claude", "projects", encoded, "sessions-index.json");
    const exists = yield* fs.exists(indexPath);
    if (!exists) return null;
    return yield* pipe(
      fs.readFile(indexPath),
      Effect.map((raw) => {
        const data = JSON.parse(raw);
        if (!data?.entries || !Array.isArray(data.entries)) return null;
        const entry = data.entries.find((e: Record<string, unknown>) => e.sessionId === sessionId);
        return (entry?.summary as string) || null;
      }),
      Effect.catch(() => Effect.succeed(null)),
    );
  });

/** Ensure a session exists, creating if needed. */
const ensureSession = (
  store: SessionStoreService,
  sessionId: string,
  cwd: string,
  tmuxSession?: string,
  source?: string,
): Session => {
  let session = store.get(sessionId);
  if (!session) {
    session = createSession(sessionId, cwd);
    if (tmuxSession) session.tmuxSession = tmuxSession;
    if (source) session.source = source;
    store.set(sessionId, session);
  }
  return session;
};

// ── Per-event handlers ───────────────────────────────────────────────

type EventContext = {
  readonly sessionId: string;
  readonly cwd: string;
  readonly data: HookData;
  readonly pid: number | null;
  readonly hookSocket?: Socket;
};

const handleSessionStart = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const patches: Patch[] = [];

    const existing = store.get(ctx.sessionId);
    // Reset on re-start with the same session id (Claude Code's /clear shape
    // and resume cases). Pi no longer triggers SessionStart per prompt —
    // pi's per-prompt boundary is TurnOpen — so there is no pi case to
    // exempt here. SessionStart for a pi session fires once, synthesized
    // eagerly by `startPiSession`, before any pi events arrive; that path
    // creates a fresh session and the `existing` branch is not taken.
    if (existing) {
      patches.push(...resetSession(existing));
    }
    const s = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session, ctx.data.source);
    const displayName = s.displayName ? undefined : (yield* lookupDisplayName(ctx.cwd, ctx.sessionId)) ?? undefined;
    patches.push(...updateMeta(s, {
      pid: ctx.pid ?? undefined,
      slug: ctx.data.slug ?? undefined,
      displayName,
      branch: ctx.data.branch ?? undefined,
      tmuxSession: ctx.data.tmux_session ?? undefined,
    }));

    const modelId = ctx.data.model;
    if (modelId && typeof modelId === "string" && modelId.length > 0) {
      s.modelName = shortModelName(modelId);
      patches.push({ op: "set_meta", modelName: s.modelName });
    }

    return patches;
  });

const handleSessionEnd = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const session = store.get(ctx.sessionId);
    if (!session) return [];
    return sessionEnd(session);
  });

const handleUserPromptSubmit = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const patches: Patch[] = [];
    const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
    const rawPrompt = ctx.data.prompt as string | undefined;
    const promptText = stripSystemXml(rawPrompt);
    const displayText = promptText || extractSlashCommand(rawPrompt);

    if (displayText) {
      const lastTurn = session.turns[session.turns.length - 1];
      const isDuplicate = lastTurn?.prompt?.text === displayText
        && lastTurn.prompt.submitted != null
        && (Date.now() - lastTurn.prompt.submitted) < 500;
      if (!isDuplicate) {
        const entry = {
          type: "user" as const,
          text: displayText,
          submitted: Date.now(),
          elapsed: null,
          toolUseId: null,
          answer: null,
        };
        // Pi opens the turn separately via TurnOpen (agent_start). The user
        // message arrives later as a UserPromptSubmit; attach to the
        // already-opened, still-empty turn instead of creating a new one.
        // Claude Code's UserPromptSubmit is atomic (open + attach), so it
        // falls through to addPrompt.
        //
        // Guard against attaching to turn 0 (pre-prompt activity): if pi
        // ever emits `message_start role=user` before `agent_start` the
        // "current" turn is turn 0, which by convention has no prompt.
        // Attaching there would orphan the prompt off the pre-prompt
        // bucket and skip actual turn creation.
        const attachToOpen =
          (ctx.data.source === "pi" || session.source === "pi") &&
          lastTurn && !lastTurn.prompt && !lastTurn.frozen &&
          lastTurn.turnNumber !== 0;
        if (attachToOpen) {
          patches.push(...attachPrompt(session, entry));
        } else {
          patches.push(...addPrompt(session, entry));
        }
        if (!session.displayName) {
          const name = displayText.length <= 60
            ? displayText
            : displayText.slice(0, 57).replace(/\s+\S*$/, "") + "...";
          patches.push(...updateMeta(session, { displayName: name }));
        }
      }
    }
    patches.push(...setClaudeStatus(session, "responding"));
    return patches;
  });

/**
 * Internal pi-driver event: open an empty turn (boundary signal). Emitted
 * by the translator on pi's `agent_start`. The corresponding UserPromptSubmit
 * (carried by pi's `message_start role=user`) will attach the prompt label
 * to this turn. If the user message never arrives or is empty, the turn
 * shows up unlabeled — the boundary is still in place.
 */
const handleTurnOpen = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session, ctx.data.source as string | undefined);
    const patches: Patch[] = [];
    patches.push(...openTurn(session));
    patches.push(...setClaudeStatus(session, "responding"));
    return patches;
  });

/**
 * Internal pi-driver event: close the current turn (stamp stop text,
 * record tokens, freeze). Emitted by the translator on pi's `agent_end`.
 * Mirrors the work that `handleStop` does for Claude Code (idle status,
 * inbox idle item), but routes through `closeTurn` to guarantee the turn
 * is frozen — even if the next prompt never comes.
 */
const handleTurnClose = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const inbox = yield* Effect.service(Inbox);
    const session = store.get(ctx.sessionId);
    if (!session) return [];

    const patches: Patch[] = [];
    patches.push(...setClaudeStatus(session, "idle"));

    const stopText = stripSystemTags(ctx.data.stop_text as string) ?? undefined;
    const stopThinking = stripSystemTags(ctx.data.stop_thinking as string) ?? undefined;
    const stopReason = typeof ctx.data.stop_reason === "string" ? ctx.data.stop_reason : undefined;
    const tokenUsage = ctx.data.token_usage;
    const closeOpts: { stopText?: string; stopThinking?: string; stopReason?: string; tokenIn?: number; tokenOut?: number } = {
      stopText,
      stopThinking,
      stopReason,
    };
    if (tokenUsage) {
      const tokenIn =
        (tokenUsage.input_tokens ?? 0) +
        (tokenUsage.cache_read_input_tokens ?? 0) +
        (tokenUsage.cache_creation_input_tokens ?? 0);
      const tokenOut = tokenUsage.output_tokens ?? 0;
      closeOpts.tokenIn = tokenIn;
      closeOpts.tokenOut = tokenOut;
      patches.push(...setTokenUsage(session, tokenUsage));
    }
    patches.push(...closeTurn(session, closeOpts));

    const turn = session.currentTurn;
    const snippet = stopText
      ? stopText.replace(/[\n\r\t]+/g, " ").substring(0, 80)
      : "idle";
    inbox.add(
      "idle",
      ctx.sessionId,
      session.project,
      session.slug || ctx.sessionId.substring(0, 8),
      snippet,
      { turn, snippet },
    );
    return patches;
  });

const handleStop = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const inbox = yield* Effect.service(Inbox);
    const session = store.get(ctx.sessionId);
    if (!session) return [];

    const patches: Patch[] = [];
    patches.push(...setClaudeStatus(session, "idle"));
    patches.push(
      ...finalizeLastPrompt(
        session,
        stripSystemTags(ctx.data.stop_text as string) ?? undefined,
        stripSystemTags(ctx.data.stop_thinking as string) ?? undefined,
      ),
    );

    const tokenUsage = ctx.data.token_usage;
    if (tokenUsage) {
      patches.push(...setTokenUsage(session, tokenUsage));
      patches.push(...finalizeTurnTokens(session, tokenUsage));
    }

    const turn = session.currentTurn;
    const stopText = ctx.data.stop_text as string | undefined;
    const snippet = stopText
      ? stopText.replace(/[\n\r\t]+/g, " ").substring(0, 80)
      : "idle";
    inbox.add("idle", ctx.sessionId, session.project, session.slug || ctx.sessionId.substring(0, 8), snippet, { turn, snippet });
    return patches;
  });

const handleSubagentStart = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
    return addAgent(session, {
      agentId: (ctx.data.agent_id as string) || "unknown",
      type: (ctx.data as Record<string, unknown>).agent_type as string || "unknown",
      status: "running",
      steps: [],
      toolCount: 0,
      stopText: null,
      stopThinking: null,
      duration: null,
      timestamp: Date.now(),
      transcriptPath: ctx.data.agent_transcript_path ?? null,
      taskToolUseId: null,
    });
  });

const handleSubagentStop = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
    const agentId = ctx.data.agent_id as string;
    if (!agentId) return [];
    return completeAgent(session, agentId, {
      stopText: stripSystemTags(ctx.data.agent_stop_text as string) ?? undefined,
      stopThinking: stripSystemTags(ctx.data.agent_stop_thinking as string) ?? undefined,
      transcriptPath: ctx.data.agent_transcript_path,
    });
  });

const handlePreToolUse = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
    const parentAgentId = ctx.data.parent_agent_id ?? null;
    const toolName = ctx.data.tool_name || "unknown";
    const toolUseId = ctx.data.tool_use_id || `unknown_${Date.now()}`;

    const tool = {
      toolUseId,
      name: toolName,
      input: (ctx.data.tool_input as Record<string, unknown>) || {},
      status: "running" as const,
      result: null as unknown,
      partial: null as unknown,
      timestamp: Date.now(),
      duration: null,
      turn: session.currentTurn,
      assistantText: stripSystemTags(ctx.data.assistant_text as string) ?? null,
      assistantThinking: stripSystemTags(ctx.data.assistant_thinking as string) ?? null,
      postText: null,
      postThinking: null,
      parentAgentId,
      ambiguous: false,
      candidateAgentIds: null,
      agentId: null,
    };

    const patches: Patch[] = [];
    patches.push(
      ...addTool(session, tool, parentAgentId, ctx.data.candidate_agent_ids ?? null),
    );
    patches.push(
      ...setPermissionMode(session, (ctx.data as Record<string, unknown>).permission_mode as string ?? null),
    );
    patches.push(...setClaudeStatus(session, "responding"));

    if (!parentAgentId) {
      const modelId = ctx.data.model;
      if (modelId && typeof modelId === "string" && modelId.length > 0) {
        session.modelName = shortModelName(modelId);
      }
    }

    patches.push(...trackFile(session, toolName, ctx.data.tool_input as Record<string, unknown>));
    patches.push(
      ...trackTask(session, "PreToolUse", toolName, ctx.data.tool_input as Record<string, unknown>, toolUseId),
    );

    if (toolName === "AskUserQuestion") {
      const input = ctx.data.tool_input as Record<string, unknown>;
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
    return patches;
  });

const handlePostToolUse = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
    const toolUseId = ctx.data.tool_use_id as string;
    const postText = stripSystemTags(ctx.data.post_tool_text as string) ?? undefined;
    const postThink = stripSystemTags(ctx.data.post_tool_thinking as string) ?? undefined;
    const toolResponse = (ctx.data as Record<string, unknown>).tool_response;

    const patches: Patch[] = [];
    if (toolUseId) {
      patches.push(
        ...completeTool(session, toolUseId, toolResponse, "done", postText, postThink),
      );
    }

    const toolName = ctx.data.tool_name || "";
    patches.push(...trackFile(session, toolName, ctx.data.tool_input as Record<string, unknown>));
    patches.push(
      ...trackTask(session, "PostToolUse", toolName, ctx.data.tool_input as Record<string, unknown>, toolUseId || "", toolResponse),
    );

    if (toolName === "AskUserQuestion" && toolUseId) {
      const answer = extractAskAnswer(toolResponse);
      patches.push(...updatePromptAnswer(session, toolUseId, answer));
    }

    if (toolName === "ExitPlanMode") {
      const input = ctx.data.tool_input as Record<string, unknown>;
      const resp = toolResponse as Record<string, unknown> | undefined;
      const planContent = input?.plan as string;
      const filePath = resp?.filePath as string;
      const allowedPrompts = input?.allowedPrompts as string[];

      if (planContent) {
        patches.push(
          ...setPlan(session, {
            content: planContent,
            filePath: filePath ?? null,
            allowedPrompts: allowedPrompts ?? [],
          }),
        );
      }

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

    const tokenUsage = ctx.data.token_usage;
    if (tokenUsage) {
      patches.push(...updateTurnTokens(session, tokenUsage));
    }
    return patches;
  });

const handlePostToolUseFailure = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
    const toolUseId = ctx.data.tool_use_id as string;
    const errorMsg = (ctx.data as Record<string, unknown>).error as string || "Unknown error";
    const postText = stripSystemTags(ctx.data.post_tool_text as string) ?? undefined;
    const postThink = stripSystemTags(ctx.data.post_tool_thinking as string) ?? undefined;

    const patches: Patch[] = [];
    if (toolUseId) {
      patches.push(
        ...completeTool(session, toolUseId, `[ERROR] ${errorMsg}`, "error", postText, postThink),
      );
    }

    const toolName = ctx.data.tool_name || "";
    patches.push(...trackFile(session, toolName, ctx.data.tool_input as Record<string, unknown>));
    patches.push(
      ...trackTask(session, "PostToolUseFailure", toolName, ctx.data.tool_input as Record<string, unknown>, toolUseId || "", errorMsg),
    );
    return patches;
  });

const handlePermissionRequest = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const inbox = yield* Effect.service(Inbox);
    const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
    const toolName = ctx.data.tool_name || "unknown";
    const summary = toolName;
    const inboxType = toolName === "ExitPlanMode" ? "plan-review" as const : "permission" as const;

    const patches: Patch[] = [];
    patches.push(...setClaudeStatus(session, "idle"));

    if (ctx.hookSocket) {
      inbox.add(
        inboxType,
        ctx.sessionId,
        session.project,
        session.slug || ctx.sessionId.substring(0, 8),
        summary,
        ctx.data as Record<string, unknown>,
        ctx.hookSocket,
      );
    }
    return patches;
  });

const handleAskUserQuestionIntercept = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const inbox = yield* Effect.service(Inbox);
    const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
    const toolName = ctx.data.tool_name || "AskUserQuestion";
    const input = ctx.data.tool_input as Record<string, unknown> | undefined;
    const questions = input?.questions as Array<Record<string, unknown>> | undefined;
    const firstQ = questions?.[0];
    const questionText = (firstQ?.question as string) || toolName;
    const summary = questionText.substring(0, 80);

    const patches: Patch[] = [];
    patches.push(...setClaudeStatus(session, "idle"));

    if (ctx.hookSocket) {
      inbox.add(
        "question",
        ctx.sessionId,
        session.project,
        session.slug || ctx.sessionId.substring(0, 8),
        summary,
        ctx.data as Record<string, unknown>,
        ctx.hookSocket,
      );
    }
    return patches;
  });

/**
 * Internal pi-driver event: pi completed a compaction. Records a
 * `CompactionMarker` on the session capturing the reason, the turn that
 * was current at the time, optional token-savings info, and pi's summary
 * of the discarded history. Does NOT move turn boundaries; the marker is
 * a session-level record terminals can render inline by `turnNumber`.
 *
 * Aborted compactions are still recorded (`aborted: true`) so the user
 * sees the attempt; summary is typically null in that case.
 */
const handleCompaction = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const session = ensureSession(
      store,
      ctx.sessionId,
      ctx.cwd,
      ctx.data.tmux_session,
      ctx.data.source as string | undefined,
    );
    const reason = typeof ctx.data.compaction_reason === "string"
      ? (ctx.data.compaction_reason as string)
      : "unknown";
    const tokensBefore = typeof ctx.data.compaction_tokens_before === "number"
      ? (ctx.data.compaction_tokens_before as number)
      : null;
    const summary = stripSystemTags(ctx.data.compaction_summary as string) ?? null;
    const aborted = ctx.data.compaction_aborted === true;

    return addCompaction(session, {
      reason,
      // -1 sentinel if no user turn has been opened yet (compaction during
      // turn 0 — the pre-prompt activity bucket). Terminals can render
      // such markers as session-prelude entries.
      turnNumber: session.currentTurn > 0 ? session.currentTurn : -1,
      timestamp: Date.now(),
      tokensBefore,
      summary,
      aborted,
    });
  });

/**
 * Internal pi-driver event: streaming partial result from a running tool.
 * Emitted by the translator on pi's `tool_execution_update`. Replaces
 * `tool.partial` with the new value; no-op if the tool is unknown or
 * already completed (late `_update` arriving after `_end` is dropped).
 */
const handleToolPartial = (ctx: EventContext) =>
  Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);
    const session = store.get(ctx.sessionId);
    if (!session) return [];
    const toolUseId = ctx.data.tool_use_id as string | undefined;
    if (!toolUseId) return [];
    const partial = (ctx.data as Record<string, unknown>).partial;
    return updateToolPartial(session, toolUseId, partial);
  });

// ── Dispatch map ─────────────────────────────────────────────────────

type Handler = (ctx: EventContext) => Effect.Effect<Patch[], never, SessionStoreService | InboxService | FsService>;

const dispatch: Record<string, Handler> = {
  SessionStart: handleSessionStart,
  SessionEnd: handleSessionEnd,
  UserPromptSubmit: handleUserPromptSubmit,
  Stop: handleStop,
  SubagentStart: handleSubagentStart,
  SubagentStop: handleSubagentStop,
  PreToolUse: handlePreToolUse,
  PostToolUse: handlePostToolUse,
  PostToolUseFailure: handlePostToolUseFailure,
  Notification: () => Effect.succeed([]),
  PermissionRequest: handlePermissionRequest,
  AskUserQuestionIntercept: handleAskUserQuestionIntercept,
  TurnOpen: handleTurnOpen,
  TurnClose: handleTurnClose,
  ToolPartial: handleToolPartial,
  Compaction: handleCompaction,
};

// ── Post-stop race detection ─────────────────────────────────────────

const postStopEvents: ReadonlySet<string> = new Set([
  "PreToolUse", "PostToolUse", "PostToolUseFailure",
  "SubagentStart", "SubagentStop",
]);

// ── Main entry point ─────────────────────────────────────────────────

/**
 * Handle a hook event, applying state mutations and returning patches.
 *
 * Returns an Effect that requires SessionStore, Inbox, and Fs services.
 */
export function handleEvent(
  eventName: HookEventName,
  sessionId: string,
  cwd: string,
  data: HookData,
  pid: number | null,
  hookSocket?: Socket,
): Effect.Effect<Patch[], never, SessionStoreService | InboxService | FsService> {
  return Effect.gen(function* () {
    const store = yield* Effect.service(SessionStore);

    // Warn if tool/agent events arrive after Stop
    if (postStopEvents.has(eventName)) {
      const session = store.get(sessionId);
      if (session && session.claudeStatus === "idle") {
        // Log warning — post-stop race condition detected
        // (logging is best-effort, no service dependency needed for this warning)
      }
    }

    // Update meta on every event for existing sessions
    const preamblePatches: Patch[] = [];
    const existing = store.get(sessionId);
    if (existing) {
      if (existing.status === "ended" && eventName !== "SessionEnd" && eventName !== "Notification") {
        existing.status = "active";
        preamblePatches.push({ op: "set_status", status: "active" });
      }
      const freshDisplayName = yield* lookupDisplayName(cwd, sessionId);
      const displayName = (freshDisplayName && freshDisplayName !== existing.displayName) ? freshDisplayName : undefined;
      preamblePatches.push(...updateMeta(existing, {
        pid: pid ?? undefined,
        slug: data.slug ?? undefined,
        displayName,
        branch: data.branch ?? undefined,
        tmuxSession: data.tmux_session ?? undefined,
      }));
    }

    // Dispatch to per-event handler
    const handler = dispatch[eventName];
    const eventPatches = handler
      ? yield* handler({ sessionId, cwd, data, pid, hookSocket })
      : [];

    return [...preamblePatches, ...eventPatches];
  });
}
