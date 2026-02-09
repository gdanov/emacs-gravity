// Enrichment logic extracted from main() for testability.
// Each function takes raw event data and returns the enriched payload.

import { existsSync, readFileSync, statSync, mkdirSync, writeFileSync } from "fs";
import { join, dirname, basename } from "path";
import {
  extractPrecedingContent,
  extractFollowingContent,
  extractTrailingText,
  extractTokenUsage,
  readTail,
} from "./index";

// --- Slug extraction (duplicated from index.ts to avoid circular dependency) ---
import { openSync, readSync, closeSync } from "fs";

function extractSlug(transcriptPath: string): string | null {
  try {
    const fd = openSync(transcriptPath, "r");
    const buffer = Buffer.alloc(64 * 1024);
    const bytesRead = readSync(fd, buffer, 0, buffer.length, 0);
    closeSync(fd);
    if (bytesRead === 0) return null;
    const text = buffer.toString("utf-8", 0, bytesRead);
    const lines = text.split("\n");
    for (const line of lines) {
      if (!line.length) continue;
      try {
        const obj = JSON.parse(line);
        if (obj.slug) return obj.slug;
      } catch { continue; }
    }
  } catch {}
  return null;
}

// --- Agent state helpers (mirror index.ts) ---
type AgentState = { [sessionId: string]: string[] };

function getAgentStatePath(cwd: string): string {
  return join(cwd, ".claude", "emacs-bridge-agents.json");
}

export function readAgentState(cwd: string): AgentState {
  try {
    const p = getAgentStatePath(cwd);
    if (existsSync(p)) {
      return JSON.parse(readFileSync(p, "utf-8"));
    }
  } catch {}
  return {};
}

export function writeAgentState(cwd: string, state: AgentState): void {
  try {
    const p = getAgentStatePath(cwd);
    const dir = dirname(p);
    if (!existsSync(dir)) mkdirSync(dir, { recursive: true });
    writeFileSync(p, JSON.stringify(state), "utf-8");
  } catch {}
}

function agentTranscriptPath(transcriptPath: string, sessionId: string, agentId: string): string {
  const transcriptDir = dirname(transcriptPath);
  const sessionBase = basename(transcriptPath, ".jsonl");
  return join(transcriptDir, sessionBase, "subagents", `agent-${agentId}.jsonl`);
}

function transcriptHasToolUseId(agentTranscript: string, toolUseId: string): boolean {
  try {
    if (!existsSync(agentTranscript)) return false;
    const content = readTail(agentTranscript, 5 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c)) continue;
        for (const block of c) {
          if (block.type === "tool_use" && block.id === toolUseId) return true;
        }
      } catch { continue; }
    }
  } catch {}
  return false;
}

function extractAgentToolIds(agentTranscript: string): string[] {
  const ids: string[] = [];
  try {
    if (!existsSync(agentTranscript)) return ids;
    const content = readFileSync(agentTranscript, "utf-8");
    const lines = content.split("\n").filter((l) => l.length > 0);
    for (const line of lines) {
      try {
        const obj = JSON.parse(line);
        if (obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c)) continue;
        for (const block of c) {
          if (block.type === "tool_use" && block.id) ids.push(block.id);
        }
      } catch { continue; }
    }
  } catch {}
  return ids;
}

function attributeToolToAgent(
  sessionId: string, cwd: string, transcriptPath: string | undefined,
  toolUseId: string, activeAgents: string[]
): { parentAgentId: string | null; candidateAgentIds?: string[] } {
  if (activeAgents.length === 0) return { parentAgentId: null };
  if (activeAgents.length === 1) return { parentAgentId: activeAgents[0] };
  if (transcriptPath && toolUseId) {
    for (const agentId of activeAgents) {
      const atp = agentTranscriptPath(transcriptPath, sessionId, agentId);
      if (transcriptHasToolUseId(atp, toolUseId)) {
        return { parentAgentId: agentId };
      }
    }
  }
  return { parentAgentId: "ambiguous", candidateAgentIds: [...activeAgents] };
}

/**
 * Enriches a raw event payload with data extracted from transcripts.
 * This is a synchronous function that performs all enrichment except
 * retry-with-delay loops (which need async). For SubagentStop and
 * PostToolUse/Stop retries, the caller should handle retries.
 *
 * @param inputData - Raw event payload from stdin
 * @param eventName - Hook event name (e.g. "PreToolUse", "Stop")
 * @param opts - Optional overrides for testing
 * @returns Enriched payload (mutates and returns inputData)
 */
export function enrichEvent(
  inputData: any,
  eventName: string,
  opts?: {
    agentState?: AgentState;
    /** Override for Stop snapshot bytes (testing without statSync) */
    stopSnapshotBytes?: number;
  }
): any {
  const sessionId = inputData.session_id || "unknown";
  const cwd = inputData.cwd || "";
  const transcriptPath = inputData.transcript_path;

  // Common enrichment: temp_id
  const tempId = inputData.temp_id; // Already set by caller or env

  // Extract slug from transcript
  if (transcriptPath) {
    try {
      const slug = extractSlug(transcriptPath);
      if (slug) inputData.slug = slug;
    } catch {}
  }

  // Use provided agent state or read from disk
  const agentState = opts?.agentState ?? (cwd ? readAgentState(cwd) : {});

  // --- SubagentStart enrichment ---
  if (eventName === "SubagentStart") {
    const agentId = inputData.agent_id;
    if (agentId && transcriptPath) {
      inputData.agent_transcript_path = agentTranscriptPath(transcriptPath, sessionId, agentId);
    }
  }

  // --- SubagentStop enrichment ---
  if (eventName === "SubagentStop") {
    const agentId = inputData.agent_id;
    if (agentId && transcriptPath) {
      const atp = agentTranscriptPath(transcriptPath, sessionId, agentId);
      const toolIds = extractAgentToolIds(atp);
      if (toolIds.length > 0) {
        inputData.agent_tool_ids = toolIds;
      }
      inputData.agent_transcript_path = atp;
      // Extract trailing text (no retry — caller handles retries)
      try {
        const { text, thinking } = extractTrailingText(atp);
        if (text) inputData.agent_stop_text = text;
        if (thinking) inputData.agent_stop_thinking = thinking;
      } catch {}
    }
  }

  // --- Tool-to-agent attribution for PreToolUse/PostToolUse/PostToolUseFailure ---
  if (eventName === "PreToolUse" || eventName === "PostToolUse" || eventName === "PostToolUseFailure") {
    const activeAgents = agentState[sessionId] || [];
    if (activeAgents.length > 0) {
      const toolUseId = inputData.tool_use_id || "";
      const { parentAgentId, candidateAgentIds } = attributeToolToAgent(
        sessionId, cwd, transcriptPath, toolUseId, activeAgents
      );
      if (parentAgentId) {
        inputData.parent_agent_id = parentAgentId;
        if (candidateAgentIds) {
          inputData.candidate_agent_ids = candidateAgentIds;
        }
      }
    }
  }

  // --- PreToolUse: extract preceding assistant text/thinking ---
  if (eventName === "PreToolUse") {
    const toolUseId = inputData.tool_use_id;
    const parentAgentId = inputData.parent_agent_id;
    const effectiveTranscript = (parentAgentId && parentAgentId !== "ambiguous" && transcriptPath)
      ? agentTranscriptPath(transcriptPath, sessionId, parentAgentId)
      : transcriptPath;
    if (effectiveTranscript && toolUseId) {
      try {
        const { text, thinking } = extractPrecedingContent(effectiveTranscript, toolUseId);
        if (text) inputData.assistant_text = text;
        if (thinking) inputData.assistant_thinking = thinking;
      } catch {}
    }
  }

  // --- PostToolUse/PostToolUseFailure: extract following assistant text/thinking ---
  if (eventName === "PostToolUse" || eventName === "PostToolUseFailure") {
    const toolUseId = inputData.tool_use_id;
    const parentAgentId = inputData.parent_agent_id;
    const effectiveTranscript = (parentAgentId && parentAgentId !== "ambiguous" && transcriptPath)
      ? agentTranscriptPath(transcriptPath, sessionId, parentAgentId)
      : transcriptPath;
    if (effectiveTranscript && toolUseId) {
      try {
        const { text, thinking } = extractFollowingContent(effectiveTranscript, toolUseId);
        if (text) inputData.post_tool_text = text;
        if (thinking) inputData.post_tool_thinking = thinking;
      } catch {}
    }
  }

  // --- Stop: extract trailing text + token usage ---
  if (eventName === "Stop") {
    if (transcriptPath) {
      try {
        const snapshotBytes = opts?.stopSnapshotBytes ?? (() => {
          try { return statSync(transcriptPath).size; } catch { return undefined; }
        })();
        const { text, thinking } = extractTrailingText(transcriptPath, snapshotBytes);
        if (text) inputData.stop_text = text;
        if (thinking) inputData.stop_thinking = thinking;
      } catch {}
      try {
        inputData.token_usage = extractTokenUsage(transcriptPath);
      } catch {}
    }
  }

  return inputData;
}
