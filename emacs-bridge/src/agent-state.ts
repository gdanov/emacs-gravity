import { join, dirname, basename } from "path";
import { existsSync, readFileSync, writeFileSync, mkdirSync } from "fs";
import { log } from "./log.js";
import { readTail } from "./enrichment.js";

// --- Active Agent List ---
// Tracks which agents are currently running per session.
// Persisted to {transcript_dir}/gravity/emacs-bridge-agents.json
// alongside Claude's transcript.

export type AgentState = { [sessionId: string]: string[] };

export function getAgentStatePath(transcriptPath: string | undefined): string | undefined {
  if (!transcriptPath) return undefined;
  const transcriptDir = dirname(transcriptPath);
  return join(transcriptDir, "gravity", "emacs-bridge-agents.json");
}

export function readAgentState(cwd: string, transcriptPath?: string): AgentState {
  const statePath = transcriptPath ? getAgentStatePath(transcriptPath) : undefined;
  if (!statePath) return {};

  try {
    if (existsSync(statePath)) {
      return JSON.parse(readFileSync(statePath, "utf-8"));
    }
  } catch (e) {
    log(`readAgentState error: ${e}`, 'error');
  }
  return {};
}

export function writeAgentState(cwd: string, transcriptPath: string | undefined, state: AgentState): void {
  const statePath = transcriptPath ? getAgentStatePath(transcriptPath) : undefined;
  if (!statePath) return;

  try {
    const dir = dirname(statePath);
    if (!existsSync(dir)) mkdirSync(dir, { recursive: true });
    writeFileSync(statePath, JSON.stringify(state), "utf-8");
  } catch (e) {
    log(`writeAgentState error: ${e}`, 'error');
  }
}

export function agentTranscriptPath(transcriptPath: string, sessionId: string, agentId: string): string {
  const transcriptDir = dirname(transcriptPath);
  const sessionBase = basename(transcriptPath, ".jsonl");
  return join(transcriptDir, sessionBase, "subagents", `agent-${agentId}.jsonl`);
}

// Search an agent transcript for a specific tool_use_id.
// Returns true if the tool_use_id is found in the transcript.
export function transcriptHasToolUseId(agentTranscript: string, toolUseId: string): boolean {
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
  } catch (e) {
    log(`transcriptHasToolUseId error: ${e}`, 'error');
  }
  return false;
}

// Extract all tool_use IDs from an agent transcript.
// Called on SubagentStop for definitive attribution fix-up.
export function extractAgentToolIds(agentTranscript: string): string[] {
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
  } catch (e) {
    log(`extractAgentToolIds error: ${e}`, 'error');
  }
  return ids;
}

// Attribute a tool event to a specific agent.
// Returns the agent_id or "ambiguous" or null (root tool).
export function attributeToolToAgent(
  sessionId: string, cwd: string, transcriptPath: string | undefined,
  toolUseId: string, activeAgents: string[]
): { parentAgentId: string | null; candidateAgentIds?: string[] } {
  if (activeAgents.length === 0) return { parentAgentId: null };
  if (activeAgents.length === 1) return { parentAgentId: activeAgents[0] };

  // Multiple active agents — scan transcripts
  if (transcriptPath && toolUseId) {
    for (const agentId of activeAgents) {
      const atp = agentTranscriptPath(transcriptPath, sessionId, agentId);
      if (transcriptHasToolUseId(atp, toolUseId)) {
        log(`Attributed tool ${toolUseId} to agent ${agentId} via transcript lookup`);
        return { parentAgentId: agentId };
      }
    }
  }

  // Transcript lookup failed (race condition) — mark ambiguous
  log(`Tool ${toolUseId} ambiguous among ${activeAgents.length} agents`, 'warn');
  return { parentAgentId: "ambiguous", candidateAgentIds: [...activeAgents] };
}
