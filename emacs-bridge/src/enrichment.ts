// Enrichment logic and transcript extraction functions.
// This is the canonical location for all transcript parsing.
// Both index.ts (one-shot bridge) and daemon-hooks.ts import from here.

import { existsSync, readFileSync, statSync, mkdirSync, writeFileSync, openSync, readSync, closeSync } from "fs";
import { join, dirname, basename } from "path";
import { log } from "./log";

// ============================================================================
// Transcript reading utilities
// ============================================================================

/** Read the tail of a file (last maxBytes), skipping any partial first line. */
export function readTail(filePath: string, maxBytes: number): string {
  const stat = statSync(filePath);
  const size = stat.size;
  if (size <= maxBytes) {
    return readFileSync(filePath, "utf-8");
  }
  const fd = openSync(filePath, "r");
  const buffer = Buffer.alloc(maxBytes);
  readSync(fd, buffer, 0, maxBytes, size - maxBytes);
  closeSync(fd);
  const text = buffer.toString("utf-8");
  const firstNewline = text.indexOf("\n");
  return firstNewline >= 0 ? text.substring(firstNewline + 1) : text;
}

/** Read the first maxBytes of a file, skipping any partial last line. */
export function readHead(filePath: string, maxBytes: number): string {
  const stat = statSync(filePath);
  const size = stat.size;
  if (size <= maxBytes) {
    return readFileSync(filePath, "utf-8");
  }
  const fd = openSync(filePath, "r");
  const buffer = Buffer.alloc(maxBytes);
  readSync(fd, buffer, 0, maxBytes, 0);
  closeSync(fd);
  const text = buffer.toString("utf-8");
  const lastNewline = text.lastIndexOf("\n");
  return lastNewline >= 0 ? text.substring(0, lastNewline) : text;
}

// ============================================================================
// Transcript content extraction
// ============================================================================

/**
 * Extract assistant text and thinking that precede a tool_use.
 * The tool_use may not be in the transcript yet (PreToolUse fires before transcript write),
 * so we fall back to reading the most recent assistant text/thinking from the end.
 */
export function extractPrecedingContent(transcriptPath: string, toolUseId: string): { text: string; thinking: string; model: string } {
  const result = { text: "", thinking: "", model: "" };
  try {
    const content = readTail(transcriptPath, 2 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);

    let startIdx = -1;
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        if (c[0].type === "tool_use" && c[0].id === toolUseId) {
          startIdx = i;
          if (obj.message?.model) result.model = obj.message.model;
          break;
        }
      } catch { continue; }
    }

    if (startIdx < 0) {
      startIdx = lines.length;
    }

    const textParts: string[] = [];
    for (let i = startIdx - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant" && obj.type !== "user") continue;
        if (obj.type === "user") break;
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        const blockType = c[0].type;
        if (!result.model && obj.message?.model) result.model = obj.message.model;
        if (blockType === "tool_use") continue;
        if (blockType === "tool_result") break;
        if (blockType === "text") {
          const text = c[0].text || "";
          if (text && text !== "(no content)") {
            textParts.unshift(text);
          }
          continue;
        }
        if (blockType === "thinking" && !result.thinking) {
          result.thinking = c[0].thinking || "";
          break;
        }
        break;
      } catch { continue; }
    }
    result.text = textParts.join("\n\n");
    return result;
  } catch (e) {
    log(`extractPrecedingContent error: ${e}`, 'error');
    return result;
  }
}

/** Extract cumulative token usage from the transcript JSONL. */
export function extractTokenUsage(transcriptPath: string): {
  input_tokens: number;
  output_tokens: number;
  cache_read_input_tokens: number;
  cache_creation_input_tokens: number;
} {
  const result = { input_tokens: 0, output_tokens: 0, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 };
  try {
    const content = readTail(transcriptPath, 2 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);
    for (const line of lines) {
      try {
        const obj = JSON.parse(line);
        const usage = obj.usage;
        if (usage) {
          result.input_tokens += usage.input_tokens || 0;
          result.output_tokens += usage.output_tokens || 0;
          result.cache_read_input_tokens += usage.cache_read_input_tokens || 0;
          result.cache_creation_input_tokens += usage.cache_creation_input_tokens || 0;
        }
      } catch { continue; }
    }
    log(`extractTokenUsage: in=${result.input_tokens} out=${result.output_tokens} cache_read=${result.cache_read_input_tokens} cache_create=${result.cache_creation_input_tokens}`, 'info');
  } catch (e) {
    log(`extractTokenUsage error: ${e}`, 'error');
  }
  return result;
}

/**
 * Extract assistant text and thinking that follow a tool_result.
 * Scans forward from the tool_result to find any assistant text/thinking
 * that appears after the tool and before the next tool_use or end of transcript.
 */
export function extractFollowingContent(transcriptPath: string, toolUseId: string): { text: string; thinking: string } {
  const result = { text: "", thinking: "" };
  try {
    const content = readTail(transcriptPath, 2 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);

    let toolResultIdx = -1;
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "user" && obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c)) continue;
        for (const block of c) {
          if (block.type === "tool_result" && block.tool_use_id === toolUseId) {
            toolResultIdx = i;
            break;
          }
        }
        if (toolResultIdx >= 0) break;
      } catch { continue; }
    }

    if (toolResultIdx < 0) return result;

    const textParts: string[] = [];
    for (let i = toolResultIdx + 1; i < lines.length; i++) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant" && obj.type !== "user") continue;
        if (obj.type === "user") break;
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        const blockType = c[0].type;
        if (blockType === "text") {
          const text = c[0].text || "";
          if (text && text !== "(no content)") {
            textParts.push(text);
          }
          continue;
        }
        if (blockType === "thinking") {
          if (!result.thinking) {
            result.thinking = c[0].thinking || "";
          }
          continue;
        }
        break;
      } catch { continue; }
    }
    result.text = textParts.join("\n\n");
    return result;
  } catch (e) {
    log(`extractFollowingContent error: ${e}`, 'error');
    return result;
  }
}

/** Extract trailing text from agent (sidechain) transcripts. */
function extractTrailingTextFromAgent(lines: string[]): { text: string; thinking: string } {
  const result = { text: "", thinking: "" };
  try {
    let foundThinking = false;
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        for (const block of c) {
          if (block.type === "text") {
            const text = block.text || "";
            if (text && text !== "(no content)") {
              if (!result.text) result.text = text;
            }
          } else if (block.type === "thinking" && !foundThinking) {
            result.thinking = block.thinking || "";
            foundThinking = true;
          }
        }
        if (result.text || result.thinking) {
          log(`extractTrailingTextFromAgent: found text=${!!result.text} (${result.text.length} chars), thinking=${!!result.thinking} (${result.thinking.length} chars)`);
          return result;
        }
      } catch { continue; }
    }
    log(`extractTrailingTextFromAgent: no text/thinking found in ${lines.length} lines`);
    return result;
  } catch (e) {
    log(`extractTrailingTextFromAgent error: ${e}`, 'error');
    return result;
  }
}

/**
 * Called on Stop events to capture the conclusion text.
 * The maxBytes parameter limits how much of the transcript to read,
 * preventing the next turn's data from contaminating the result.
 */
export function extractTrailingText(transcriptPath: string, maxBytes?: number): { text: string; thinking: string } {
  const result = { text: "", thinking: "" };
  try {
    if (!existsSync(transcriptPath)) {
      log(`extractTrailingText: file not found: ${transcriptPath}`, 'warn');
      return result;
    }

    const content = maxBytes
      ? readHead(transcriptPath, maxBytes)
      : readTail(transcriptPath, 2 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);

    // Detect sidechain format
    let isSidechain = false;
    for (const line of lines.slice(0, 10)) {
      try {
        const obj = JSON.parse(line);
        if (obj.isSidechain !== undefined) {
          isSidechain = obj.isSidechain === true;
          break;
        }
      } catch { }
    }

    if (isSidechain) {
      log(`extractTrailingText: detected sidechain format, using agent extraction`, 'info');
      return extractTrailingTextFromAgent(lines);
    }

    const diagCount = Math.min(10, lines.length);
    const diagLines: string[] = [];
    for (let i = lines.length - diagCount; i < lines.length; i++) {
      try {
        const obj = JSON.parse(lines[i]);
        const t = obj.type || "?";
        const c = obj.message?.content;
        const block0 = Array.isArray(c) && c.length > 0 ? c[0].type : "-";
        const preview = block0 === "text" ? (c[0].text || "").substring(0, 60) : "";
        diagLines.push(`[${i}] ${t}/${block0} ${preview}`);
      } catch {
        diagLines.push(`[${i}] (parse error)`);
      }
    }
    log(`extractTrailingText: ${lines.length} lines, tail:\n  ${diagLines.join("\n  ")}`);

    const textParts: string[] = [];
    let stopReason = "exhausted";
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant" && obj.type !== "user") continue;
        if (obj.type === "user") {
          const uc = obj.message?.content;
          if (Array.isArray(uc) && uc.some((b: any) => b.type === "text")) {
            stopReason = `user_text@${i}`;
            break;
          }
          continue;
        }
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        let hasThinking = false;
        let hasToolUse = false;
        for (const block of c) {
          if (block.type === "text") {
            const text = block.text || "";
            if (text && text !== "(no content)") {
              textParts.unshift(text);
            }
          } else if (block.type === "thinking") {
            result.thinking = block.thinking || "";
            hasThinking = true;
          } else {
            stopReason = `${block.type}@${i}`;
            hasToolUse = true;
          }
        }
        if (hasThinking || hasToolUse) break;
      } catch { continue; }
    }
    result.text = textParts.join("\n\n");
    log(`extractTrailingText result: ${result.text.length} chars text, ${result.thinking.length} chars thinking, stop=${stopReason}, parts=${textParts.length}`);
    return result;
  } catch (e) {
    log(`extractTrailingText error: ${e}`, 'error');
    return result;
  }
}

// ============================================================================
// Slug / transcript metadata extraction
// ============================================================================

interface TranscriptMeta {
  slug: string | null;
  gitBranch: string | null;
}

export function extractTranscriptMeta(transcriptPath: string): TranscriptMeta {
  const result: TranscriptMeta = { slug: null, gitBranch: null };
  try {
    const fd = openSync(transcriptPath, "r");
    const buffer = Buffer.alloc(64 * 1024);
    const bytesRead = readSync(fd, buffer, 0, buffer.length, 0);
    closeSync(fd);
    if (bytesRead === 0) return result;
    const text = buffer.toString("utf-8", 0, bytesRead);
    const lines = text.split("\n");
    for (const line of lines) {
      if (!line.length) continue;
      try {
        const obj = JSON.parse(line);
        if (!result.slug && obj.slug) result.slug = obj.slug;
        if (!result.gitBranch && obj.gitBranch) result.gitBranch = obj.gitBranch;
        if (result.slug && result.gitBranch) break;
      } catch { continue; }
    }
  } catch {}
  return result;
}

export function extractSlug(transcriptPath: string): string | null {
  return extractTranscriptMeta(transcriptPath).slug;
}

// ============================================================================
// Agent state helpers
// ============================================================================

export type AgentState = { [sessionId: string]: string[] };

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

export function agentTranscriptPath(transcriptPath: string, sessionId: string, agentId: string): string {
  const transcriptDir = dirname(transcriptPath);
  const sessionBase = basename(transcriptPath, ".jsonl");
  return join(transcriptDir, sessionBase, "subagents", `agent-${agentId}.jsonl`);
}

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
  } catch {}
  return false;
}

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
  } catch {}
  return ids;
}

export function attributeToolToAgent(
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

// ============================================================================
// Event enrichment (used by one-shot bridge and daemon hooks)
// ============================================================================

/**
 * Enriches a raw event payload with data extracted from transcripts.
 * Synchronous — caller handles retries for Stop/SubagentStop.
 */
export function enrichEvent(
  inputData: any,
  eventName: string,
  opts?: {
    agentState?: AgentState;
    stopSnapshotBytes?: number;
  }
): any {
  const sessionId = inputData.session_id || "unknown";
  const cwd = inputData.cwd || "";
  const transcriptPath = inputData.transcript_path;

  if (transcriptPath) {
    try {
      const slug = extractSlug(transcriptPath);
      if (slug) inputData.slug = slug;
    } catch {}
  }

  const agentState = opts?.agentState ?? (cwd ? readAgentState(cwd) : {});

  if (eventName === "SubagentStart") {
    const agentId = inputData.agent_id;
    if (agentId && transcriptPath) {
      inputData.agent_transcript_path = agentTranscriptPath(transcriptPath, sessionId, agentId);
    }
  }

  if (eventName === "SubagentStop") {
    const agentId = inputData.agent_id;
    if (agentId && transcriptPath) {
      const atp = agentTranscriptPath(transcriptPath, sessionId, agentId);
      const toolIds = extractAgentToolIds(atp);
      if (toolIds.length > 0) {
        inputData.agent_tool_ids = toolIds;
      }
      inputData.agent_transcript_path = atp;
      try {
        const { text, thinking } = extractTrailingText(atp);
        if (text) inputData.agent_stop_text = text;
        if (thinking) inputData.agent_stop_thinking = thinking;
      } catch {}
    }
  }

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

  if (eventName === "PreToolUse") {
    const toolUseId = inputData.tool_use_id;
    const parentAgentId = inputData.parent_agent_id;
    const effectiveTranscript = (parentAgentId && parentAgentId !== "ambiguous" && transcriptPath)
      ? agentTranscriptPath(transcriptPath, sessionId, parentAgentId)
      : transcriptPath;
    if (effectiveTranscript && toolUseId) {
      try {
        const { text, thinking, model } = extractPrecedingContent(effectiveTranscript, toolUseId);
        if (text) inputData.assistant_text = text;
        if (thinking) inputData.assistant_thinking = thinking;
        if (model) inputData.model = model;
      } catch {}
    }
  }

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
