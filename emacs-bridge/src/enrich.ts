import { existsSync, statSync } from "fs";
import { log } from "./log.js";
import type { HookData } from "./types.js";
import {
  extractPrecedingContent,
  extractFollowingContent,
  extractTrailingText,
  extractTokenUsage,
  extractTranscriptMeta,
} from "./enrichment.js";
import {
  readAgentState,
  writeAgentState,
  agentTranscriptPath,
  extractAgentToolIds,
  attributeToolToAgent,
} from "./agent-state.js";

// Extract session metadata (slug, branch) from transcript
export function enrichSessionMetadata(inputData: HookData, transcriptPath: string | undefined): void {
  if (!transcriptPath) return;
  const meta = extractTranscriptMeta(transcriptPath);
  if (meta.slug) {
    inputData.slug = meta.slug;
    log(`Extracted slug: ${meta.slug}`);
  }
  if (meta.gitBranch) {
    inputData.branch = meta.gitBranch;
  }
}

// SubagentStart: add agent to active list, inject transcript path
export function enrichSubagentStart(inputData: HookData, sessionId: string, cwd: string, transcriptPath: string | undefined): void {
  const agentId = inputData.agent_id;
  if (!agentId || !cwd) return;

  const state = readAgentState(cwd);
  if (!state[sessionId]) state[sessionId] = [];
  if (!state[sessionId].includes(agentId)) {
    state[sessionId].push(agentId);
  }
  writeAgentState(cwd, state);
  log(`Agent ${agentId} started, active list: ${state[sessionId].join(", ")}`, 'info');

  if (transcriptPath) {
    inputData.agent_transcript_path = agentTranscriptPath(transcriptPath, sessionId, agentId);
  }
}

// SubagentStop: remove agent from active list, extract tool IDs and stop text
export function enrichSubagentStop(inputData: HookData, sessionId: string, cwd: string, transcriptPath: string | undefined): void {
  log(`[SUBAGENT_STOP_ENTERED] event processing started`, 'warn');
  const agentId = inputData.agent_id;
  if (!agentId || !cwd) return;

  const state = readAgentState(cwd);
  if (state[sessionId]) {
    state[sessionId] = state[sessionId].filter((id: string) => id !== agentId);
    if (state[sessionId].length === 0) delete state[sessionId];
  }
  writeAgentState(cwd, state);
  log(`Agent ${agentId} stopped, active list: ${state[sessionId]?.join(", ") || "(empty)"}`, 'warn');

  // Use agent_transcript_path provided by Claude Code (don't construct)
  const providedAtp = inputData.agent_transcript_path;
  log(`SubagentStop: agent_id=${agentId}, has_atp=${!!providedAtp}`, 'warn');
  if (!providedAtp) {
    log(`SubagentStop: Claude Code did not provide agent_transcript_path`, 'warn');
    return;
  }

  // Check if agent transcript file exists
  const atpExists = existsSync(providedAtp);
  log(`SubagentStop: atp_exists=${atpExists}, path=${providedAtp}`, 'warn');

  // Extract all tool_use IDs from agent transcript for definitive fix-up
  const toolIds = extractAgentToolIds(providedAtp);
  if (toolIds.length > 0) {
    inputData.agent_tool_ids = toolIds;
    log(`Agent ${agentId} had ${toolIds.length} tool calls`, 'warn');
  }

  // Extract trailing text from agent transcript (agent's final summary)
  //
  // EXPECTED BEHAVIOR (95% of cases):
  // - Agent writes all output to sidechain file (agent_transcript_path)
  // - Sidechain includes final summary text in last assistant message
  // - extractTrailingText() detects sidechain format via "isSidechain:true" marker
  // - Summary extracted and populated into agent_stop_text
  //
  // EDGE CASE BEHAVIOR (5% of cases):
  // - Agent completes very quickly (< 100ms) or during session transitions
  // - Sidechain file never materializes or remains empty
  // - Agent's final summary gets written to MAIN session transcript instead
  // - This is a quirk of Claude Code's agent subprocess lifecycle:
  //   1. Subprocess spawned with sidechain path
  //   2. Subprocess completes before sidechain write flushes
  //   3. Summary written to main transcript as fallback
  // - extractTrailingText(transcriptPath) handles both formats via auto-detection
  //
  // See: emacs-bridge/docs/transcript-extraction-behavior.md for details
  try {
    let { text, thinking } = extractTrailingText(providedAtp);
    log(`SubagentStop: initial extraction: text=${text.length}B, thinking=${thinking.length}B`, 'warn');
    if (!text && !thinking) {
      // Fallback: extract from main session transcript
      // Agent's summary may have been written there instead of sidechain
      // This happens when agent completes before sidechain write flushes.
      // extractTrailingText() auto-detects format and applies appropriate extraction:
      // - Main transcript: walk backward from end, stop at tool_use (turn boundary)
      // - Sidechain: find last assistant message, extract all text/thinking blocks
      log(`SubagentStop: no text/thinking in agent transcript, trying main transcript fallback`, 'warn');
      try {
        if (transcriptPath) ({ text, thinking } = extractTrailingText(transcriptPath));
        if (text || thinking) {
          log(`SubagentStop: extracted from main transcript: text=${text.length}B, thinking=${thinking.length}B`, 'warn');
        }
      } catch (e) {
        log(`SubagentStop: main transcript fallback failed: ${e}`, 'warn');
      }
    }
    if (text) {
      inputData.agent_stop_text = text;
      log(`Agent ${agentId} stop_text set (${text.length} chars)`, 'warn');
    }
    if (thinking) {
      inputData.agent_stop_thinking = thinking;
      log(`Agent ${agentId} stop_thinking set (${thinking.length} chars)`, 'warn');
    }
    if (!text && !thinking) {
      log(`SubagentStop: FAILED to extract any text/thinking for agent ${agentId}`, 'warn');
    }
  } catch (e) {
    log(`Failed to extract agent trailing content: ${e}`, 'error');
  }
}

// SessionEnd: clean up agent state for this session
export function enrichSessionEnd(sessionId: string, cwd: string): void {
  if (!cwd) return;
  const state = readAgentState(cwd);
  if (state[sessionId]) {
    delete state[sessionId];
    writeAgentState(cwd, state);
    log(`Cleaned up agent state for session ${sessionId}`, 'info');
  }
}

// Tool-to-agent attribution for PreToolUse/PostToolUse/PostToolUseFailure
export function enrichToolAttribution(inputData: HookData, sessionId: string, cwd: string, transcriptPath: string | undefined): void {
  if (!cwd) return;
  const state = readAgentState(cwd);
  const activeAgents = state[sessionId] || [];
  if (activeAgents.length === 0) return;

  const toolUseId = inputData.tool_use_id || "";
  const { parentAgentId, candidateAgentIds } = attributeToolToAgent(
    sessionId, cwd, transcriptPath, toolUseId, activeAgents
  );
  if (parentAgentId) {
    inputData.parent_agent_id = parentAgentId;
    if (candidateAgentIds) {
      inputData.candidate_agent_ids = candidateAgentIds;
    }
    log(`Tool ${toolUseId} attributed to agent: ${parentAgentId}`);
  }
}

// Enrich PreToolUse with assistant monologue text and thinking from transcript.
// For agent tools, read from the agent's transcript instead of the session transcript.
export function enrichPreToolUse(inputData: HookData, sessionId: string, transcriptPath: string | undefined): void {
  const toolUseId = inputData.tool_use_id;
  const parentAgentId = inputData.parent_agent_id;
  const effectiveTranscript = (parentAgentId && parentAgentId !== "ambiguous" && transcriptPath)
    ? agentTranscriptPath(transcriptPath, sessionId, parentAgentId)
    : transcriptPath;
  if (effectiveTranscript && toolUseId) {
    try {
      const { text, thinking, model } = extractPrecedingContent(effectiveTranscript, toolUseId);
      if (text) {
        inputData.assistant_text = text;
        log(`Extracted assistant text (${text.length} chars) for ${toolUseId}`);
      }
      if (thinking) {
        inputData.assistant_thinking = thinking;
        log(`Extracted thinking (${thinking.length} chars) for ${toolUseId}`);
      }
      if (model) {
        inputData.model = model;
      }
    } catch (e) {
      log(`Failed to extract preceding content: ${e}`, 'error');
    }
  }
  // For Task tools, extract the explicitly requested model from tool_input
  const toolName = inputData.tool_name;
  const toolInput = inputData.tool_input;
  if (toolName === "Task" && toolInput?.model) {
    inputData.requested_model = toolInput.model;
  }
}

// Enrich PostToolUse/PostToolUseFailure with assistant text that follows the tool_result.
// For agent tools, read from the agent's transcript instead of the session transcript.
export function enrichPostToolUse(inputData: HookData, sessionId: string, transcriptPath: string | undefined): void {
  const toolUseId = inputData.tool_use_id;
  const parentAgentId = inputData.parent_agent_id;
  const effectiveTranscript = (parentAgentId && parentAgentId !== "ambiguous" && transcriptPath)
    ? agentTranscriptPath(transcriptPath, sessionId, parentAgentId)
    : transcriptPath;
  if (effectiveTranscript && toolUseId) {
    try {
      const { text, thinking } = extractFollowingContent(effectiveTranscript, toolUseId);
      if (text) {
        inputData.post_tool_text = text;
        log(`Extracted post-tool text (${text.length} chars) for ${toolUseId}`);
      }
      if (thinking) {
        inputData.post_tool_thinking = thinking;
        log(`Extracted post-tool thinking (${thinking.length} chars) for ${toolUseId}`);
      }
    } catch (e) {
      log(`Failed to extract following content: ${e}`, 'error');
    }
  }
}

// Enrich Stop with trailing assistant text and token usage from transcript.
export async function enrichStop(inputData: HookData, transcriptPath: string | undefined): Promise<void> {
  if (!transcriptPath) return;

  try {
    // Capture transcript size at invocation time. The Stop hook fires
    // when a turn completes, but by the time we read the file the next
    // turn may have started appending data. Using the size at Stop time
    // ensures we only read data from the completed turn.
    let snapshotBytes: number | undefined;
    try {
      snapshotBytes = statSync(transcriptPath).size;
      log(`Stop: transcript snapshot ${snapshotBytes} bytes`, 'warn');
    } catch { /* file may not exist yet */ }

    let { text, thinking } = extractTrailingText(transcriptPath, snapshotBytes);
    // Always retry if the transcript file grows — the assistant's final
    // text may not be flushed yet when Stop fires.  The old logic only
    // retried when `text` was empty, but when the snapshot ends right
    // after the user prompt the extraction crosses the turn boundary and
    // returns the *previous* turn's text (non-empty but wrong).
    const maxRetries = 3;
    const delayMs = 150;
    for (let retry = 0; retry < maxRetries; retry++) {
      await new Promise(r => setTimeout(r, delayMs));
      let newSize: number | undefined;
      try { newSize = statSync(transcriptPath).size; } catch {}
      if (newSize && newSize > (snapshotBytes ?? 0)) {
        snapshotBytes = newSize;
        const extracted = extractTrailingText(transcriptPath, snapshotBytes);
        if (extracted.text) text = extracted.text;
        if (extracted.thinking) thinking = extracted.thinking;
        log(`Stop retry ${retry + 1}: file grew to ${newSize}, ${text.length} chars text, ${thinking.length} chars thinking`, 'warn');
      } else {
        break;  // file didn't grow, no point retrying
      }
    }
    if (text) {
      inputData.stop_text = text;
      log(`Stop: extracted trailing text (${text.length} chars)`, 'warn');
    } else {
      log(`Stop: no trailing text found`, 'warn');
    }
    if (thinking) {
      inputData.stop_thinking = thinking;
      log(`Stop: extracted trailing thinking (${thinking.length} chars)`, 'warn');
    }
  } catch (e) {
    log(`Failed to extract trailing content: ${e}`, 'error');
  }

  // Extract cumulative token usage
  try {
    const tokenUsage = extractTokenUsage(transcriptPath);
    inputData.token_usage = tokenUsage;
  } catch (e) {
    log(`Failed to extract token usage: ${e}`, 'error');
  }
}
