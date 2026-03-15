import { existsSync, statSync } from "fs";
import { log } from "./log.js";
import type { HookData } from "@gravity/shared";
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
export function enrichSessionMetadata(inputData: HookData, transcriptPath: string | undefined): HookData {
  if (!transcriptPath) return inputData;
  const meta = extractTranscriptMeta(transcriptPath);
  if (meta.slug) {
    log(`Extracted slug: ${meta.slug}`);
  }
  return {
    ...inputData,
    ...(meta.slug && { slug: meta.slug }),
    ...(meta.gitBranch && { branch: meta.gitBranch }),
  };
}

// SubagentStart: add agent to active list, inject transcript path
export function enrichSubagentStart(inputData: HookData, sessionId: string, cwd: string, transcriptPath: string | undefined): HookData {
  const agentId = inputData.agent_id;
  if (!agentId || !cwd) return inputData;

  const state = readAgentState(cwd, transcriptPath);
  if (!state[sessionId]) state[sessionId] = [];
  if (!state[sessionId].includes(agentId)) {
    state[sessionId].push(agentId);
  }
  writeAgentState(cwd, transcriptPath, state);
  log(`Agent ${agentId} started, active list: ${state[sessionId].join(", ")}`, 'info');

  if (!transcriptPath) return inputData;
  return {
    ...inputData,
    agent_transcript_path: agentTranscriptPath(transcriptPath, sessionId, agentId),
  };
}

// SubagentStop: remove agent from active list, extract tool IDs and stop text
export function enrichSubagentStop(inputData: HookData, sessionId: string, cwd: string, transcriptPath: string | undefined): HookData {
  log(`[SUBAGENT_STOP_ENTERED] event processing started`, 'warn');
  const agentId = inputData.agent_id;
  if (!agentId || !cwd) return inputData;

  const state = readAgentState(cwd, transcriptPath);
  if (state[sessionId]) {
    state[sessionId] = state[sessionId].filter((id: string) => id !== agentId);
    if (state[sessionId].length === 0) delete state[sessionId];
  }
  writeAgentState(cwd, transcriptPath, state);
  log(`Agent ${agentId} stopped, active list: ${state[sessionId]?.join(", ") || "(empty)"}`, 'warn');

  // Use agent_transcript_path provided by Claude Code (don't construct)
  const providedAtp = inputData.agent_transcript_path;
  log(`SubagentStop: agent_id=${agentId}, has_atp=${!!providedAtp}`, 'warn');
  if (!providedAtp) {
    log(`SubagentStop: Claude Code did not provide agent_transcript_path`, 'warn');
    return inputData;
  }

  // Check if agent transcript file exists
  const atpExists = existsSync(providedAtp);
  log(`SubagentStop: atp_exists=${atpExists}, path=${providedAtp}`, 'warn');

  // Extract all tool_use IDs from agent transcript for definitive fix-up
  const toolIds = extractAgentToolIds(providedAtp);
  if (toolIds.length > 0) {
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

  let text = "";
  let thinking = "";
  try {
    const extracted = extractTrailingText(providedAtp);
    text = extracted.text;
    thinking = extracted.thinking;
    log(`SubagentStop: initial extraction: text=${text.length}B, thinking=${thinking.length}B`, 'warn');
    if (!text && !thinking) {
      // Fallback: extract from main session transcript
      log(`SubagentStop: no text/thinking in agent transcript, trying main transcript fallback`, 'warn');
      try {
        if (transcriptPath) {
          const fallback = extractTrailingText(transcriptPath);
          text = fallback.text;
          thinking = fallback.thinking;
        }
        if (text || thinking) {
          log(`SubagentStop: extracted from main transcript: text=${text.length}B, thinking=${thinking.length}B`, 'warn');
        }
      } catch (e) {
        log(`SubagentStop: main transcript fallback failed: ${e}`, 'warn');
      }
    }
    if (!text && !thinking) {
      log(`SubagentStop: FAILED to extract any text/thinking for agent ${agentId}`, 'warn');
    }
  } catch (e) {
    log(`Failed to extract agent trailing content: ${e}`, 'error');
  }

  return {
    ...inputData,
    ...(toolIds.length > 0 && { agent_tool_ids: toolIds }),
    ...(text && { agent_stop_text: text }),
    ...(thinking && { agent_stop_thinking: thinking }),
  };
}

// SessionEnd: clean up agent state for this session
// Note: This function doesn't enrich inputData, it just cleans up state.
// Returns inputData for potential chain compatibility.
export function enrichSessionEnd(inputData: HookData, sessionId: string, cwd: string, transcriptPath?: string): HookData {
  if (!cwd) return inputData;
  const state = readAgentState(cwd, transcriptPath);
  if (state[sessionId]) {
    delete state[sessionId];
    writeAgentState(cwd, transcriptPath, state);
    log(`Cleaned up agent state for session ${sessionId}`, 'info');
  }
  return inputData;
}

// Tool-to-agent attribution for PreToolUse/PostToolUse/PostToolUseFailure
export function enrichToolAttribution(inputData: HookData, sessionId: string, cwd: string, transcriptPath: string | undefined): HookData {
  if (!cwd) return inputData;
  const state = readAgentState(cwd, transcriptPath);
  const activeAgents = state[sessionId] || [];
  if (activeAgents.length === 0) return inputData;

  const toolUseId = inputData.tool_use_id || "";
  const { parentAgentId, candidateAgentIds } = attributeToolToAgent(
    sessionId, cwd, transcriptPath, toolUseId, activeAgents
  );
  if (!parentAgentId) return inputData;

  log(`Tool ${toolUseId} attributed to agent: ${parentAgentId}`);
  return {
    ...inputData,
    parent_agent_id: parentAgentId,
    ...(candidateAgentIds && { candidate_agent_ids: candidateAgentIds }),
  };
}

// Enrich PreToolUse with assistant monologue text and thinking from transcript.
// For agent tools, read from the agent's transcript instead of the session transcript.
export function enrichPreToolUse(inputData: HookData, sessionId: string, transcriptPath: string | undefined): HookData {
  const toolUseId = inputData.tool_use_id;
  const parentAgentId = inputData.parent_agent_id;
  const effectiveTranscript = (parentAgentId && parentAgentId !== "ambiguous" && transcriptPath)
    ? agentTranscriptPath(transcriptPath, sessionId, parentAgentId)
    : transcriptPath;

  if (!effectiveTranscript || !toolUseId) return inputData;

  let text = "";
  let thinking = "";
  let model: string | undefined;

  try {
    const extracted = extractPrecedingContent(effectiveTranscript, toolUseId);
    text = extracted.text;
    thinking = extracted.thinking;
    model = extracted.model;
    if (text) {
      log(`Extracted assistant text (${text.length} chars) for ${toolUseId}`);
    }
    if (thinking) {
      log(`Extracted thinking (${thinking.length} chars) for ${toolUseId}`);
    }
  } catch (e) {
    log(`Failed to extract preceding content: ${e}`, 'error');
  }

  // For Task tools, extract the explicitly requested model from tool_input
  const toolName = inputData.tool_name;
  const toolInput = inputData.tool_input;
  const requestedModel = (toolName === "Task" && toolInput?.model) ? String(toolInput.model) : undefined;

  return {
    ...inputData,
    ...(text && { assistant_text: text }),
    ...(thinking && { assistant_thinking: thinking }),
    ...(model && { model }),
    ...(requestedModel && { requested_model: requestedModel }),
  };
}

// Enrich PostToolUse/PostToolUseFailure with assistant text that follows the tool_result.
// For agent tools, read from the agent's transcript instead of the session transcript.
export function enrichPostToolUse(inputData: HookData, sessionId: string, transcriptPath: string | undefined): HookData {
  const toolUseId = inputData.tool_use_id;
  const parentAgentId = inputData.parent_agent_id;
  const effectiveTranscript = (parentAgentId && parentAgentId !== "ambiguous" && transcriptPath)
    ? agentTranscriptPath(transcriptPath, sessionId, parentAgentId)
    : transcriptPath;

  if (!effectiveTranscript || !toolUseId) return inputData;

  let text = "";
  let thinking = "";

  try {
    const extracted = extractFollowingContent(effectiveTranscript, toolUseId);
    text = extracted.text;
    thinking = extracted.thinking;
    if (text) {
      log(`Extracted post-tool text (${text.length} chars) for ${toolUseId}`);
    }
    if (thinking) {
      log(`Extracted post-tool thinking (${thinking.length} chars) for ${toolUseId}`);
    }
  } catch (e) {
    log(`Failed to extract following content: ${e}`, 'error');
  }

  return {
    ...inputData,
    ...(text && { post_tool_text: text }),
    ...(thinking && { post_tool_thinking: thinking }),
  };
}

// Enrich Stop with trailing assistant text and token usage from transcript.
export async function enrichStop(inputData: HookData, transcriptPath: string | undefined): Promise<HookData> {
  if (!transcriptPath) return inputData;

  let text = "";
  let thinking = "";

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

    const extracted = extractTrailingText(transcriptPath, snapshotBytes);
    text = extracted.text;
    thinking = extracted.thinking;

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
        const retryExtracted = extractTrailingText(transcriptPath, snapshotBytes);
        if (retryExtracted.text) text = retryExtracted.text;
        if (retryExtracted.thinking) thinking = retryExtracted.thinking;
        log(`Stop retry ${retry + 1}: file grew to ${newSize}, ${text.length} chars text, ${thinking.length} chars thinking`, 'warn');
      } else {
        break;  // file didn't grow, no point retrying
      }
    }

    if (!text) {
      log(`Stop: no trailing text found`, 'warn');
    }
  } catch (e) {
    log(`Failed to extract trailing content: ${e}`, 'error');
  }

  // Extract cumulative token usage
  let tokenUsage: import("./types.js").TokenUsage | undefined;
  try {
    tokenUsage = extractTokenUsage(transcriptPath);
  } catch (e) {
    log(`Failed to extract token usage: ${e}`, 'error');
  }

  return {
    ...inputData,
    ...(text && { stop_text: text }),
    ...(thinking && { stop_thinking: thinking }),
    ...(tokenUsage && { token_usage: tokenUsage }),
  };
}
