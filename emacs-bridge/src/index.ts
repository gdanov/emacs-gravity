import { createConnection } from "net";
import { readFileSync, writeFileSync, existsSync, mkdirSync, statSync } from "fs";
import { join, dirname, basename } from "path";
import { execSync } from "child_process";
import { log } from "./log";

// Re-export transcript functions from enrichment (canonical location)
export {
  readTail,
  readHead,
  extractPrecedingContent,
  extractFollowingContent,
  extractTrailingText,
  extractTokenUsage,
  extractTranscriptMeta,
  extractSlug,
} from "./enrichment";

// --- Fixture dump mode ---
// When CLAUDE_GRAVITY_DUMP_DIR is set, save raw input and enriched output
// as JSON files for replay testing. Files are named {seq}__{event}__{suffix}.json
// where seq is a zero-padded counter preserving event ordering.
function nextDumpSeq(dumpDir: string): number {
  if (!existsSync(dumpDir)) mkdirSync(dumpDir, { recursive: true });
  const counterFile = join(dumpDir, "_counter.txt");
  let counter = 0;
  try { counter = parseInt(readFileSync(counterFile, "utf-8").trim(), 10) || 0; } catch {}
  counter++;
  writeFileSync(counterFile, String(counter));
  return counter;
}

function writeDumpFile(dumpDir: string, seq: number, eventName: string, suffix: string, data: any): void {
  try {
    const filename = `${String(seq).padStart(4, "0")}__${eventName}__${suffix}.json`;
    writeFileSync(join(dumpDir, filename), JSON.stringify(data, null, 2) + "\n");
  } catch (e) {
    log(`writeDumpFile error: ${e}`, 'error');
  }
}

// Import transcript functions for use in main()
import {
  readTail,
  extractPrecedingContent,
  extractFollowingContent,
  extractTrailingText,
  extractTokenUsage,
  extractTranscriptMeta,
} from "./enrichment";

// Resolve socket path from plugin root or fallback to relative location
function getSocketPath(): string {
  const pluginRoot = process.env.CLAUDE_PLUGIN_ROOT;
  if (pluginRoot) {
    return join(pluginRoot, "..", "claude-gravity.sock");
  }
  return join(__dirname, "..", "..", "claude-gravity.sock");
}

// Helper to send data to Emacs socket
async function sendToEmacs(eventName: string, sessionId: string, cwd: string, pid: number | null, payload: any, hookInput?: any) {
  log(`Sending event: ${eventName} session: ${sessionId}`);
  const socketPath = getSocketPath();
  log(`Socket path: ${socketPath}`);

  return new Promise<void>((resolve, reject) => {
    const client = createConnection(socketPath);

    client.on("connect", () => {
      log("Connected to socket");
      const msg: any = { event: eventName, session_id: sessionId, cwd: cwd, pid: pid, data: payload };
      if (hookInput) msg.hook_input = hookInput;
      const message = JSON.stringify(msg) + "\n";
      client.write(message);
      client.end();
    });

    client.on("error", (err) => {
      log(`Socket error: ${err.message}`, 'error');
      // Fail silently to avoid blocking Claude if Emacs is down
      resolve();
    });

    client.on("close", () => {
      log("Connection closed");
      resolve();
    });
  });
}

// Helper to send data to Emacs socket and wait for a response (bidirectional).
// Used for PermissionRequest where Emacs must reply with allow/deny.
async function sendToEmacsAndWait(eventName: string, sessionId: string, cwd: string, pid: number | null, payload: any, timeoutMs: number = 345600000, hookInput?: any): Promise<any> {
  log(`Sending event (wait): ${eventName} session: ${sessionId}`);
  const socketPath = getSocketPath();

  return new Promise<any>((resolve) => {
    const client = createConnection(socketPath);
    let responded = false;
    let buffer = "";

    const timer = setTimeout(() => {
      if (!responded) {
        responded = true;
        log(`sendToEmacsAndWait timeout after ${timeoutMs}ms`, 'warn');
        client.destroy();
        resolve({});
      }
    }, timeoutMs);

    client.on("connect", () => {
      log("Connected to socket (wait mode)");
      const msg: any = { event: eventName, session_id: sessionId, cwd: cwd, pid: pid, needs_response: true, data: payload };
      if (hookInput) msg.hook_input = hookInput;
      const message = JSON.stringify(msg) + "\n";
      client.write(message);
      // Do NOT call client.end() — keep connection open for response
    });

    client.on("data", (chunk) => {
      buffer += chunk.toString();
      const newlineIdx = buffer.indexOf("\n");
      if (newlineIdx >= 0) {
        const line = buffer.substring(0, newlineIdx);
        if (!responded) {
          responded = true;
          clearTimeout(timer);
          try {
            const response = JSON.parse(line);
            log(`Received response: ${JSON.stringify(response)}`);
            resolve(response);
          } catch (e) {
            log(`Failed to parse response: ${e}`, 'error');
            resolve({});
          }
          client.end();
        }
      }
    });

    client.on("error", (err) => {
      if (!responded) {
        responded = true;
        clearTimeout(timer);
        log(`Socket error (wait): ${err.message}`, 'error');
        resolve({});
      }
    });

    client.on("close", () => {
      if (!responded) {
        responded = true;
        clearTimeout(timer);
        log("Connection closed before response", 'warn');
        resolve({});
      }
    });
  });
}

// Transcript functions are now in enrichment.ts — imported above

// --- Active Agent List ---
// Tracks which agents are currently running per session.
// Persisted to {cwd}/.claude/emacs-bridge-agents.json so each
// one-shot bridge invocation can read the current state.

type AgentState = { [sessionId: string]: string[] };

function getAgentStatePath(cwd: string): string {
  return join(cwd, ".claude", "emacs-bridge-agents.json");
}

function readAgentState(cwd: string): AgentState {
  try {
    const p = getAgentStatePath(cwd);
    if (existsSync(p)) {
      return JSON.parse(readFileSync(p, "utf-8"));
    }
  } catch (e) {
    log(`readAgentState error: ${e}`, 'error');
  }
  return {};
}

function writeAgentState(cwd: string, state: AgentState): void {
  try {
    const p = getAgentStatePath(cwd);
    const dir = dirname(p);
    if (!existsSync(dir)) mkdirSync(dir, { recursive: true });
    writeFileSync(p, JSON.stringify(state), "utf-8");
  } catch (e) {
    log(`writeAgentState error: ${e}`, 'error');
  }
}

function agentTranscriptPath(transcriptPath: string, sessionId: string, agentId: string): string {
  const transcriptDir = dirname(transcriptPath);
  const sessionBase = basename(transcriptPath, ".jsonl");
  return join(transcriptDir, sessionBase, "subagents", `agent-${agentId}.jsonl`);
}

// Search an agent transcript for a specific tool_use_id.
// Returns true if the tool_use_id is found in the transcript.
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
  } catch (e) {
    log(`transcriptHasToolUseId error: ${e}`, 'error');
  }
  return false;
}

// Extract all tool_use IDs from an agent transcript.
// Called on SubagentStop for definitive attribution fix-up.
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
  } catch (e) {
    log(`extractAgentToolIds error: ${e}`, 'error');
  }
  return ids;
}

// Attribute a tool event to a specific agent.
// Returns the agent_id or "ambiguous" or null (root tool).
function attributeToolToAgent(
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

async function main() {
  log(`Process started: ${process.argv.join(" ")}`);
  try {
    const eventName = process.argv[2]; // e.g., "PreToolUse"

    // Read STDIN
    // Note: readFileSync(0) reads from stdin file descriptor
    let inputData = {};
    try {
      const stdinBuffer = readFileSync(0);
      if (stdinBuffer.length > 0) {
        inputData = JSON.parse(stdinBuffer.toString());
      }
    } catch (e) {
      // Ignore stdin read errors (e.g. if no input provided)
    }

    log(`Payload: ${JSON.stringify(inputData)}`);

    // Snapshot raw hook input before any enrichment mutations.
    // Sent alongside enriched data so Emacs debug viewer can show both.
    const rawHookInput = JSON.parse(JSON.stringify(inputData));

    // Extract session identifiers from hook input
    const sessionId = (inputData as any).session_id || "unknown";
    const cwd = (inputData as any).cwd || "";
    const pid = parseInt(process.env.CLAUDE_PID || "0", 10) || null;
    const tempId = process.env.CLAUDE_GRAVITY_TEMP_ID || null;
    if (tempId) {
      (inputData as any).temp_id = tempId;
    }

    // Detect tmux session name from $TMUX env var
    if (process.env.TMUX) {
      try {
        const tmuxName = execSync('tmux display-message -p "#{session_name}"',
          { encoding: 'utf-8', timeout: 1000 }).trim();
        if (tmuxName) {
          (inputData as any).tmux_session = tmuxName;
        }
      } catch {}
    }

    // Dump raw input if dump mode enabled
    const dumpDir = process.env.CLAUDE_GRAVITY_DUMP_DIR;
    let dumpSeq: number | undefined;
    if (dumpDir) {
      dumpSeq = nextDumpSeq(dumpDir);
      writeDumpFile(dumpDir, dumpSeq, eventName, "raw", inputData);
    }

    // Extract session metadata from transcript (slug, gitBranch)
    const transcriptPath = (inputData as any).transcript_path;
    if (transcriptPath) {
      const meta = extractTranscriptMeta(transcriptPath);
      if (meta.slug) {
        (inputData as any).slug = meta.slug;
        log(`Extracted slug: ${meta.slug}`);
      }
      if (meta.gitBranch) {
        (inputData as any).branch = meta.gitBranch;
      }
    }

    // --- Agent tracking ---
    // SubagentStart: add agent to active list
    if (eventName === "SubagentStart") {
      const agentId = (inputData as any).agent_id;
      if (agentId && cwd) {
        const state = readAgentState(cwd);
        if (!state[sessionId]) state[sessionId] = [];
        if (!state[sessionId].includes(agentId)) {
          state[sessionId].push(agentId);
        }
        writeAgentState(cwd, state);
        log(`Agent ${agentId} started, active list: ${state[sessionId].join(", ")}`, 'info');
        // Inject agent transcript path
        if (transcriptPath) {
          (inputData as any).agent_transcript_path = agentTranscriptPath(transcriptPath, sessionId, agentId);
        }
      }
    }

    // SubagentStop: remove agent from active list, extract all tool IDs for fix-up
    if (eventName === "SubagentStop") {
      log(`[SUBAGENT_STOP_ENTERED] event processing started`, 'warn');
      const agentId = (inputData as any).agent_id;

      if (agentId && cwd) {
        const state = readAgentState(cwd);
        if (state[sessionId]) {
          state[sessionId] = state[sessionId].filter((id: string) => id !== agentId);
          if (state[sessionId].length === 0) delete state[sessionId];
        }
        writeAgentState(cwd, state);
        log(`Agent ${agentId} stopped, active list: ${state[sessionId]?.join(", ") || "(empty)"}`, 'warn');

        // Use agent_transcript_path provided by Claude Code (don't construct)
        const providedAtp = (inputData as any).agent_transcript_path;
        log(`SubagentStop: agent_id=${agentId}, has_atp=${!!providedAtp}`, 'warn');
        if (providedAtp) {
          // Check if agent transcript file exists
          const atpExists = existsSync(providedAtp);
          log(`SubagentStop: atp_exists=${atpExists}, path=${providedAtp}`, 'warn');

          // Extract all tool_use IDs from agent transcript for definitive fix-up
          const toolIds = extractAgentToolIds(providedAtp);
          if (toolIds.length > 0) {
            (inputData as any).agent_tool_ids = toolIds;
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
                ({ text, thinking } = extractTrailingText(transcriptPath));
                if (text || thinking) {
                  log(`SubagentStop: extracted from main transcript: text=${text.length}B, thinking=${thinking.length}B`, 'warn');
                }
              } catch (e) {
                log(`SubagentStop: main transcript fallback failed: ${e}`, 'warn');
              }
            }
            if (text) {
              (inputData as any).agent_stop_text = text;
              log(`Agent ${agentId} stop_text set (${text.length} chars)`, 'warn');
            }
            if (thinking) {
              (inputData as any).agent_stop_thinking = thinking;
              log(`Agent ${agentId} stop_thinking set (${thinking.length} chars)`, 'warn');
            }
            if (!text && !thinking) {
              log(`SubagentStop: FAILED to extract any text/thinking for agent ${agentId}`, 'warn');
            }
          } catch (e) {
            log(`Failed to extract agent trailing content: ${e}`, 'error');
          }
        } else {
          log(`SubagentStop: Claude Code did not provide agent_transcript_path`, 'warn');
        }
      }
    }

    // SessionEnd: clean up agent state for this session
    if (eventName === "SessionEnd") {
      if (cwd) {
        const state = readAgentState(cwd);
        if (state[sessionId]) {
          delete state[sessionId];
          writeAgentState(cwd, state);
          log(`Cleaned up agent state for session ${sessionId}`, 'info');
        }
      }
    }

    // Tool-to-agent attribution for PreToolUse/PostToolUse/PostToolUseFailure
    if (eventName === "PreToolUse" || eventName === "PostToolUse" || eventName === "PostToolUseFailure") {
      if (cwd) {
        const state = readAgentState(cwd);
        const activeAgents = state[sessionId] || [];
        if (activeAgents.length > 0) {
          const toolUseId = (inputData as any).tool_use_id || "";
          const { parentAgentId, candidateAgentIds } = attributeToolToAgent(
            sessionId, cwd, transcriptPath, toolUseId, activeAgents
          );
          if (parentAgentId) {
            (inputData as any).parent_agent_id = parentAgentId;
            if (candidateAgentIds) {
              (inputData as any).candidate_agent_ids = candidateAgentIds;
            }
            log(`Tool ${toolUseId} attributed to agent: ${parentAgentId}`);
          }
        }
      }
    }

    // Enrich PreToolUse with assistant monologue text and thinking from transcript.
    // For agent tools, read from the agent's transcript instead of the session transcript.
    if (eventName === "PreToolUse") {
      const toolUseId = (inputData as any).tool_use_id;
      const parentAgentId = (inputData as any).parent_agent_id;
      const effectiveTranscript = (parentAgentId && parentAgentId !== "ambiguous" && transcriptPath)
        ? agentTranscriptPath(transcriptPath, sessionId, parentAgentId)
        : transcriptPath;
      if (effectiveTranscript && toolUseId) {
        try {
          const { text, thinking, model } = extractPrecedingContent(effectiveTranscript, toolUseId);
          if (text) {
            (inputData as any).assistant_text = text;
            log(`Extracted assistant text (${text.length} chars) for ${toolUseId}`);
          }
          if (thinking) {
            (inputData as any).assistant_thinking = thinking;
            log(`Extracted thinking (${thinking.length} chars) for ${toolUseId}`);
          }
          if (model) {
            (inputData as any).model = model;
          }
        } catch (e) {
          log(`Failed to extract preceding content: ${e}`, 'error');
        }
      }
      // For Task tools, extract the explicitly requested model from tool_input
      const toolName = (inputData as any).tool_name;
      const toolInput = (inputData as any).tool_input;
      if (toolName === "Task" && toolInput?.model) {
        (inputData as any).requested_model = toolInput.model;
      }
    }

    // Enrich PostToolUse/PostToolUseFailure with assistant text that follows the tool_result.
    // For agent tools, read from the agent's transcript instead of the session transcript.
    if (eventName === "PostToolUse" || eventName === "PostToolUseFailure") {
      const toolUseId = (inputData as any).tool_use_id;
      const parentAgentId = (inputData as any).parent_agent_id;
      const effectiveTranscript = (parentAgentId && parentAgentId !== "ambiguous" && transcriptPath)
        ? agentTranscriptPath(transcriptPath, sessionId, parentAgentId)
        : transcriptPath;
      if (effectiveTranscript && toolUseId) {
        try {
          const { text, thinking } = extractFollowingContent(effectiveTranscript, toolUseId);
          if (text) {
            (inputData as any).post_tool_text = text;
            log(`Extracted post-tool text (${text.length} chars) for ${toolUseId}`);
          }
          if (thinking) {
            (inputData as any).post_tool_thinking = thinking;
            log(`Extracted post-tool thinking (${thinking.length} chars) for ${toolUseId}`);
          }
        } catch (e) {
          log(`Failed to extract following content: ${e}`, 'error');
        }
      }
    }

    // Enrich Stop with trailing assistant text from transcript.
    // The Stop hook often fires before the final assistant text is flushed
    // to the transcript file (race condition), so we retry with a short
    // delay when the initial read finds nothing.
    if (eventName === "Stop") {
      if (transcriptPath) {
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
          // Retry if text is missing, even if thinking was found.
          // The text message is typically the last to be flushed and contains
          // the actual assistant response (stop_text). Thinking may arrive earlier,
          // causing the initial extraction to miss the subsequent text message.
          if (!text) {
            const maxRetries = 3;
            const delayMs = 100;
            for (let retry = 0; retry < maxRetries && !text; retry++) {
              await new Promise(r => setTimeout(r, delayMs));
              // Re-stat to get updated size (file may be flushing)
              try { snapshotBytes = statSync(transcriptPath).size; } catch {}
              const extracted = extractTrailingText(transcriptPath, snapshotBytes);
              if (extracted.text) text = extracted.text;
              // Only update thinking if we didn't find it in the first attempt
              if (!thinking && extracted.thinking) thinking = extracted.thinking;
              log(`Stop retry ${retry + 1}: ${text.length} chars text, ${thinking.length} chars thinking`, 'warn');
            }
          }
          if (text) {
            (inputData as any).stop_text = text;
            log(`Stop: extracted trailing text (${text.length} chars)`, 'warn');
          } else {
            log(`Stop: no trailing text found`, 'warn');
          }
          if (thinking) {
            (inputData as any).stop_thinking = thinking;
            log(`Stop: extracted trailing thinking (${thinking.length} chars)`, 'warn');
          }
        } catch (e) {
          log(`Failed to extract trailing content: ${e}`, 'error');
        }
        // Extract cumulative token usage
        try {
          const tokenUsage = extractTokenUsage(transcriptPath);
          (inputData as any).token_usage = tokenUsage;
        } catch (e) {
          log(`Failed to extract token usage: ${e}`, 'error');
        }
      }
    }

    // Dump enriched output (socket message format) if dump mode enabled
    if (dumpDir && dumpSeq !== undefined) {
      writeDumpFile(dumpDir, dumpSeq, eventName, "output", {
        event: eventName, session_id: sessionId, cwd, pid, data: inputData,
      });
    }

    // Send to Emacs — PermissionRequest and AskUserQuestionIntercept use bidirectional wait
    if (eventName === "PermissionRequest") {
      const response = await sendToEmacsAndWait(eventName, sessionId, cwd, pid, inputData, undefined, rawHookInput);
      console.log(JSON.stringify(response));
    } else if (eventName === "AskUserQuestionIntercept") {
      const response = await sendToEmacsAndWait("PreToolUse", sessionId, cwd, pid, inputData, undefined, rawHookInput);
      if (response && response.hookSpecificOutput) {
        console.log(JSON.stringify(response));
      } else {
        // Fallback: allow terminal execution
        console.log(JSON.stringify({}));
      }
    } else {
      // Debug: log what we're sending for SubagentStop
      if (eventName === "SubagentStop") {
        log(`[FINAL_CHECK_BEFORE_SEND] agent_stop_text=${typeof (inputData as any).agent_stop_text === 'string' ? `"${((inputData as any).agent_stop_text as string).substring(0, 40)}"` : (inputData as any).agent_stop_text}`, 'warn');
      }
      await sendToEmacs(eventName, sessionId, cwd, pid, inputData, rawHookInput);
      // Always output valid JSON to stdout as expected by Claude Code
      console.log(JSON.stringify({}));
    }
  } catch (error) {
    // Log error to stderr but output valid JSON to stdout to not break Claude
    // console.error("Emacs Bridge Logic Error:", error);
    console.log(JSON.stringify({}));
  }
}

if (require.main === module) {
  const args = process.argv.slice(2);
  if (args.includes('--mode') && args[args.indexOf('--mode') + 1] === 'opencode') {
    import('./opencode-bridge').then(() => {
      log('OpenCode bridge started');
    }).catch(e => {
      console.error('Failed to load opencode-bridge:', e);
      process.exit(1);
    });
  } else {
    main();
  }
}
