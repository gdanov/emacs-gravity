import { createConnection } from "net";
import { readFileSync, writeFileSync, appendFileSync, existsSync, mkdirSync, statSync, openSync, readSync, closeSync } from "fs";
import { join, dirname, basename } from "path";

// Debug logging
function log(msg: string) {
  try {
    const timestamp = new Date().toISOString();
    appendFileSync("/tmp/emacs-bridge.log", `[${timestamp}] ${msg}\n`);
  } catch (e) {
    // ignore logging errors
  }
}

// Resolve socket path from plugin root or fallback to relative location
function getSocketPath(): string {
  const pluginRoot = process.env.CLAUDE_PLUGIN_ROOT;
  if (pluginRoot) {
    return join(pluginRoot, "..", "claude-gravity.sock");
  }
  return join(__dirname, "..", "..", "claude-gravity.sock");
}

// Helper to send data to Emacs socket
async function sendToEmacs(eventName: string, sessionId: string, cwd: string, pid: number | null, payload: any) {
  log(`Sending event: ${eventName} session: ${sessionId}`);
  const socketPath = getSocketPath();
  log(`Socket path: ${socketPath}`);

  return new Promise<void>((resolve, reject) => {
    const client = createConnection(socketPath);

    client.on("connect", () => {
      log("Connected to socket");
      const message =
        JSON.stringify({ event: eventName, session_id: sessionId, cwd: cwd, pid: pid, data: payload }) + "\n";
      client.write(message);
      client.end();
    });

    client.on("error", (err) => {
      log(`Socket error: ${err.message}`);
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
async function sendToEmacsAndWait(eventName: string, sessionId: string, cwd: string, pid: number | null, payload: any, timeoutMs: number = 345600000): Promise<any> {
  log(`Sending event (wait): ${eventName} session: ${sessionId}`);
  const socketPath = getSocketPath();

  return new Promise<any>((resolve) => {
    const client = createConnection(socketPath);
    let responded = false;
    let buffer = "";

    const timer = setTimeout(() => {
      if (!responded) {
        responded = true;
        log(`sendToEmacsAndWait timeout after ${timeoutMs}ms`);
        client.destroy();
        resolve({});
      }
    }, timeoutMs);

    client.on("connect", () => {
      log("Connected to socket (wait mode)");
      const message =
        JSON.stringify({ event: eventName, session_id: sessionId, cwd: cwd, pid: pid, needs_response: true, data: payload }) + "\n";
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
            log(`Failed to parse response: ${e}`);
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
        log(`Socket error (wait): ${err.message}`);
        resolve({});
      }
    });

    client.on("close", () => {
      if (!responded) {
        responded = true;
        clearTimeout(timer);
        log("Connection closed before response");
        resolve({});
      }
    });
  });
}

// Read the tail of a file (last maxBytes), skipping any partial first line.
// If the file is smaller than maxBytes, reads the whole file.
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

// Extract assistant text and thinking that precede a tool_use.
// The tool_use may not be in the transcript yet (PreToolUse fires before transcript write),
// so we fall back to reading the most recent assistant text/thinking from the end.
export function extractPrecedingContent(transcriptPath: string, toolUseId: string): { text: string; thinking: string } {
  const result = { text: "", thinking: "" };
  try {
    const content = readTail(transcriptPath, 10 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);

    // Try to find the tool_use line matching toolUseId (may exist if transcript is pre-written)
    let startIdx = -1;
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        if (c[0].type === "tool_use" && c[0].id === toolUseId) {
          startIdx = i;
          break;
        }
      } catch {
        continue;
      }
    }

    // If tool_use not found (common: PreToolUse fires before transcript write),
    // start from the end of the transcript
    if (startIdx < 0) {
      startIdx = lines.length;
    }

    // Walk backwards collecting all text and thinking blocks before this tool_use
    const textParts: string[] = [];
    for (let i = startIdx - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        // Skip non-message entries (progress, file-history-snapshot, summary, etc.)
        if (obj.type !== "assistant" && obj.type !== "user") continue;
        if (obj.type === "user") break; // hit user message — stop
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        const blockType = c[0].type;
        if (blockType === "tool_use") continue; // parallel sibling or preceding tool, skip
        if (blockType === "tool_result") break; // hit a tool result — stop
        if (blockType === "text") {
          const text = c[0].text || "";
          if (text && text !== "(no content)") {
            textParts.unshift(text); // prepend to maintain order
          }
          continue; // keep looking for thinking
        }
        if (blockType === "thinking" && !result.thinking) {
          result.thinking = c[0].thinking || "";
          break; // thinking is always first, stop searching
        }
        // Any other block type — stop
        break;
      } catch {
        continue;
      }
    }
    result.text = textParts.join("\n\n");
    return result;
  } catch (e) {
    log(`extractPrecedingContent error: ${e}`);
    return result;
  }
}

// Extract trailing assistant text/thinking after the last tool interaction.
// Extract cumulative token usage from the transcript JSONL.
// Sums usage fields across all entries that contain a "usage" object.
export function extractTokenUsage(transcriptPath: string): {
  input_tokens: number;
  output_tokens: number;
  cache_read_input_tokens: number;
  cache_creation_input_tokens: number;
} {
  const result = { input_tokens: 0, output_tokens: 0, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 };
  try {
    const content = readTail(transcriptPath, 10 * 1024 * 1024);
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
      } catch {
        continue;
      }
    }
    log(`extractTokenUsage: in=${result.input_tokens} out=${result.output_tokens} cache_read=${result.cache_read_input_tokens} cache_create=${result.cache_creation_input_tokens}`);
  } catch (e) {
    log(`extractTokenUsage error: ${e}`);
  }
  return result;
}

// Extract assistant text and thinking that follow a tool_result.
// Scans forward from the tool_result to find any assistant text/thinking
// that appears after the tool and before the next tool_use or end of transcript.
export function extractFollowingContent(transcriptPath: string, toolUseId: string): { text: string; thinking: string } {
  const result = { text: "", thinking: "" };
  try {
    const content = readTail(transcriptPath, 10 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);

    // Find the tool_result matching toolUseId
    let toolResultIdx = -1;
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "user" && obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c)) continue;
        // Look for tool_result with matching id
        for (const block of c) {
          if (block.type === "tool_result" && block.tool_use_id === toolUseId) {
            toolResultIdx = i;
            break;
          }
        }
        if (toolResultIdx >= 0) break;
      } catch {
        continue;
      }
    }

    // If tool_result not found, return empty
    if (toolResultIdx < 0) {
      return result;
    }

    // Walk forward from after the tool_result, collecting assistant text/thinking
    // Stop when we hit: another tool_use, end of transcript, or user message
    const textParts: string[] = [];
    for (let i = toolResultIdx + 1; i < lines.length; i++) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant") break; // hit user or other, stop
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) break;
        const blockType = c[0].type;

        // Collect text blocks
        if (blockType === "text") {
          const text = c[0].text || "";
          if (text && text !== "(no content)") {
            textParts.push(text);
          }
          continue; // keep looking for more text
        }

        // Extract thinking (do not break, keep looking for text after)
        if (blockType === "thinking") {
          if (!result.thinking) {
            result.thinking = c[0].thinking || "";
          }
          continue; // keep looking for text after thinking
        }

        // Hit tool_use or tool_result — we've gone past the following content
        break;
      } catch {
        continue;
      }
    }

    result.text = textParts.join("\n\n");
    return result;
  } catch (e) {
    log(`extractFollowingContent error: ${e}`);
    return result;
  }
}

// Called on Stop events to capture the conclusion text.
export function extractTrailingText(transcriptPath: string): { text: string; thinking: string } {
  const result = { text: "", thinking: "" };
  try {
    const content = readTail(transcriptPath, 10 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);

    // Diagnostic: log the last N transcript entries so we can verify what
    // the file actually contains when the Stop hook fires.
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
    // Walk backwards from end, collecting trailing assistant text/thinking
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        // Skip non-message entries (progress, system, summary, etc.)
        if (obj.type !== "assistant" && obj.type !== "user") continue;
        if (obj.type === "user") { stopReason = `user@${i}`; break; }
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        const blockType = c[0].type;
        if (blockType === "text") {
          const text = c[0].text || "";
          if (text && text !== "(no content)") {
            textParts.unshift(text); // prepend to maintain order
          }
          continue;
        }
        if (blockType === "thinking") {
          result.thinking = c[0].thinking || "";
          stopReason = `thinking@${i}`;
          break; // thinking is always first
        }
        // tool_use or other — we've gone past the trailing text
        stopReason = `${blockType}@${i}`;
        break;
      } catch {
        continue;
      }
    }
    result.text = textParts.join("\n\n");
    log(`extractTrailingText result: ${result.text.length} chars text, ${result.thinking.length} chars thinking, stop=${stopReason}, parts=${textParts.length}`);
    return result;
  } catch (e) {
    log(`extractTrailingText error: ${e}`);
    return result;
  }
}

// Extract session slug from the transcript JSONL.
// The slug field appears on `progress` entries which may not be the first line
// (e.g. `file-history-snapshot` often comes first), so we scan several lines.
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
    log(`readAgentState error: ${e}`);
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
    log(`writeAgentState error: ${e}`);
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
    log(`transcriptHasToolUseId error: ${e}`);
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
    log(`extractAgentToolIds error: ${e}`);
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
  log(`Tool ${toolUseId} ambiguous among ${activeAgents.length} agents`);
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

    // Extract session identifiers from hook input
    const sessionId = (inputData as any).session_id || "unknown";
    const cwd = (inputData as any).cwd || "";
    const pid = parseInt(process.env.CLAUDE_PID || "0", 10) || null;

    // Extract session slug from transcript
    const transcriptPath = (inputData as any).transcript_path;
    if (transcriptPath) {
      const slug = extractSlug(transcriptPath);
      if (slug) {
        (inputData as any).slug = slug;
        log(`Extracted slug: ${slug}`);
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
        log(`Agent ${agentId} started, active list: ${state[sessionId].join(", ")}`);
        // Inject agent transcript path
        if (transcriptPath) {
          (inputData as any).agent_transcript_path = agentTranscriptPath(transcriptPath, sessionId, agentId);
        }
      }
    }

    // SubagentStop: remove agent from active list, extract all tool IDs for fix-up
    if (eventName === "SubagentStop") {
      const agentId = (inputData as any).agent_id;
      if (agentId && cwd) {
        const state = readAgentState(cwd);
        if (state[sessionId]) {
          state[sessionId] = state[sessionId].filter((id: string) => id !== agentId);
          if (state[sessionId].length === 0) delete state[sessionId];
        }
        writeAgentState(cwd, state);
        log(`Agent ${agentId} stopped, active list: ${state[sessionId]?.join(", ") || "(empty)"}`);
        // Extract all tool_use IDs from agent transcript for definitive fix-up
        if (transcriptPath) {
          const atp = agentTranscriptPath(transcriptPath, sessionId, agentId);
          const toolIds = extractAgentToolIds(atp);
          if (toolIds.length > 0) {
            (inputData as any).agent_tool_ids = toolIds;
            log(`Agent ${agentId} had ${toolIds.length} tool calls`);
          }
          (inputData as any).agent_transcript_path = atp;
          // Extract trailing text from agent transcript (agent's final summary)
          try {
            let { text, thinking } = extractTrailingText(atp);
            if (!text && !thinking) {
              const maxRetries = 5;
              const delayMs = 250;
              for (let retry = 0; retry < maxRetries && !text && !thinking; retry++) {
                await new Promise(r => setTimeout(r, delayMs));
                ({ text, thinking } = extractTrailingText(atp));
                log(`SubagentStop retry ${retry + 1}: ${text.length} chars text, ${thinking.length} chars thinking`);
              }
            }
            if (text) {
              (inputData as any).agent_stop_text = text;
              log(`Agent ${agentId} trailing text (${text.length} chars)`);
            }
            if (thinking) {
              (inputData as any).agent_stop_thinking = thinking;
              log(`Agent ${agentId} trailing thinking (${thinking.length} chars)`);
            }
          } catch (e) {
            log(`Failed to extract agent trailing content: ${e}`);
          }
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
          log(`Cleaned up agent state for session ${sessionId}`);
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
          const { text, thinking } = extractPrecedingContent(effectiveTranscript, toolUseId);
          if (text) {
            (inputData as any).assistant_text = text;
            log(`Extracted assistant text (${text.length} chars) for ${toolUseId}`);
          }
          if (thinking) {
            (inputData as any).assistant_thinking = thinking;
            log(`Extracted thinking (${thinking.length} chars) for ${toolUseId}`);
          }
        } catch (e) {
          log(`Failed to extract preceding content: ${e}`);
        }
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
          let { text, thinking } = extractFollowingContent(effectiveTranscript, toolUseId);
          if (!text && !thinking) {
            const maxRetries = 3;
            const delayMs = 150;
            for (let retry = 0; retry < maxRetries && !text && !thinking; retry++) {
              await new Promise(r => setTimeout(r, delayMs));
              ({ text, thinking } = extractFollowingContent(effectiveTranscript, toolUseId));
            }
          }
          if (text) {
            (inputData as any).post_tool_text = text;
            log(`Extracted post-tool text (${text.length} chars) for ${toolUseId}`);
          }
          if (thinking) {
            (inputData as any).post_tool_thinking = thinking;
            log(`Extracted post-tool thinking (${thinking.length} chars) for ${toolUseId}`);
          }
        } catch (e) {
          log(`Failed to extract following content: ${e}`);
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
          let { text, thinking } = extractTrailingText(transcriptPath);
          if (!text && !thinking) {
            const maxRetries = 5;
            const delayMs = 250;
            for (let retry = 0; retry < maxRetries && !text && !thinking; retry++) {
              await new Promise(r => setTimeout(r, delayMs));
              ({ text, thinking } = extractTrailingText(transcriptPath));
              log(`Stop retry ${retry + 1}: ${text.length} chars text, ${thinking.length} chars thinking`);
            }
          }
          if (text) {
            (inputData as any).stop_text = text;
            log(`Extracted trailing text (${text.length} chars)`);
          }
          if (thinking) {
            (inputData as any).stop_thinking = thinking;
            log(`Extracted trailing thinking (${thinking.length} chars)`);
          }
        } catch (e) {
          log(`Failed to extract trailing content: ${e}`);
        }
        // Extract cumulative token usage
        try {
          const tokenUsage = extractTokenUsage(transcriptPath);
          (inputData as any).token_usage = tokenUsage;
        } catch (e) {
          log(`Failed to extract token usage: ${e}`);
        }
      }
    }

    // Send to Emacs — PermissionRequest and AskUserQuestionIntercept use bidirectional wait
    if (eventName === "PermissionRequest") {
      const response = await sendToEmacsAndWait(eventName, sessionId, cwd, pid, inputData);
      console.log(JSON.stringify(response));
    } else if (eventName === "AskUserQuestionIntercept") {
      const response = await sendToEmacsAndWait("PreToolUse", sessionId, cwd, pid, inputData);
      if (response && response.hookSpecificOutput) {
        console.log(JSON.stringify(response));
      } else {
        // Fallback: allow terminal execution
        console.log(JSON.stringify({}));
      }
    } else {
      await sendToEmacs(eventName, sessionId, cwd, pid, inputData);
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
  main();
}
