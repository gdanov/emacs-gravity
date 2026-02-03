import { createConnection } from "net";
import { readFileSync, appendFileSync, statSync, openSync, readSync, closeSync } from "fs";
import { join } from "path";

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

// Read the tail of a file (last maxBytes), skipping any partial first line
function readTail(filePath: string, maxBytes: number): string {
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

// Extract assistant text and thinking that precede a tool_use with the given ID.
// Returns { text, thinking } where either may be empty.
function extractPrecedingContent(transcriptPath: string, toolUseId: string): { text: string; thinking: string } {
  const result = { text: "", thinking: "" };
  try {
    const content = readTail(transcriptPath, 2 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);

    // Find the tool_use line matching toolUseId, searching from end
    let toolIdx = -1;
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        if (c[0].type === "tool_use" && c[0].id === toolUseId) {
          toolIdx = i;
          break;
        }
      } catch {
        continue;
      }
    }
    if (toolIdx < 0) return result;

    // Walk backwards from toolIdx collecting text and thinking blocks
    for (let i = toolIdx - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        const blockType = c[0].type;
        if (blockType === "tool_use") continue; // parallel sibling, skip
        if (blockType === "text" && !result.text) {
          const text = c[0].text || "";
          if (text && text !== "(no content)") {
            result.text = text;
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
    return result;
  } catch (e) {
    log(`extractPrecedingContent error: ${e}`);
    return result;
  }
}

// Extract trailing assistant text/thinking after the last tool interaction.
// Called on Stop events to capture the conclusion text.
function extractTrailingText(transcriptPath: string): { text: string; thinking: string } {
  const result = { text: "", thinking: "" };
  try {
    const content = readTail(transcriptPath, 2 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);

    const textParts: string[] = [];
    // Walk backwards from end, collecting trailing assistant text/thinking
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        // Skip non-message entries (progress, system, summary, etc.)
        if (obj.type !== "assistant" && obj.type !== "user") continue;
        if (obj.type === "user") break; // hit user/tool_result — stop
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
          break; // thinking is always first
        }
        // tool_use or other — we've gone past the trailing text
        break;
      } catch {
        continue;
      }
    }
    result.text = textParts.join("\n\n");
    return result;
  } catch (e) {
    log(`extractTrailingText error: ${e}`);
    return result;
  }
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

    // Enrich PreToolUse with assistant monologue text and thinking from transcript
    if (eventName === "PreToolUse") {
      const transcriptPath = (inputData as any).transcript_path;
      const toolUseId = (inputData as any).tool_use_id;
      if (transcriptPath && toolUseId) {
        try {
          const { text, thinking } = extractPrecedingContent(transcriptPath, toolUseId);
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

    // Enrich Stop with trailing assistant text from transcript
    if (eventName === "Stop") {
      const transcriptPath = (inputData as any).transcript_path;
      if (transcriptPath) {
        try {
          const { text, thinking } = extractTrailingText(transcriptPath);
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
      }
    }

    // Send to Emacs
    await sendToEmacs(eventName, sessionId, cwd, pid, inputData);

    // Always output valid JSON to stdout as expected by Claude Code
    console.log(JSON.stringify({}));
  } catch (error) {
    // Log error to stderr but output valid JSON to stdout to not break Claude
    // console.error("Emacs Bridge Logic Error:", error);
    console.log(JSON.stringify({}));
  }
}

main();
