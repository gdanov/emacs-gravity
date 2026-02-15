import { createConnection } from "net";
import { appendFileSync } from "fs";
import { join } from "path";
import Bonjour, { Service, Browser } from "bonjour-service";

type LogLevel = 'debug' | 'info' | 'warn' | 'error';
const LOG_LEVELS: Record<LogLevel, number> = { debug: 0, info: 1, warn: 2, error: 3 };
const CURRENT_LOG_LEVEL: LogLevel = (process.env.EMACS_BRIDGE_LOG_LEVEL as LogLevel) || 'info';

function log(msg: string, level: LogLevel = 'debug') {
  if (LOG_LEVELS[level] < LOG_LEVELS[CURRENT_LOG_LEVEL]) return;
  try {
    const timestamp = new Date().toISOString();
    appendFileSync("/tmp/opencode-bridge.log", `[${timestamp}] [${level.toUpperCase()}] ${msg}\n`);
  } catch (e) {
    // ignore logging errors
  }
}

function getSocketPath(): string {
  const gravitySock = process.env.CLAUDE_GRAVITY_SOCK;
  if (gravitySock) {
    return gravitySock;
  }
  const pluginRoot = process.env.CLAUDE_PLUGIN_ROOT;
  if (pluginRoot) {
    return join(pluginRoot, "..", "claude-gravity.sock");
  }
  return join(__dirname, "..", "..", "claude-gravity.sock");
}

async function sendToEmacs(eventName: string, sessionId: string, cwd: string, payload: any, instancePort?: number, instanceDir?: string) {
  const socketPath = getSocketPath();
  log(`Sending event: ${eventName} session: ${sessionId} to ${socketPath}`);

  return new Promise<void>((resolve) => {
    const client = createConnection(socketPath);

    client.on("connect", () => {
      log("Connected to socket");
      const msg: any = {
        event: eventName,
        session_id: sessionId,
        cwd: cwd,
        source: "opencode",
        data: {
          ...payload,
          instance_port: instancePort,
          instance_dir: instanceDir,
        }
      };
      log(`Full message: ${JSON.stringify(msg)}`, 'info');
      const message = JSON.stringify(msg) + "\n";
      client.write(message);
      client.end();
    });

    client.on("error", (err) => {
      log(`Socket error: ${err.message}`, 'error');
      resolve();
    });

    client.on("close", () => {
      resolve();
    });
  });
}

async function sendBidirectional(eventName: string, sessionId: string, cwd: string, payload: any, instancePort?: number, instanceDir?: string): Promise<any> {
  const socketPath = getSocketPath();
  log(`Sending bidirectional event: ${eventName} session: ${sessionId} to ${socketPath}`);

  return new Promise<any>((resolve) => {
    const client = createConnection(socketPath);
    let responseBuffer = '';
    let resolved = false;

    const finish = (result: any) => {
      if (!resolved) {
        resolved = true;
        resolve(result);
        client.end();
      }
    };

    client.on("connect", () => {
      log("Connected to socket (bidirectional)");
      const msg: any = {
        event: eventName,
        session_id: sessionId,
        cwd: cwd,
        source: "opencode",
        needs_response: true,
        data: {
          ...payload,
          instance_port: instancePort,
          instance_dir: instanceDir,
        }
      };
      log(`Full message (bidirectional): ${JSON.stringify(msg)}`, 'info');
      client.write(JSON.stringify(msg) + "\n");
      // Keep socket open — wait for response from Emacs
    });

    client.on("data", (chunk) => {
      responseBuffer += chunk.toString();
      const newlineIdx = responseBuffer.indexOf('\n');
      if (newlineIdx >= 0) {
        const line = responseBuffer.substring(0, newlineIdx);
        try {
          const response = JSON.parse(line);
          log(`Received bidirectional response: ${JSON.stringify(response)}`, 'info');
          finish(response);
        } catch (e) {
          log(`Failed to parse bidirectional response: ${e}`, 'error');
          finish(null);
        }
      }
    });

    client.on("error", (err) => {
      log(`Socket error (bidirectional): ${err.message}`, 'error');
      finish(null);
    });

    client.on("close", () => {
      finish(null);
    });

    // Timeout: 96 hours (match Claude Code's PermissionRequest timeout)
    setTimeout(() => {
      log(`Bidirectional timeout for ${eventName}`, 'warn');
      finish(null);
    }, 96 * 60 * 60 * 1000);
  });
}

/**
 * Translate Emacs permission response (Claude Code format) to OpenCode format,
 * then POST to OpenCode's permission reply API.
 */
export async function respondToPermission(port: number, directory: string, permissionId: string, emacsResponse: any): Promise<void> {
  const decision = emacsResponse?.hookSpecificOutput?.decision;
  if (!decision) {
    log('No decision in Emacs response for permission', 'warn');
    return;
  }

  const behavior = decision.behavior; // "allow" or "deny"
  const updatedPerms = decision.updatedPermissions;

  // Map to OpenCode format: "once" (allow), "always" (allow+remember), "reject" (deny)
  let ocResponse: string;
  if (behavior === 'allow') {
    ocResponse = updatedPerms ? 'always' : 'once';
  } else {
    ocResponse = 'reject';
  }

  try {
    const url = `http://localhost:${port}/permission/${encodeURIComponent(permissionId)}/reply?directory=${encodeURIComponent(directory)}`;
    log(`Posting permission reply: ${ocResponse} to ${url}`, 'info');
    const response = await fetch(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(ocResponse),
    });
    if (!response.ok) {
      log(`Permission reply failed: ${response.status} ${response.statusText}`, 'error');
    } else {
      log(`Permission ${permissionId} → ${ocResponse}`, 'info');
    }
  } catch (e) {
    log(`Failed to post permission reply: ${e}`, 'error');
  }
}

/**
 * Translate Emacs question response to OpenCode format,
 * then POST to OpenCode's question reply/reject API.
 */
export async function respondToQuestion(port: number, directory: string, questionId: string, emacsResponse: any): Promise<void> {
  // Check if it's a reject (no answer provided)
  const answer = emacsResponse?.answer;
  const hookOutput = emacsResponse?.hookSpecificOutput;

  if (!answer && hookOutput) {
    // No structured answer — might be a reject
    try {
      const url = `http://localhost:${port}/question/${encodeURIComponent(questionId)}/reject?directory=${encodeURIComponent(directory)}`;
      log(`Posting question reject to ${url}`, 'info');
      const response = await fetch(url, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
      });
      if (!response.ok) {
        log(`Question reject failed: ${response.status} ${response.statusText}`, 'error');
      }
    } catch (e) {
      log(`Failed to post question reject: ${e}`, 'error');
    }
    return;
  }

  if (answer) {
    try {
      const url = `http://localhost:${port}/question/${encodeURIComponent(questionId)}/reply?directory=${encodeURIComponent(directory)}`;
      log(`Posting question reply: ${answer} to ${url}`, 'info');
      const response = await fetch(url, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ answers: [answer] }),
      });
      if (!response.ok) {
        log(`Question reply failed: ${response.status} ${response.statusText}`, 'error');
      } else {
        log(`Question ${questionId} → ${answer}`, 'info');
      }
    } catch (e) {
      log(`Failed to post question reply: ${e}`, 'error');
    }
  }
}

interface OpenCodeInstance {
  port: number;
  directory: string;
  sessions: Map<string, any>;
  abortController: AbortController | null;
}

const instances = new Map<number, OpenCodeInstance>();
let bonjour: Bonjour | null = null;
let browser: Browser | null = null;
const messageRoles = new Map<string, string>();

async function fetchJson(url: string): Promise<any> {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}: ${response.statusText}`);
  }
  return response.json();
}

async function getSessionList(port: number, directory: string): Promise<any[]> {
  try {
    const url = `http://localhost:${port}/session?directory=${encodeURIComponent(directory)}`;
    return await fetchJson(url);
  } catch (e) {
    log(`Failed to fetch sessions from port ${port}: ${e}`, 'warn');
    return [];
  }
}

async function getSessionStatus(port: number, directory: string): Promise<Record<string, any>> {
  try {
    const url = `http://localhost:${port}/session/status?directory=${encodeURIComponent(directory)}`;
    return await fetchJson(url);
  } catch (e) {
    log(`Failed to fetch session status from port ${port}: ${e}`, 'warn');
    return {};
  }
}

async function getVcsInfo(port: number, directory: string): Promise<{ branch?: string }> {
  try {
    const url = `http://localhost:${port}/vcs?directory=${encodeURIComponent(directory)}`;
    return await fetchJson(url);
  } catch (e) {
    log(`Failed to fetch VCS info from port ${port}: ${e}`, 'warn');
    return {};
  }
}

export function mapOpenCodeEventToGravity(event: any, instancePort: number, instanceDir: string): { event: string; sessionId: string; cwd: string; data: any } | null {
  const type = event.type;
  if (!type) return null;
  const props = event.properties || {};

  switch (type) {
    case 'session.created': {
      const session = props.info;
      if (!session) return null;
      return {
        event: 'SessionStart',
        sessionId: session.id,
        cwd: session.directory,
        data: {
          slug: session.slug,
          title: session.title,
          parent_id: session.parentID,
          project_id: session.projectID,
          time_created: session.time?.created,
          permission: session.permission,
        },
      };
    }

    case 'session.deleted': {
      const sessionId = props.sessionID;
      return {
        event: 'SessionEnd',
        sessionId,
        cwd: instanceDir,
        data: {},
      };
    }

    case 'session.status': {
      const sessionId = props.sessionID;
      const status = props.status;
      return {
        event: 'SessionStatus',
        sessionId,
        cwd: instanceDir,
        data: {
          status: status.type,
          attempt: status.attempt,
          message: status.message,
        },
      };
    }

    case 'session.idle': {
      const sessionId = props.sessionID;
      return {
        event: 'SessionIdle',
        sessionId,
        cwd: instanceDir,
        data: {},
      };
    }

    case 'message.updated': {
      const message = props.info;
      if (!message) return null;

      const sessionId = message.sessionID;
      const messageId = message.id;
      if (message.role) {
        messageRoles.set(messageId, message.role);
      }
      if (message.role === 'user') {
        return {
          event: 'UserPromptSubmit',
          sessionId,
          cwd: instanceDir,
          data: {
            message_id: messageId,
            agent: message.agent,
            model: message.model,
            tools: message.tools,
            system: message.system,
          },
        };
      } else if (message.role === 'assistant') {
        return {
          event: 'AssistantMessage',
          sessionId,
          cwd: instanceDir,
          data: {
            message_id: messageId,
            parent_id: message.parentID,
            model_id: message.modelID,
            provider_id: message.providerID,
            cost: message.cost,
            tokens: message.tokens,
            finish: message.finish,
            error: message.error,
          },
        };
      }
      return null;
    }

    case 'message.part.updated': {
      const part = props.part;
      if (!part) return null;

      const sessionId = part.sessionID;
      const messageId = part.messageID;
      const messageRole = messageRoles.get(messageId);
      return {
        event: 'MessagePart',
        sessionId,
        cwd: instanceDir,
        data: {
          message_id: messageId,
          message_role: messageRole,
          part_id: part.id,
          part_type: part.type,
          text: part.text,
          tool: part.type === 'tool' ? {
            call_id: part.callID,
            tool_name: part.tool,
            state: part.state,
          } : undefined,
          delta: props.delta,
        },
      };
    }

    case 'permission.asked': {
      return {
        event: 'PermissionRequest',
        sessionId: props.sessionID,
        cwd: instanceDir,
        data: {
          permission_id: props.id,
          // Flattened fields for Emacs dispatch compatibility (expects tool_name/tool_input at root)
          tool_name: props.tool?.name,
          tool_input: props.tool?.input,
          permission: props.permission,
          patterns: props.patterns,
          metadata: props.metadata,
          always: props.always,
          tool: props.tool,
        },
      };
    }

    case 'question.asked': {
      return {
        event: 'AskUserQuestion',
        sessionId: props.sessionID,
        cwd: instanceDir,
        data: {
          question_id: props.id,
          // Flattened fields for Emacs dispatch compatibility
          tool_name: 'AskUserQuestion',
          tool_input: { questions: props.questions },
          questions: props.questions,
          tool: props.tool,
        },
      };
    }

    case 'vcs.branch.updated': {
      const sessionId = props.sessionID;
      return {
        event: 'VcsBranchUpdate',
        sessionId: sessionId || 'unknown',
        cwd: instanceDir,
        data: {
          branch: props.branch,
        },
      };
    }

    case 'session.updated': {
      const session = props.info;
      if (!session) return null;
      return {
        event: 'SessionUpdate',
        sessionId: session.id,
        cwd: instanceDir,
        data: {
          title: session.title,
          slug: session.slug,
          summary: session.summary,
        },
      };
    }

    default:
      log(`Unhandled event type: ${type}`, 'debug');
      return null;
  }
}

async function subscribeToEvents(instance: OpenCodeInstance) {
  const { port, directory } = instance;

  try {
    const url = `http://localhost:${port}/global/event?directory=${encodeURIComponent(directory)}`;
    log(`Subscribing to events from port ${port}`);

    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Failed to connect: ${response.status}`);
    }

    if (!response.body) {
      throw new Error("No response body");
    }

    const reader = response.body.getReader();
    const decoder = new TextDecoder();
    let buffer = "";

    instance.abortController = new AbortController();
    const signal = instance.abortController.signal;

    signal.addEventListener("abort", () => {
      reader.cancel();
    });

    while (true) {
      if (signal.aborted) break;

      const { done, value } = await reader.read();
      if (done) break;

      buffer += decoder.decode(value, { stream: true });
      const lines = buffer.split('\n');
      buffer = lines.pop() || "";

      for (const line of lines) {
        if (!line.trim() || !line.startsWith('data: ')) continue;

        try {
          const data = line.slice(6);
          if (data === '') continue;

          const wrapper = JSON.parse(data);
          // SSE events come wrapped in "payload" object
          const event = wrapper.payload || wrapper;
          log(`Received event: ${event.type} for port ${port}`, 'info');

          const mapped = mapOpenCodeEventToGravity(event, port, directory);
          if (mapped) {
            if (mapped.event === 'PermissionRequest' || mapped.event === 'AskUserQuestion') {
              // Bidirectional: send to Emacs, wait for response, post back to OpenCode
              log(`Sending bidirectional event: ${mapped.event} for session: ${mapped.sessionId}`, 'info');
              const emacsResponse = await sendBidirectional(
                mapped.event, mapped.sessionId, mapped.cwd, mapped.data, port, directory
              );
              if (emacsResponse) {
                if (mapped.event === 'PermissionRequest') {
                  await respondToPermission(port, directory, mapped.data.permission_id, emacsResponse);
                } else {
                  await respondToQuestion(port, directory, mapped.data.question_id, emacsResponse);
                }
              } else {
                log(`No response from Emacs for ${mapped.event}`, 'warn');
              }
            } else {
              // Fire-and-forget
              log(`Sending event: ${mapped.event} for session: ${mapped.sessionId}`, 'info');
              await sendToEmacs(mapped.event, mapped.sessionId, mapped.cwd, mapped.data, port, directory);
            }
          } else {
            log(`No mapping for event: ${event.type}`, 'debug');
          }
        } catch (e) {
          log(`Failed to parse event: ${e}`, 'warn');
        }
      }
    }
  } catch (e) {
    if ((e as Error).name !== 'AbortError') {
      log(`Event subscription error for port ${port}: ${e}`, 'error');
    }
  }

  instances.delete(port);
  log(`Disconnected from port ${port}`, 'info');
}

async function pollSessions(instance: OpenCodeInstance) {
  const { port, directory } = instance;

  try {
    const sessions = await getSessionList(port, directory);
    const statusMap = await getSessionStatus(port, directory);
    const vcs = await getVcsInfo(port, directory);

    for (const session of sessions) {
      const status = statusMap[session.id];
      await sendToEmacs('SessionStatus', session.id, directory, {
        status: status?.type || 'idle',
        title: session.title,
        slug: session.slug,
        branch: vcs.branch,
      }, port, directory);
    }

    if (vcs.branch) {
      const mainSession = sessions[0];
      if (mainSession) {
        await sendToEmacs('VcsBranchUpdate', mainSession.id, directory, { branch: vcs.branch }, port, directory);
      }
    }
  } catch (e) {
    log(`Polling error for port ${port}: ${e}`, 'warn');
  }
}

async function startInstance(port: number, directory: string) {
  if (instances.has(port)) {
    log(`Already connected to port ${port}`, 'debug');
    return;
  }

  log(`Connecting to OpenCode instance on port ${port}, directory: ${directory}`, 'info');

  const instance: OpenCodeInstance = {
    port,
    directory,
    sessions: new Map(),
    abortController: null,
  };

  instances.set(port, instance);

  subscribeToEvents(instance);

  setInterval(async () => {
    await pollSessions(instance);
  }, 5000);

  await pollSessions(instance);
}

function startDiscovery() {
  log("Starting mDNS discovery for OpenCode instances", 'info');

  bonjour = new Bonjour();

  browser = bonjour.find({ type: 'http' });

  browser.on('up', (service: Service) => {
    const name = service.name;
    if (!name.startsWith('opencode-')) {
      return;
    }

    const port = service.port;
    let directory = '/';

    if (service.txt && service.txt.path) {
      directory = service.txt.path;
    }

    log(`Discovered OpenCode instance: ${name} on port ${port}`, 'info');
    startInstance(port, directory);
  });

  browser.on('down', (service: Service) => {
    const port = service.port;
    const instance = instances.get(port);
    if (instance && instance.abortController) {
      instance.abortController.abort();
    }
    instances.delete(port);
    log(`OpenCode instance removed: port ${port}`, 'info');
  });
}

async function main() {
  log("Starting OpenCode bridge", 'info');
  log(`Socket path: ${getSocketPath()}`, 'info');

  startDiscovery();

  setInterval(() => {
    log(`Active instances: ${instances.size}`, 'debug');
  }, 30000);
}

main().catch((e) => {
  log(`Fatal error: ${e}`, 'error');
  process.exit(1);
});
