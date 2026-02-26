// ON HOLD (2026-02): Agent SDK requires pay-per-use API key.
// Using Max/Pro subscription is against Anthropic TOS. See ARCHITECTURE.md.
//
// Main daemon entry point.
// Long-running Node.js process that manages SDK sessions.
// Listens on a command socket for Emacs commands, forwards events to Emacs via the gravity socket.

import { createServer, createConnection, Server, Socket } from "net";
import { unlinkSync, existsSync, readFileSync } from "fs";
import { join, dirname } from "path";
import { homedir } from "os";
import { fileURLToPath } from "url";
import { log, setLogLevel, setLogFile } from "./log.js";
import { DaemonSession, DaemonSessionOptions, SendEventFn, SendAndWaitFn } from "./daemon-session.js";
import { createDaemonHooks, createCanUseTool, clearAgents } from "./daemon-hooks.js";
import { PiSession } from "./pi-session.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// ============================================================================
// Authentication — API key resolution
// ============================================================================
// Priority: 1) ANTHROPIC_API_KEY env var (set by Emacs override)
//           2) Config file: ~/.claude/gravity-config.json { "anthropic_api_key": "..." }
//           3) Not set → sessions will fail with auth error

function loadApiKey(): string | undefined {
  // 1. Environment variable (Emacs override or shell profile)
  if (process.env.ANTHROPIC_API_KEY) {
    return process.env.ANTHROPIC_API_KEY;
  }
  // 2. Config file
  const configPath = join(homedir(), ".claude", "gravity-config.json");
  if (existsSync(configPath)) {
    try {
      const config = JSON.parse(readFileSync(configPath, "utf-8"));
      if (config.anthropic_api_key) {
        return config.anthropic_api_key;
      }
    } catch (e: any) {
      log(`[daemon] Failed to read config ${configPath}: ${e.message}`, "error");
    }
  }
  return undefined;
}

function loadMinimaxApiKey(): string | undefined {
  // 1. Environment variable
  if (process.env.MINIMAX_API_KEY) {
    return process.env.MINIMAX_API_KEY;
  }
  // 2. Config file
  const configPath = join(homedir(), ".claude", "gravity-config.json");
  if (existsSync(configPath)) {
    try {
      const config = JSON.parse(readFileSync(configPath, "utf-8"));
      if (config.minimax_api_key) {
        return config.minimax_api_key;
      }
    } catch (e: any) {
      log(`[daemon] Failed to read config ${configPath}: ${e.message}`, "error");
    }
  }
  return undefined;
}

// ============================================================================
// Socket path resolution
// ============================================================================

function getGravitySocketPath(): string {
  // Use CLAUDE_GRAVITY_SOCK env var if set, otherwise default to ~/.local/state
  const envSock = process.env.CLAUDE_GRAVITY_SOCK;
  if (envSock) return envSock;
  const sockDir = process.env.CLAUDE_GRAVITY_SOCK_DIR || join(homedir(), ".local", "state");
  return join(sockDir, "claude-gravity.sock");
}

function getCommandSocketPath(): string {
  // Command socket is at emacs-gravity/claude-gravity-daemon.sock
  return join(__dirname, "..", "..", "claude-gravity-daemon.sock");
}

// ============================================================================
// Event forwarding — send events to Emacs via gravity socket
// ============================================================================

const sendEvent: SendEventFn = async (eventName, sessionId, cwd, pid, payload) => {
  const socketPath = getGravitySocketPath();
  log(`[daemon] sendEvent: ${eventName} session=${sessionId}`, 'debug');

  // Check if this is a PiSession to add source:pi
  const session = findSession(sessionId);
  const isPiSession = session && "sendPrompt" in session;
  const source = isPiSession ? "pi" : undefined;

  return new Promise<void>((resolve) => {
    const client = createConnection(socketPath);

    client.on("connect", () => {
      const msg = source 
        ? { event: eventName, session_id: sessionId, cwd, pid, source, data: payload }
        : { event: eventName, session_id: sessionId, cwd, pid, data: payload };
      client.write(JSON.stringify(msg) + "\n");
      client.end();
    });

    client.on("error", (err) => {
      log(`[daemon] sendEvent socket error: ${err.message}`, 'error');
      resolve();
    });

    client.on("close", () => resolve());
  });
};

const sendAndWait: SendAndWaitFn = async (eventName, sessionId, cwd, pid, payload) => {
  const socketPath = getGravitySocketPath();
  log(`[daemon] sendAndWait: ${eventName} session=${sessionId}`, 'debug');

  // Check if this is a PiSession to add source:pi
  const session = findSession(sessionId);
  const isPiSession = session && "sendPrompt" in session;
  const source = isPiSession ? "pi" : undefined;

  return new Promise<any>((resolve) => {
    const client = createConnection(socketPath);
    let responded = false;
    let buffer = "";

    // 96 hour timeout (same as one-shot bridge)
    const timer = setTimeout(() => {
      if (!responded) {
        responded = true;
        log(`[daemon] sendAndWait timeout for ${eventName}`, 'warn');
        client.destroy();
        resolve({});
      }
    }, 345600000);

    client.on("connect", () => {
      const msg = source
        ? { event: eventName, session_id: sessionId, cwd, pid, source, needs_response: true, data: payload }
        : { event: eventName, session_id: sessionId, cwd, pid, needs_response: true, data: payload };
      client.write(JSON.stringify(msg) + "\n");
      // Keep connection open for response
    });

    client.on("data", (chunk) => {
      buffer += chunk.toString();
      const newlineIdx = buffer.indexOf("\n");
      if (newlineIdx >= 0 && !responded) {
        responded = true;
        clearTimeout(timer);
        const line = buffer.substring(0, newlineIdx);
        try {
          resolve(JSON.parse(line));
        } catch (e) {
          log(`[daemon] sendAndWait parse error: ${e}`, 'error');
          resolve({});
        }
        client.end();
      }
    });

    client.on("error", (err) => {
      if (!responded) {
        responded = true;
        clearTimeout(timer);
        log(`[daemon] sendAndWait socket error: ${err.message}`, 'error');
        resolve({});
      }
    });

    client.on("close", () => {
      if (!responded) {
        responded = true;
        clearTimeout(timer);
        resolve({});
      }
    });
  });
};

// ============================================================================
// Session management
// ============================================================================

const sessions: Map<string, AnySession> = new Map();

// Map real session IDs back to session (populated after system/init)
const sessionByRealId: Map<string, DaemonSession | any> = new Map();

type AnySession = DaemonSession | any;

function findSession(id: string): AnySession | undefined {
  return sessions.get(id) || sessionByRealId.get(id);
}

function startSession(cmd: { id: string; cwd: string; model?: string; permission_mode?: string; resume?: string }): { ok: boolean; session_id?: string; error?: string } {
  if (sessions.has(cmd.id)) {
    return { ok: false, error: `Session ${cmd.id} already exists` };
  }

  const getSessionId = () => {
    const s = sessions.get(cmd.id);
    return s ? s.sessionId : cmd.id;
  };
  const getCwd = () => cmd.cwd;

  const hooks = createDaemonHooks(sendEvent, sendAndWait, getSessionId, getCwd);
  const canUseTool = createCanUseTool(sendAndWait, getSessionId, getCwd);

  const opts: DaemonSessionOptions = {
    id: cmd.id,
    cwd: cmd.cwd,
    model: cmd.model,
    permissionMode: cmd.permission_mode,
    resume: cmd.resume,
    sendEvent,
    sendAndWait,
    hooks,
    canUseTool,
    onSessionId: (realId: string) => {
      log(`[daemon] Session re-keyed: ${cmd.id} → ${realId}`, 'info');
      sessionByRealId.set(realId, session);
    },
    onDone: (sessionId: string) => {
      log(`[daemon] Session done: ${sessionId}`, 'info');
      clearAgents(sessionId);
      // Keep session in map for status queries; Emacs can send "stop" to clean up
    },
  };

  const session = new DaemonSession(opts);
  sessions.set(cmd.id, session);

  // Start session async (don't await — it runs until completion)
  session.start(opts).catch((e: any) => {
    log(`[daemon] Session start error: ${e.message}`, 'error');
  });

  return { ok: true, session_id: cmd.id };
}

async function startPiSession(cmd: { id: string; cwd: string; model?: string; permission_mode?: string; resume?: string }): Promise<{ ok: boolean; session_id?: string; error?: string }> {
  if (sessions.has(cmd.id)) {
    return { ok: false, error: `Session ${cmd.id} already exists` };
  }

  const getSessionId = () => {
    const s = sessions.get(cmd.id);
    return s ? s.sessionId : cmd.id;
  };
  const getCwd = () => cmd.cwd;

  const opts: any = {
    id: cmd.id,
    cwd: cmd.cwd,
    model: cmd.model,
    permissionMode: cmd.permission_mode,
    resume: cmd.resume,
    sendEvent,
    sendAndWait,
    onSessionId: (realId: string) => {
      log(`[daemon] Pi Session re-keyed: ${cmd.id} → ${realId}`, 'info');
      sessionByRealId.set(realId, sessions.get(cmd.id)!);
    },
    onDone: (sessionId: string) => {
      log(`[daemon] Pi Session done: ${sessionId}`, 'info');
    },
  };

  const session = new PiSession(opts);
  sessions.set(cmd.id, session);

  session.start(opts).catch((e: any) => {
    log(`[daemon] Pi Session start error: ${e.message}`, 'error');
  });

  return { ok: true, session_id: cmd.id };
}

function removeSession(id: string): void {
  const session = findSession(id);
  if (session) {
    sessions.delete(session.tempId);
    sessionByRealId.delete(session.sessionId);
    clearAgents(session.sessionId);
  }
}

// ============================================================================
// Command dispatch
// ============================================================================

async function handleCommand(cmd: any): Promise<any> {
  log(`[daemon] Command: ${JSON.stringify(cmd)}`, 'info');

  switch (cmd.cmd) {
    case "start": {
      const bridge = cmd.bridge ?? "sdk";
      if (bridge === "pi") {
        return await startPiSession(cmd);
      }
      return startSession(cmd);
    }

    case "resume": {
      const bridge = cmd.bridge ?? "sdk";
      if (bridge === "pi") {
        return await startPiSession({ ...cmd, resume: cmd.session_id });
      }
      return startSession({ ...cmd, resume: cmd.session_id });
    }

    case "prompt": {
      const session = findSession(cmd.session_id);
      if (!session) return { ok: false, error: `Session ${cmd.session_id} not found` };
      if (!session.isRunning) return { ok: false, error: `Session ${cmd.session_id} not running` };
      session.sendPrompt(cmd.text);
      return { ok: true };
    }

    case "interrupt": {
      const session = findSession(cmd.session_id);
      if (!session) return { ok: false, error: `Session ${cmd.session_id} not found` };
      session.interrupt();
      return { ok: true };
    }

    case "stop": {
      const session = findSession(cmd.session_id);
      if (!session) return { ok: false, error: `Session ${cmd.session_id} not found` };
      session.stop();
      removeSession(cmd.session_id);
      return { ok: true };
    }

    case "set_model": {
      const session = findSession(cmd.session_id);
      if (!session) return { ok: false, error: `Session ${cmd.session_id} not found` };
      session.setModel(cmd.model);
      return { ok: true };
    }

    case "set_permission_mode": {
      const session = findSession(cmd.session_id);
      if (!session) return { ok: false, error: `Session ${cmd.session_id} not found` };
      session.setPermissionMode(cmd.mode);
      return { ok: true };
    }

    case "status": {
      const data: any[] = [];
      for (const [id, session] of sessions) {
        data.push({
          temp_id: id,
          session_id: session.sessionId,
          running: session.isRunning,
        });
      }
      return { ok: true, data };
    }

    case "shutdown": {
      log("[daemon] Shutdown requested", 'info');
      // Stop all sessions
      for (const [id, session] of sessions) {
        session.stop();
        clearAgents(session.sessionId);
      }
      sessions.clear();
      sessionByRealId.clear();
      // Schedule shutdown after response is sent
      setTimeout(() => shutdown(), 100);
      return { ok: true };
    }

    default:
      return { ok: false, error: `Unknown command: ${cmd.cmd}` };
  }
}

// ============================================================================
// Command socket server
// ============================================================================

let commandServer: Server | null = null;

function startCommandServer(): void {
  const socketPath = getCommandSocketPath();

  // Clean up stale socket
  if (existsSync(socketPath)) {
    try { unlinkSync(socketPath); } catch {}
  }

  commandServer = createServer(async (client: Socket) => {
    let buffer = "";

    client.on("data", async (chunk) => {
      buffer += chunk.toString();
      // Process all complete lines (newline-delimited JSON)
      let newlineIdx: number;
      while ((newlineIdx = buffer.indexOf("\n")) >= 0) {
        const line = buffer.substring(0, newlineIdx).trim();
        buffer = buffer.substring(newlineIdx + 1);
        if (!line) continue;

        try {
          const cmd = JSON.parse(line);
          const response = await handleCommand(cmd);
          client.write(JSON.stringify(response) + "\n");
        } catch (e: any) {
          log(`[daemon] Command parse error: ${e.message}`, 'error');
          client.write(JSON.stringify({ ok: false, error: `Parse error: ${e.message}` }) + "\n");
        }
      }
    });

    client.on("error", (err) => {
      log(`[daemon] Client error: ${err.message}`, 'warn');
    });
  });

  commandServer.on("error", (err) => {
    log(`[daemon] Server error: ${err.message}`, 'error');
    process.exit(1);
  });

  commandServer.listen(socketPath, () => {
    log(`[daemon] Listening on ${socketPath}`, 'info');
    // Signal readiness to parent process (Emacs)
    process.stdout.write(`READY ${socketPath}\n`);
  });
}

// ============================================================================
// Shutdown
// ============================================================================

function shutdown(): void {
  log("[daemon] Shutting down", 'info');

  if (commandServer) {
    commandServer.close();
    commandServer = null;
  }

  const socketPath = getCommandSocketPath();
  if (existsSync(socketPath)) {
    try { unlinkSync(socketPath); } catch {}
  }

  process.exit(0);
}

// Handle signals
process.on("SIGTERM", shutdown);
process.on("SIGINT", shutdown);
process.on("SIGHUP", shutdown);

// ============================================================================
// Entry point
// ============================================================================

function main(): void {
  setLogFile("/tmp/emacs-bridge-daemon.log");
  setLogLevel((process.env.EMACS_BRIDGE_LOG_LEVEL as any) || "info");

  log("[daemon] Starting emacs-gravity daemon", 'info');
  log(`[daemon] PID: ${process.pid}`, 'info');
  log(`[daemon] Gravity socket: ${getGravitySocketPath()}`, 'info');
  log(`[daemon] Command socket: ${getCommandSocketPath()}`, 'info');

  // Resolve API key and inject into process.env so daemon-session picks it up
  const hadEnvKey = !!process.env.ANTHROPIC_API_KEY;
  const apiKey = loadApiKey();
  if (apiKey) {
    process.env.ANTHROPIC_API_KEY = apiKey;
    const source = hadEnvKey ? "env/emacs" : "config file";
    log(`[daemon] ANTHROPIC_API_KEY: set (${apiKey.length} chars, source: ${source})`, 'info');
  } else {
    log("[daemon] WARNING: ANTHROPIC_API_KEY not set — SDK sessions will fail to authenticate", 'error');
    log("[daemon] Set ANTHROPIC_API_KEY env var or create ~/.claude/gravity-config.json with {\"anthropic_api_key\": \"sk-ant-...\"}", 'error');
  }

  // Resolve MiniMax API key
  const hadMinimaxEnvKey = !!process.env.MINIMAX_API_KEY;
  const minimaxApiKey = loadMinimaxApiKey();
  if (minimaxApiKey) {
    process.env.MINIMAX_API_KEY = minimaxApiKey;
    const source = hadMinimaxEnvKey ? "env/emacs" : "config file";
    log(`[daemon] MINIMAX_API_KEY: set (${minimaxApiKey.length} chars, source: ${source})`, 'info');
  }

  startCommandServer();
}

main();
