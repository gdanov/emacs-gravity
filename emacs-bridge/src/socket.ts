import { createConnection } from "net";
import { existsSync } from "fs";
import { join } from "path";
import { log } from "./log.js";

// Resolve socket path from CLAUDE_GRAVITY_SOCK, CLAUDE_GRAVITY_SOCK_DIR, or default location
export function getSocketPath(): string {
  const gravitySock = process.env.CLAUDE_GRAVITY_SOCK;
  if (gravitySock) {
    return gravitySock;
  }
  const sockDir = process.env.CLAUDE_GRAVITY_SOCK_DIR;
  if (sockDir) {
    return join(sockDir, "claude-gravity.sock");
  }
  const home = process.env.HOME || "/tmp";
  return join(home, ".local", "state", "claude-gravity.sock");
}

// Helper to send data to Emacs socket
export async function sendToEmacs(eventName: string, sessionId: string, cwd: string, pid: number | null, payload: any, hookInput?: any) {
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
      const flushed = client.write(message);
      if (flushed) {
        client.end();
      } else {
        client.once("drain", () => client.end());
      }
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
export async function sendToEmacsAndWait(eventName: string, sessionId: string, cwd: string, pid: number | null, payload: any, timeoutMs: number = 345600000, hookInput?: any): Promise<any> {
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
            log(`Received response: ${JSON.stringify(response).substring(0, 200)}`, 'warn');
            resolve(response);
          } catch (e) {
            log(`Failed to parse response: ${e}`, 'error');
            resolve({});
          }
          client.destroy();
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
