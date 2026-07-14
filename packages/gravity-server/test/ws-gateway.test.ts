// ws-gateway.test.ts — Tests for the browser-facing HTTP + WS gateway.
//
// Pattern follows `mermaid-rpc-server.test.ts`: build a minimal real
// counterpart (a bare `net.createServer` here, since the gateway speaks
// NDJSON over a Unix socket) and a real `ws` client — never a mocked
// transport. The HTTP/WS listener is always bound to an OS-assigned
// ephemeral port (port 0) and the real port is read back through
// `WsGateway.port`, mirroring the upstream test-harness pattern.

import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { createServer as createNetServer, type Server as NetServer, type Socket as NetSocket } from "net";
import { mkdtempSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import WebSocket from "ws";

import { WsGateway, type WsGatewayOptions } from "../src/gateway/ws-gateway.js";

const REJECTION_TIMEOUT_MS = 2000;
const OPEN_TIMEOUT_MS = 5000;
const COLLECT_TIMEOUT_MS = 3000;

interface FakeTerminal {
  server: NetServer;
  socketPath: string;
  connections: NetSocket[];
}

function makeFakeTerminal(): Promise<FakeTerminal> {
  return new Promise((resolve, reject) => {
    const dir = mkdtempSync(join(tmpdir(), "gravity-ws-gw-term-"));
    const socketPath = join(dir, "terminal.sock");
    const connections: NetSocket[] = [];
    const server = createNetServer((socket) => {
      connections.push(socket);
    });
    server.on("error", reject);
    server.listen(socketPath, () => resolve({ server, socketPath, connections }));
  });
}

function closeFakeTerminal(fake: FakeTerminal): Promise<void> {
  return new Promise((resolve) => {
    for (const c of fake.connections) {
      try { c.destroy(); } catch { /* ignore */ }
    }
    fake.server.close(() => {
      try { rmSync(dirnameOf(fake.socketPath), { recursive: true, force: true }); } catch { /* ignore */ }
      resolve();
    });
  });
}

function dirnameOf(p: string): string {
  const idx = p.lastIndexOf("/");
  return idx === -1 ? "." : p.substring(0, idx);
}

async function startGateway(token: string, terminalSocketPath: string): Promise<WsGateway> {
  const opts: WsGatewayOptions = {
    host: "127.0.0.1",
    port: 0,
    terminalSocketPath,
    token,
    clientHtml: "<!doctype html><html><body>placeholder</body></html>",
  };
  const gateway = new WsGateway(opts);
  await gateway.start();
  return gateway;
}

interface WsHandle {
  ws: WebSocket;
  nextMessage: (timeoutMs?: number) => Promise<string>;
  awaitClose: (timeoutMs?: number) => Promise<void>;
}

function openWs(
  port: number,
  token: string | null,
  origin: string | null = `http://127.0.0.1:${port}`,
  path: string = "/ws",
): Promise<WsHandle> {
  return new Promise((resolve, reject) => {
    const url = new URL(path, `http://127.0.0.1:${port}`);
    if (token !== null) url.searchParams.set("token", token);

    const headers: Record<string, string> = {};
    if (origin !== null) headers["origin"] = origin;
    const wsOptions: { headers?: Record<string, string> } = {};
    if (Object.keys(headers).length > 0) wsOptions.headers = headers;

    const ws = new WebSocket(url.toString(), wsOptions) as WebSocket & {
      on: (event: string, listener: (...args: unknown[]) => void) => WebSocket;
    };

    const messageQueue: string[] = [];
    const pendingResolvers: Array<{ res: (v: string) => void; rej: (e: Error) => void; timer: ReturnType<typeof setTimeout> | null }> = [];
    let closeResolve: (() => void) | null = null;
    let closeReject: ((err: Error) => void) | null = null;
    let openSettled = false;

    const openTimer = setTimeout(() => {
      if (openSettled) return;
      openSettled = true;
      try { ws.terminate(); } catch { /* ignore */ }
      reject(new Error("WS open timeout"));
    }, OPEN_TIMEOUT_MS);

    ws.on("open", () => {
      clearTimeout(openTimer);
      openSettled = true;

      const deliverMessage = (text: string): void => {
        if (pendingResolvers.length > 0) {
          const next = pendingResolvers.shift()!;
          if (next.timer) clearTimeout(next.timer);
          next.res(text);
        } else {
          messageQueue.push(text);
        }
      };

      ws.on("message", (data: unknown) => {
        const text =
          typeof data === "string"
            ? data
            : Buffer.isBuffer(data)
              ? data.toString("utf8")
              : Array.isArray(data)
                ? Buffer.concat(data).toString("utf8")
                : "";
        if (text.length > 0) deliverMessage(text);
      });
      ws.on("close", () => {
        if (closeResolve) {
          const r = closeResolve;
          closeResolve = null;
          closeReject = null;
          r();
        }
        for (const p of pendingResolvers) {
          if (p.timer) clearTimeout(p.timer);
          p.rej(new Error("WS closed before message arrived"));
        }
        pendingResolvers.length = 0;
      });
      ws.on("error", (err: Error) => {
        if (closeReject) {
          const r = closeReject;
          closeResolve = null;
          closeReject = null;
          r(err);
        }
        for (const p of pendingResolvers) {
          if (p.timer) clearTimeout(p.timer);
          p.rej(err);
        }
        pendingResolvers.length = 0;
      });

      resolve({
        ws,
        nextMessage: (timeoutMs: number = COLLECT_TIMEOUT_MS) =>
          new Promise((res, rej) => {
            if (messageQueue.length > 0) {
              res(messageQueue.shift()!);
              return;
            }
            const entry = { res, rej, timer: null as ReturnType<typeof setTimeout> | null };
            entry.timer = setTimeout(() => {
              const idx = pendingResolvers.indexOf(entry);
              if (idx !== -1) {
                pendingResolvers.splice(idx, 1);
                rej(new Error("WS next-message timeout"));
              }
            }, timeoutMs);
            pendingResolvers.push(entry);
          }),
        awaitClose: (timeoutMs: number = COLLECT_TIMEOUT_MS) =>
          new Promise((res, rej) => {
            closeResolve = res;
            closeReject = rej;
            setTimeout(() => {
              if (closeResolve) {
                closeResolve = null;
                closeReject = null;
                rej(new Error("WS await-close timeout"));
              }
            }, timeoutMs);
          }),
      });
    });
    ws.on("unexpected-response", () => {
      clearTimeout(openTimer);
      if (openSettled) return;
      openSettled = true;
      try { ws.terminate(); } catch { /* ignore */ }
      reject(new Error("WS rejected with HTTP response"));
    });
    ws.on("error", (err: Error) => {
      clearTimeout(openTimer);
      if (openSettled) return;
      openSettled = true;
      try { ws.terminate(); } catch { /* ignore */ }
      reject(err);
    });
  });
}

function expectWsRejection(
  port: number,
  token: string | null,
  origin: string | null = `http://127.0.0.1:${port}`,
  path: string = "/ws",
): Promise<void> {
  return new Promise((resolve) => {
    const url = new URL(path, `http://127.0.0.1:${port}`);
    if (token !== null) url.searchParams.set("token", token);

    const headers: Record<string, string> = {};
    if (origin !== null) headers["origin"] = origin;
    const wsOptions: { headers?: Record<string, string> } = {};
    if (Object.keys(headers).length > 0) wsOptions.headers = headers;

    const ws = new WebSocket(url.toString(), wsOptions) as WebSocket & {
      on: (event: string, listener: (...args: unknown[]) => void) => WebSocket;
    };

    const settle = (): void => {
      try { ws.terminate(); } catch { /* ignore */ }
      resolve();
    };

    const openTimer = setTimeout(settle, REJECTION_TIMEOUT_MS);
    ws.on("open", () => { clearTimeout(openTimer); settle(); });
    ws.on("unexpected-response", () => { clearTimeout(openTimer); settle(); });
    ws.on("error", () => { clearTimeout(openTimer); settle(); });
  });
}

function waitForTerminalConnection(fake: FakeTerminal, timeoutMs: number = 2000): Promise<NetSocket> {
  return new Promise((resolve, reject) => {
    const start = Date.now();
    const tick = (): void => {
      if (fake.connections.length > 0) {
        resolve(fake.connections[0]);
        return;
      }
      if (Date.now() - start > timeoutMs) {
        reject(new Error("Timed out waiting for terminal connection"));
        return;
      }
      setTimeout(tick, 5);
    };
    tick();
  });
}

describe("WsGateway", () => {
  let fake: FakeTerminal;
  let gateway: WsGateway;
  let token: string;
  let port: number;
  const liveSockets: WebSocket[] = [];

  function track(ws: WebSocket): void {
    liveSockets.push(ws);
  }

  function closeAllSockets(): void {
    for (const ws of liveSockets) {
      try { ws.terminate(); } catch { /* ignore */ }
    }
    liveSockets.length = 0;
  }

  beforeEach(async () => {
    fake = await makeFakeTerminal();
    token = "test-token-" + Math.random().toString(36).slice(2);
    gateway = await startGateway(token, fake.socketPath);
    port = gateway.port;
    expect(port).toBeGreaterThan(0);
  });

  afterEach(async () => {
    closeAllSockets();
    gateway.stop();
    await closeFakeTerminal(fake);
  });

  it("accepts a valid handshake (correct origin + correct token) and bridges NDJSON", async () => {
    const handle = await openWs(port, token);
    track(handle.ws);

    // Wait for the gateway to dial the fake terminal — only then is it
    // safe to write data (writes before the connection is open would
    // race with the gateway's own createConnection).
    const term = await waitForTerminalConnection(fake);
    const first = handle.nextMessage();
    term.write('{"hello":"world"}\n');
    expect(await first).toBe('{"hello":"world"}');
  });

  it("rejects an upgrade with a wrong Origin", async () => {
    await expectWsRejection(port, token, "http://evil.example");
  });

  it("rejects an upgrade with a missing Origin header", async () => {
    await expectWsRejection(port, token, null);
  });

  it("rejects an upgrade with a missing token query param", async () => {
    await expectWsRejection(port, null);
  });

  it("rejects an upgrade with a wrong token", async () => {
    await expectWsRejection(port, "nope");
  });

  it("forwards two NDJSON lines in one chunk as two WS messages (split)", async () => {
    const handle = await openWs(port, token);
    track(handle.ws);

    const term = await waitForTerminalConnection(fake);
    const a = handle.nextMessage();
    const b = handle.nextMessage();
    term.write('{"a":1}\n{"b":2}\n');
    expect(await a).toBe('{"a":1}');
    expect(await b).toBe('{"b":2}');
  });

  it("reassembles a single NDJSON line split across two chunks (coalesce)", async () => {
    const handle = await openWs(port, token);
    track(handle.ws);

    const term = await waitForTerminalConnection(fake);
    const one = handle.nextMessage();
    term.write('{"x":');
    setTimeout(() => term.write('42}\n'), 30);
    expect(await one).toBe('{"x":42}');
  });

  it("propagates a terminal-socket close to the WS client (server→client)", async () => {
    const handle = await openWs(port, token);
    track(handle.ws);

    const term = await waitForTerminalConnection(fake);
    const closed = handle.awaitClose();
    term.destroy();
    await closed;
  });

  it("propagates a WS client close to the terminal socket (client→server)", async () => {
    const handle = await openWs(port, token);
    track(handle.ws);

    const term = await waitForTerminalConnection(fake);
    const termClosed = new Promise<void>((resolve) => {
      term.once("close", () => resolve());
    });
    try { handle.ws.close(); } catch { /* ignore */ }
    await termClosed;
  });

  it("writes WS messages to the terminal socket as NDJSON (text + '\\n')", async () => {
    const handle = await openWs(port, token);
    track(handle.ws);

    const term = await waitForTerminalConnection(fake);
    const line = await new Promise<string>((resolve, reject) => {
      const t = setTimeout(() => reject(new Error("terminal read timeout")), 2000);
      let buf = "";
      term.on("data", (chunk: Buffer) => {
        buf += chunk.toString("utf8");
        const idx = buf.indexOf("\n");
        if (idx !== -1) {
          clearTimeout(t);
          resolve(buf.substring(0, idx));
        }
      });
      handle.ws.send('{"from":"client"}');
    });
    expect(line).toBe('{"from":"client"}');
  });
});

describe("WsGateway.port", () => {
  it("resolves the OS-assigned port when constructed with port: 0", async () => {
    const fake = await makeFakeTerminal();
    try {
      const gateway = await startGateway("tok", fake.socketPath);
      try {
        expect(gateway.port).toBeGreaterThan(0);
        expect(Number.isInteger(gateway.port)).toBe(true);
      } finally {
        gateway.stop();
      }
    } finally {
      await closeFakeTerminal(fake);
    }
  });

  it("stop() is idempotent", async () => {
    const fake = await makeFakeTerminal();
    try {
      const gateway = await startGateway("tok", fake.socketPath);
      gateway.stop();
      expect(() => gateway.stop()).not.toThrow();
    } finally {
      await closeFakeTerminal(fake);
    }
  });
});
