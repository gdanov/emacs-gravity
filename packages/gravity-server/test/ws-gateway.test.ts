// ws-gateway.test.ts — Tests for the browser-facing HTTP + WS gateway.
//
// Pattern follows `mermaid-rpc-server.test.ts`: build a minimal real
// counterpart (a bare `net.createServer` here, since the gateway speaks
// NDJSON over a Unix socket) and a real `ws` client — never a mocked
// transport. The HTTP/WS listener is always bound to an OS-assigned
// ephemeral port (port 0) and the real port is read back through
// `WsGateway.port`, mirroring the upstream test-harness pattern.

import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { createServer as createNetServer, createConnection as createNetConnection, type Server as NetServer, type Socket as NetSocket } from "net";
import { mkdtempSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { randomBytes } from "crypto";
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
  return new Promise<void>((resolve, reject) => {
    const url = new URL(path, `http://127.0.0.1:${port}`);
    if (token !== null) url.searchParams.set("token", token);

    const headers: Record<string, string> = {};
    if (origin !== null) headers["origin"] = origin;
    const wsOptions: { headers?: Record<string, string> } = {};
    if (Object.keys(headers).length > 0) wsOptions.headers = headers;

    const ws = new WebSocket(url.toString(), wsOptions) as WebSocket & {
      on: (event: string, listener: (...args: unknown[]) => void) => WebSocket;
    };

    let settled = false;
    let opened = false;

    const settleOk = (): void => {
      if (settled) return;
      settled = true;
      clearTimeout(openTimer);
      try { ws.terminate(); } catch { /* ignore */ }
      resolve();
    };

    const settleFail = (msg: string): void => {
      if (settled) return;
      settled = true;
      clearTimeout(openTimer);
      try { ws.terminate(); } catch { /* ignore */ }
      reject(new Error(msg));
    };

    const openTimer = setTimeout(() => {
      settleFail(`WS rejection timeout after ${REJECTION_TIMEOUT_MS}ms (neither unexpected-response nor error fired before open)`);
    }, REJECTION_TIMEOUT_MS);

    ws.on("open", () => {
      opened = true;
      // A successful open is the OPPOSITE of a rejection — the server
      // accepted the handshake, so any origin/token check we expected
      // to fire did NOT fire. Fail the test loud.
      settleFail("WS opened — expected handshake rejection but server accepted the connection");
    });
    ws.on("unexpected-response", () => {
      // The expected path: server returned an HTTP error response (e.g.
      // 403) before completing the upgrade.
      settleOk();
    });
    ws.on("error", () => {
      // Some Node/`ws` combos report a hard-refused upgrade as an
      // "error" instead of "unexpected-response". Only treat as a
      // rejection-equivalent when no open ever fired — otherwise we'd
      // also swallow post-open client-side errors as "rejection".
      if (!opened) settleOk();
      else settleFail("WS error after open — unexpected");
    });
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

// Keepalive tests live in a dedicated describe block because they need a
// much shorter interval (the production KEEPALIVE_INTERVAL_MS is 30s and
// impractical to wait for) and the "never-pongs" client needs a raw TCP
// socket that completes the WebSocket opening handshake by hand, since
// the `ws` npm client auto-responds to ping frames with pong unless its
// internal socket state is deeply monkey-patched — a raw `net.Socket`
// that just stops talking after the 101 Switching Protocols response is
// a more robust way to simulate a stuck/dead peer. The healthy-client
// tests use the regular `ws` client so they exercise the library's
// default auto-pong path too.
describe("WsGateway keepalive", () => {
  const KEEPALIVE_INTERVAL = 50;
  let fake: FakeTerminal;
  let gateway: WsGateway;
  let token: string;
  let port: number;
  const liveSockets: WebSocket[] = [];
  const rawSockets: NetSocket[] = [];

  function track(ws: WebSocket): void {
    liveSockets.push(ws);
  }
  function trackRaw(s: NetSocket): void {
    rawSockets.push(s);
  }
  function closeAllSockets(): void {
    for (const ws of liveSockets) {
      try { ws.terminate(); } catch { /* ignore */ }
    }
    liveSockets.length = 0;
    for (const s of rawSockets) {
      try { s.destroy(); } catch { /* ignore */ }
    }
    rawSockets.length = 0;
  }

  async function startKeepaliveGateway(): Promise<void> {
    fake = await makeFakeTerminal();
    token = "keepalive-tok-" + Math.random().toString(36).slice(2);
    const opts: WsGatewayOptions = {
      host: "127.0.0.1",
      port: 0,
      terminalSocketPath: fake.socketPath,
      token,
      clientHtml: "<!doctype html><html><body>k</body></html>",
      keepaliveIntervalMs: KEEPALIVE_INTERVAL,
    };
    gateway = new WsGateway(opts);
    await gateway.start();
    port = gateway.port;
  }

  beforeEach(async () => {
    await startKeepaliveGateway();
  });

  afterEach(async () => {
    closeAllSockets();
    gateway.stop();
    await closeFakeTerminal(fake);
  });

  // Hand-roll a minimal WebSocket client over a raw TCP socket. We
  // complete the opening handshake so the server-side upgrade succeeds,
  // then deliberately stop talking — never respond to ping frames, never
  // send close. This is the cleanest way to simulate a stuck client
  // without fighting the `ws` library's internal auto-pong.
  function openDeadClient(
    p: number,
    tok: string,
  ): Promise<NetSocket> {
    return new Promise((resolve, reject) => {
      const s = createNetConnection({ host: "127.0.0.1", port: p }, () => {
        // RFC 6455: Sec-WebSocket-Key is a 16-byte value base64-encoded
        // to 24 chars (with "==" padding). The `ws` server enforces this.
        const key = randomBytes(16).toString("base64");
        const req =
          `GET /ws?token=${tok} HTTP/1.1\r\n` +
          `Host: 127.0.0.1:${p}\r\n` +
          `Upgrade: websocket\r\n` +
          `Connection: Upgrade\r\n` +
          `Sec-WebSocket-Key: ${key}\r\n` +
          `Sec-WebSocket-Version: 13\r\n` +
          `Origin: http://127.0.0.1:${p}\r\n` +
          `\r\n`;
        s.write(req);
      });
      let resolved = false;
      s.on("error", (err: Error) => {
        if (!resolved) {
          resolved = true;
          reject(err);
        }
      });
      // Resolve once we see the 101 response — the server-side WS is
      // then fully open and the keepalive loop will start pinging it.
      const onData = (chunk: Buffer): void => {
        const text = chunk.toString("utf8");
        if (!resolved && text.includes("101")) {
          resolved = true;
          s.off("data", onData);
          // Drop any further incoming data — we never pong.
          s.on("data", () => { /* discard */ });
          resolve(s);
        }
      };
      s.on("data", onData);
    });
  }

  it("terminates a client that never answers pongs (after 2 ticks)", async () => {
    const sock = await openDeadClient(port, token);
    trackRaw(sock);

    // Wait for the server to terminate the socket. tick 1 (~50ms):
    // not-yet-dead → ping sent. tick 2 (~100ms): still isAlive=false →
    // terminate. Allow generous slack for scheduling jitter.
    const closed = new Promise<void>((resolve) => {
      sock.once("close", () => resolve());
    });
    await closed;

    expect(sock.destroyed).toBe(true);
  });

  it("keeps a healthy client (auto-pongs) alive across multiple keepalive ticks", async () => {
    const handle = await openWs(port, token);
    track(handle.ws);

    // Wait through at least 4 tick intervals; if the keepalive were
    // mis-terminating, the client would be gone well before this.
    const settleMs = KEEPALIVE_INTERVAL * 4 + 80;
    await new Promise<void>((resolve) => setTimeout(resolve, settleMs));

    expect(handle.ws.readyState).toBe(WebSocket.OPEN);
  });

  it("stop() clears the keepalive interval (no pings after stop)", async () => {
    const handle = await openWs(port, token);
    track(handle.ws);

    let pingCount = 0;
    handle.ws.on("ping", () => {
      pingCount += 1;
    });

    // Let several ticks fire while the gateway is alive.
    const observeMs = KEEPALIVE_INTERVAL * 3;
    await new Promise<void>((resolve) => setTimeout(resolve, observeMs));
    expect(pingCount).toBeGreaterThan(0);

    const pingsBeforeStop = pingCount;
    gateway.stop();
    // After stop() the client-side WS will close (server tears the
    // listener down), so any further "ping" events would only be
    // observable from a hypothetical second client. We instead measure
    // the absence: the existing client must NOT receive any new ping
    // frames after stop(). Give a generous slack window for any
    // in-flight tick that fired just before clearInterval.
    await new Promise<void>((resolve) => setTimeout(resolve, KEEPALIVE_INTERVAL * 3));
    expect(pingCount).toBe(pingsBeforeStop);
  });
});
