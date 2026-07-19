// ws-gateway.ts — Browser-facing HTTP + WebSocket gateway.
//
// Bridges WebSocket clients to the existing terminal Unix domain socket,
// one WS connection per terminal-socket connection. Owns the byte-level
// protocol translation only — no session state, no protocol knowledge
// beyond "complete lines ↔ one WS text message" framing. Authentication
// is a per-process-start token supplied as the `?token=` query param on
// the upgrade request, with an Origin allow-list limited to the loopback
// host:port pair of this same listener.

import { createServer as createHttpServer, type IncomingMessage, type Server as HttpServer } from "http";
import type { AddressInfo } from "net";
import { createConnection } from "net";
import type { Duplex } from "stream";
import { WebSocketServer, type WebSocket } from "ws";

const KEEPALIVE_INTERVAL_MS = 30_000;
const WS_PATH = "/ws";
const MAX_LINE_BYTES = 1024 * 1024;

export interface WsGatewayOptions {
  /** Bind host, e.g. "127.0.0.1". */
  host: string;
  /** Bind port. 0 = OS-assigned ephemeral (read back via `.port` after `start()`). */
  port: number;
  /** Path to the terminal Unix domain socket this gateway bridges to. One
   * new connection to this path is opened per accepted WS connection. */
  terminalSocketPath: string;
  /** Random per-process-start token. Required as a `?token=` query param
   * on every `/ws` upgrade request. */
  token: string;
  /** Pre-rendered client HTML (token placeholder already substituted).
   * Served verbatim on GET "/" and GET "/index.html". */
  clientHtml: string;
  /** Optional log sink; defaults to a no-op. Signature mirrors
   * `makeTerminal`'s `logFn` in services/terminal.ts. */
  logFn?: (msg: string, level?: string) => void;
  /** Keepalive ping interval override, mostly for tests. Defaults to
   * KEEPALIVE_INTERVAL_MS (30s) when omitted. */
  keepaliveIntervalMs?: number;
}

interface TrackedWebSocket extends WebSocket {
  isAlive: boolean;
}

export class WsGateway {
  private readonly opts: WsGatewayOptions;
  private readonly logFn: (msg: string, level?: string) => void;
  private readonly httpServer: HttpServer;
  private readonly wss: WebSocketServer;
  private readonly live: Set<TrackedWebSocket> = new Set();
  private keepalive: ReturnType<typeof setInterval> | null = null;
  private boundPort: number = 0;
  private started: boolean = false;
  private stopped: boolean = false;

  constructor(options: WsGatewayOptions) {
    this.opts = options;
    this.logFn = options.logFn ?? ((): void => {});

    this.wss = new WebSocketServer({ noServer: true });

    this.wss.on("connection", (ws: WebSocket) => {
      this.handleConnection(ws as TrackedWebSocket);
    });

    this.httpServer = createHttpServer((req, res) => {
      this.handleHttpRequest(req, res);
    });

    this.httpServer.on("upgrade", (req, socket, head) => {
      this.handleUpgrade(req, socket, head);
    });

    this.httpServer.on("error", (err) => {
      this.logFn(`Browser gateway HTTP error: ${err.message}`, "error");
    });
  }

  start(): Promise<void> {
    return new Promise((resolve, reject) => {
      if (this.started) {
        resolve();
        return;
      }
      this.httpServer.once("error", (err) => {
        reject(err);
      });
      this.httpServer.listen(this.opts.port, this.opts.host, () => {
        const addr = this.httpServer.address();
        if (addr && typeof addr === "object") {
          this.boundPort = (addr as AddressInfo).port;
        }
        this.keepalive = setInterval(() => {
          this.tickKeepalive();
        }, this.opts.keepaliveIntervalMs ?? KEEPALIVE_INTERVAL_MS);
        this.started = true;
        resolve();
      });
    });
  }

  stop(): void {
    if (this.stopped) return;
    this.stopped = true;

    for (const ws of [...this.live]) {
      try {
        ws.terminate();
      } catch {
        // Swallow — terminate() can throw if the socket is mid-close.
      }
    }
    this.live.clear();

    if (this.keepalive) {
      clearInterval(this.keepalive);
      this.keepalive = null;
    }

    this.wss.close();
    this.httpServer.close();
  }

  get port(): number {
    return this.boundPort;
  }

  private tickKeepalive(): void {
    for (const ws of [...this.live]) {
      if (ws.isAlive === false) {
        try {
          ws.terminate();
        } catch {
          // Already gone
        }
        this.live.delete(ws);
        continue;
      }
      ws.isAlive = false;
      try {
        ws.ping();
      } catch {
        try { ws.terminate(); } catch { /* ignore */ }
        this.live.delete(ws);
      }
    }
  }

  private handleHttpRequest(req: IncomingMessage, res: import("http").ServerResponse): void {
    if (req.method !== "GET") {
      res.writeHead(405, { "content-type": "text/plain" });
      res.end("Method Not Allowed");
      return;
    }
    if (req.url === "/" || req.url === "/index.html") {
      res.writeHead(200, { "content-type": "text/html; charset=utf-8" });
      res.end(this.opts.clientHtml);
      return;
    }
    res.writeHead(404, { "content-type": "text/plain" });
    res.end("Not Found");
  }

  private handleUpgrade(req: IncomingMessage, socket: Duplex, head: Buffer): void {
    if (this.boundPort === 0) {
      this.rejectUpgrade(socket);
      return;
    }

    let url: URL;
    try {
      url = new URL(req.url ?? "/", "http://placeholder");
    } catch {
      this.rejectUpgrade(socket);
      return;
    }

    if (url.pathname !== WS_PATH) {
      this.rejectUpgrade(socket);
      return;
    }

    if (!this.originAllowed(req.headers.origin, this.boundPort)) {
      this.rejectUpgrade(socket);
      return;
    }

    const token = url.searchParams.get("token");
    if (token !== this.opts.token) {
      this.rejectUpgrade(socket);
      return;
    }

    this.wss.handleUpgrade(req, socket, head, (ws) => {
      this.wss.emit("connection", ws, req);
    });
  }

  private originAllowed(origin: string | undefined, port: number): boolean {
    if (typeof origin !== "string" || origin.length === 0) {
      return false;
    }
    let parsed: URL;
    try {
      parsed = new URL(origin);
    } catch {
      return false;
    }
    if (parsed.protocol !== "http:") return false;
    if (parsed.port !== String(port)) return false;
    if (parsed.hostname !== "127.0.0.1" && parsed.hostname !== "localhost") {
      return false;
    }
    return true;
  }

  private rejectUpgrade(socket: Duplex): void {
    try {
      socket.write("HTTP/1.1 403 Forbidden\r\n\r\n");
    } catch {
      // socket may already be in a half-closed state
    }
    socket.destroy();
  }

  private handleConnection(ws: TrackedWebSocket): void {
    ws.isAlive = true;
    this.live.add(ws);

    ws.on("pong", () => {
      ws.isAlive = true;
    });

    const terminalSocket = createConnection(this.opts.terminalSocketPath);

    let inboundBuffer = "";
    terminalSocket.on("data", (chunk: Buffer) => {
      inboundBuffer += chunk.toString("utf8");
      if (inboundBuffer.length > MAX_LINE_BYTES) {
        this.logFn("Browser gateway: inbound line buffer exceeded limit, closing", "warn");
        try { ws.terminate(); } catch { /* ignore */ }
        terminalSocket.destroy();
        return;
      }
      let newlineIdx: number;
      while ((newlineIdx = inboundBuffer.indexOf("\n")) !== -1) {
        const line = inboundBuffer.substring(0, newlineIdx);
        inboundBuffer = inboundBuffer.substring(newlineIdx + 1);
        if (line.length === 0) continue;
        try {
          ws.send(line);
        } catch (err) {
          this.logFn(`Browser gateway WS send failed: ${(err as Error).message}`, "warn");
        }
      }
    });

    ws.on("message", (data, isBinary) => {
      if (isBinary) {
        this.logFn("Browser gateway: binary WS frame rejected", "warn");
        return;
      }
      const text = typeof data === "string" ? data : Buffer.isBuffer(data) ? data.toString("utf8") : "";
      if (text.length === 0) return;
      try {
        terminalSocket.write(text + "\n");
      } catch (err) {
        this.logFn(`Browser gateway terminal write failed: ${(err as Error).message}`, "warn");
      }
    });

    terminalSocket.on("error", (err) => {
      this.logFn(`Browser gateway terminal socket error: ${err.message}`, "warn");
    });

    terminalSocket.on("close", () => {
      try { ws.close(1011, "terminal socket closed"); } catch { /* ignore */ }
    });

    ws.on("close", () => {
      this.live.delete(ws);
      try { terminalSocket.destroy(); } catch { /* ignore */ }
    });

    ws.on("error", (err) => {
      this.logFn(`Browser gateway WS error: ${err.message}`, "warn");
      try { terminalSocket.destroy(); } catch { /* ignore */ }
    });
  }
}
