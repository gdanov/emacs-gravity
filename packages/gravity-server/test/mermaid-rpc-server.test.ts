// mermaid-rpc-server.test.ts — Tests for TCP JSON-RPC server
// ⚠️ VIBECODED EXPERIMENT — This code was generated in a single AI-assisted session.
// It may contain bugs, design issues, or unexpected behavior. Use with caution.
// Tests the TCP server for mermaid rendering requests.

import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import { MermaidRpcServer, type MermaidRpcServerOptions } from "../src/handlers/mermaid-rpc-server.js";
import { createServer, createConnection, type Socket } from "net";

interface TestServer {
  port: number;
  start: () => Promise<void>;
  stop: () => void;
}

function createTestServer(options: MermaidRpcServerOptions = {}): TestServer {
  const server = new MermaidRpcServer(options);
  let resolvedPort: number;

  return {
    get port() {
      return resolvedPort;
    },
    async start() {
      // Find available port
      const tmp = createServer();
      await new Promise<void>((resolve) => {
        tmp.listen(0, "127.0.0.1", () => {
          resolvedPort = (tmp.address() as { port: number }).port;
          tmp.close();
          resolve();
        });
      });

      const serverWithPort = new MermaidRpcServer({ port: resolvedPort, ...options });
      await serverWithPort.start();
      Object.setPrototypeOf(server, serverWithPort);
    },
    stop() {
      server.stop();
    },
  };
}

function sendRpc(port: number, request: object): Promise<unknown> {
  return new Promise((resolve, reject) => {
    const socket = createConnection(port, "127.0.0.1", () => {
      let response = "";
      socket.on("data", (chunk: Buffer) => {
        response += chunk.toString();
      });
      socket.on("end", () => {
        socket.destroy();
        try {
          resolve(JSON.parse(response.trim()));
        } catch {
          reject(new Error(`Failed to parse response: ${response}`));
        }
      });
      socket.on("error", reject);
      socket.write(JSON.stringify(request) + "\n");
      socket.end();
    });
    socket.on("error", reject);
  });
}

describe("MermaidRpcServer", () => {
  let server: TestServer;
  let port: number;

  beforeEach(async () => {
    server = createTestServer();
    await server.start();
    port = (server as { port: number }).port;
  });

  afterEach(() => {
    server.stop();
  });

  describe("health", () => {
    it("returns health status", async () => {
      const response = await sendRpc(port, {
        jsonrpc: "2.0",
        id: 1,
        method: "health",
      }) as { result: { status: string; service: string } };

      expect(response.jsonrpc).toBe("2.0");
      expect(response.id).toBe(1);
      expect(response.result).toBeDefined();
      expect(response.result.status).toBe("ok");
      expect(response.result.service).toBe("mermaid-rpc");
    });
  });

  describe("renderMermaid", () => {
    it("renders simple flowchart", async () => {
      const response = await sendRpc(port, {
        jsonrpc: "2.0",
        id: 1,
        method: "renderMermaid",
        params: {
          source: "graph TD\nA-->B",
        },
      }) as { result: string };

      expect(response.result).toBeDefined();
      expect(typeof response.result).toBe("string");
      expect(response.result.length).toBeGreaterThan(0);
    });

    it("renders flowchart with nodes", async () => {
      const response = await sendRpc(port, {
        jsonrpc: "2.0",
        id: 2,
        method: "renderMermaid",
        params: {
          source: "graph LR\nA[Start]-->B{Decision}\nB-->|Yes|C[Action]\nB-->|No|D[End]",
        },
      }) as { result: string };

      expect(response.result).toBeDefined();
      expect(response.result).toContain("Start");
      expect(response.result).toContain("Decision");
    });

    it("renders with useAscii option", async () => {
      const response = await sendRpc(port, {
        jsonrpc: "2.0",
        id: 3,
        method: "renderMermaid",
        params: {
          source: "graph LR\nA-->B",
          options: { useAscii: true },
        },
      }) as { result: string };

      expect(response.result).toBeDefined();
      // ASCII mode uses +---|--> style connectors
      expect(response.result).toMatch(/(\+---|->)/);
    });

    it("renders with Unicode box-drawing by default", async () => {
      const response = await sendRpc(port, {
        jsonrpc: "2.0",
        id: 4,
        method: "renderMermaid",
        params: {
          source: "graph LR\nA-->B",
        },
      }) as { result: string };

      expect(response.result).toBeDefined();
      // Unicode mode uses ┌|│|─|► style connectors
      expect(response.result).toMatch(/(┌|└|│|─|►)/);
    });

    it("returns error for missing source", async () => {
      const response = await sendRpc(port, {
        jsonrpc: "2.0",
        id: 5,
        method: "renderMermaid",
        params: {},
      }) as { error: { code: number; message: string } };

      expect(response.error).toBeDefined();
      expect(response.error.code).toBe(-32602);
      expect(response.error.message).toContain("source string required");
    });

    it("returns error for invalid mermaid", async () => {
      const response = await sendRpc(port, {
        jsonrpc: "2.0",
        id: 6,
        method: "renderMermaid",
        params: {
          source: "this is not valid mermaid [",
        },
      }) as { result: string };

      // Error is returned as string in result, not as JSON-RPC error
      expect(response.result).toBeDefined();
      expect(typeof response.result).toBe("string");
    });
  });

  describe("method not found", () => {
    it("returns error for unknown method", async () => {
      const response = await sendRpc(port, {
        jsonrpc: "2.0",
        id: 7,
        method: "unknownMethod",
      }) as { error: { code: number; message: string } };

      expect(response.error).toBeDefined();
      expect(response.error.code).toBe(-32601);
      expect(response.error.message).toContain("Method not found");
    });
  });

  describe("invalid JSON-RPC", () => {
    it("returns error for invalid jsonrpc version", async () => {
      const response = await sendRpc(port, {
        jsonrpc: "1.0",
        id: 8,
        method: "health",
      }) as { error: { code: number; message: string } };

      expect(response.error).toBeDefined();
      expect(response.error.code).toBe(-32600);
    });
  });
});

describe("MermaidRpcServer standalone", () => {
  it("can be imported and instantiated", () => {
    const server = new MermaidRpcServer({ port: 0 });
    expect(server).toBeDefined();
    expect(typeof server.start).toBe("function");
    expect(typeof server.stop).toBe("function");
  });

  it("uses default port 9876", () => {
    const server = new MermaidRpcServer();
    expect((server as unknown as { port: number }).port).toBe(9876);
  });

  it("uses custom port when specified", () => {
    const server = new MermaidRpcServer({ port: 9999 });
    expect((server as unknown as { port: number }).port).toBe(9999);
  });

  it("uses localhost by default", () => {
    const server = new MermaidRpcServer();
    expect((server as unknown as { host: string }).host).toBe("127.0.0.1");
  });

  it("uses custom host when specified", () => {
    const server = new MermaidRpcServer({ host: "0.0.0.0" });
    expect((server as unknown as { host: string }).host).toBe("0.0.0.0");
  });
});