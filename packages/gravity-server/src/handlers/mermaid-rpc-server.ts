// mermaid-rpc-server.ts — TCP JSON-RPC server for mermaid rendering
// ⚠️ VIBECODED EXPERIMENT — This code was generated in a single AI-assisted session.
// It may contain bugs, design issues, or unexpected behavior. Use with caution.
// Listens on a TCP port for JSON-RPC requests to render mermaid diagrams.
// This allows emacs to call rendering without needing the full gravity-server IPC.

import { createServer, type Socket } from "net";
import { renderMermaidToASCII, type RenderMermaidParams } from "./mermaid-ascii.js";

const DEFAULT_PORT = 9876;

interface JsonRpcRequest {
  jsonrpc: "2.0";
  id: number | string | null;
  method: string;
  params?: unknown;
}

interface JsonRpcResponse {
  jsonrpc: "2.0";
  id: number | string | null;
  result?: unknown;
  error?: {
    code: number;
    message: string;
  };
}

export interface MermaidRpcServerOptions {
  port?: number;
  host?: string;
}

export class MermaidRpcServer {
  private server: ReturnType<typeof createServer>;
  private port: number;
  private host: string;

  constructor(options: MermaidRpcServerOptions = {}) {
    this.port = options.port ?? DEFAULT_PORT;
    this.host = options.host ?? "127.0.0.1";
    this.server = createServer(this.handleConnection.bind(this));
  }

  start(): Promise<void> {
    return new Promise((resolve, reject) => {
      this.server.on("error", (err) => {
        reject(err);
      });

      this.server.listen(this.port, this.host, () => {
        console.log(`[mermaid-rpc] Listening on ${this.host}:${this.port}`);
        resolve();
      });
    });
  }

  stop(): void {
    this.server.close();
  }

  private handleConnection(socket: Socket): void {
    let buffer = "";

    socket.on("data", (chunk: Buffer) => {
      buffer += chunk.toString();

      // Process complete JSON lines
      let newlineIdx: number;
      while ((newlineIdx = buffer.indexOf("\n")) !== -1) {
        const line = buffer.substring(0, newlineIdx).trim();
        buffer = buffer.substring(newlineIdx + 1);

        if (line.length === 0) continue;

        try {
          const request = JSON.parse(line) as JsonRpcRequest;
          const response = this.handleRequest(request);
          socket.write(JSON.stringify(response) + "\n");
        } catch (e) {
          const errorResponse: JsonRpcResponse = {
            jsonrpc: "2.0",
            id: null,
            error: {
              code: -32700,
              message: `Parse error: ${e}`,
            },
          };
          socket.write(JSON.stringify(errorResponse) + "\n");
        }
      }
    });

    socket.on("error", (err) => {
      console.error(`[mermaid-rpc] Socket error: ${err.message}`);
    });

    socket.on("close", () => {
      // Cleanup if needed
    });
  }

  private handleRequest(request: JsonRpcRequest): JsonRpcResponse {
    // Validate JSON-RPC format
    if (request.jsonrpc !== "2.0") {
      return {
        jsonrpc: "2.0",
        id: request.id ?? null,
        error: {
          code: -32600,
          message: "Invalid JSON-RPC request",
        },
      };
    }

    try {
      switch (request.method) {
        case "renderMermaid":
          return this.handleRenderMermaid(request);

        case "renderMermaidANSI":
          return this.handleRenderMermaidANSI(request);

        case "health":
          return {
            jsonrpc: "2.0",
            id: request.id,
            result: { status: "ok", service: "mermaid-rpc" },
          };

        default:
          return {
            jsonrpc: "2.0",
            id: request.id,
            error: {
              code: -32601,
              message: `Method not found: ${request.method}`,
            },
          };
      }
    } catch (e) {
      return {
        jsonrpc: "2.0",
        id: request.id,
        error: {
          code: -32603,
          message: `Internal error: ${e}`,
        },
      };
    }
  }

  private handleRenderMermaid(request: JsonRpcRequest): JsonRpcResponse {
    const params = request.params as RenderMermaidParams | undefined;

    if (!params || typeof params.source !== "string") {
      return {
        jsonrpc: "2.0",
        id: request.id,
        error: {
          code: -32602,
          message: "Invalid params: source string required",
        },
      };
    }

    const result = renderMermaidToASCII(params.source, {
      useAscii: params.options?.useAscii,
      theme: params.options?.theme,
    });

    return {
      jsonrpc: "2.0",
      id: request.id,
      result,
    };
  }

  private handleRenderMermaidANSI(request: JsonRpcRequest): JsonRpcResponse {
    // Import the ANSI renderer (lazy to avoid circular deps)
    const { renderMermaidToANSI } = require("./mermaid-ascii.js");

    const params = request.params as RenderMermaidParams | undefined;

    if (!params || typeof params.source !== "string") {
      return {
        jsonrpc: "2.0",
        id: request.id,
        error: {
          code: -32602,
          message: "Invalid params: source string required",
        },
      };
    }

    const result = renderMermaidToANSI(params.source, {
      useAscii: params.options?.useAscii,
      theme: params.options?.theme,
    });

    return {
      jsonrpc: "2.0",
      id: request.id,
      result,
    };
  }
}

// Quick start: run standalone
if (import.meta.url === `file://${process.argv[1]}`) {
  const port = parseInt(process.env.MERMAID_RPC_PORT ?? `${DEFAULT_PORT}`, 10);
  const server = new MermaidRpcServer({ port });
  server.start().catch((e) => {
    console.error(`Failed to start mermaid RPC server: ${e}`);
    process.exit(1);
  });
}

export default MermaidRpcServer;