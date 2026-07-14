// ws-gateway-startup-resilience.test.ts — Verifies the actual
// `startGatewayNonFatal` helper exported from `gravity-server.ts`, which
// wraps `WsGateway.start()` so a bind failure never escapes as a fatal
// error or an unhandled rejection:
//
//   export function startGatewayNonFatal(gateway, host, log) {
//     return gateway.start()
//       .then(() => log(`Browser gateway listening on ...`))
//       .catch((e) => log(`Browser gateway failed to start: ${e}`, "warn"));
//   }
//
// The contract: if `start()` rejects (e.g. port already in use), the
// `.catch` handler must run, the promise chain must settle without an
// unhandled rejection reaching the process, and the log line must
// contain a meaningful "failed to start" message. The first listener
// occupies the port via `bind 0` so we always start from a real
// ephemeral port; no hardcoded port numbers anywhere in this file.

import { afterEach, describe, expect, it } from "vitest";
import { mkdtempSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { createServer as createNetServer, type Server as NetServer } from "net";

import { WsGateway } from "../src/gateway/ws-gateway.js";
import { startGatewayNonFatal } from "../src/gravity-server.js";

function makeTempSocketDir(): string {
  return mkdtempSync(join(tmpdir(), "gravity-ws-gw-resilience-"));
}

function cleanupTempSocketDir(dir: string): void {
  try { rmSync(dir, { recursive: true, force: true }); } catch { /* ignore */ }
}

interface HeldPort {
  port: number;
  server: NetServer;
}

async function bindEphemeral(host: string): Promise<HeldPort> {
  return new Promise((resolve, reject) => {
    const s = createNetServer();
    s.on("error", reject);
    s.listen(0, host, () => {
      const addr = s.address();
      if (addr && typeof addr === "object") {
        resolve({ port: addr.port, server: s });
      } else {
        reject(new Error("could not read port from held listener"));
      }
    });
  });
}

describe("WsGateway startup resilience (startGatewayNonFatal wrapper)", () => {
  let tempDir: string;
  let occupant: WsGateway | null = null;
  let heldPort: HeldPort | null = null;
  // unhandledRejection listener — registered for the duration of the
  // test so any escaped rejection fails the test loud instead of being
  // silently dropped.
  let unhandled: ((reason: unknown) => void) | null = null;

  function makeGateway(token: string, port: number): WsGateway {
    return new WsGateway({
      host: "127.0.0.1",
      port,
      terminalSocketPath: join(tempDir, "terminal.sock"),
      token,
      clientHtml: "<!doctype html><html><body>x</body></html>",
    });
  }

  afterEach(() => {
    if (occupant) {
      try { occupant.stop(); } catch { /* ignore */ }
      occupant = null;
    }
    if (heldPort) {
      try { heldPort.server.close(); } catch { /* ignore */ }
      heldPort = null;
    }
    if (tempDir) {
      cleanupTempSocketDir(tempDir);
      tempDir = "";
    }
    if (unhandled) {
      process.off("unhandledRejection", unhandled);
      unhandled = null;
    }
  });

  it("logs a 'failed to start' warning and does not throw when start() rejects on a busy port (real WsGateway)", async () => {
    tempDir = makeTempSocketDir();

    // Occupy a real free port with a first WsGateway, then read the
    // OS-assigned port back via `.port`. The first gateway holds it
    // until afterEach.
    const tok1 = "occupant-token";
    occupant = makeGateway(tok1, 0);
    await occupant.start();
    const occupiedPort = occupant.port;
    expect(occupiedPort).toBeGreaterThan(0);

    const logs: string[] = [];
    const logFn = (msg: string, level?: string): void => {
      logs.push(`[${level ?? "info"}] ${msg}`);
    };

    let unhandledReason: unknown = null;
    unhandled = (reason: unknown): void => { unhandledReason = reason; };
    process.on("unhandledRejection", unhandled);

    const second = makeGateway("loser-token", occupiedPort);
    try {
      // Exercise the actual production wrapper, with logFn pointed at
      // our array instead of the real `logMsg`.
      await startGatewayNonFatal(second, "127.0.0.1", logFn);

      // Give the event loop a tick to surface any unhandledRejection.
      await new Promise<void>((resolve) => setImmediate(resolve));

      // The .catch handler MUST have run with a message mirroring the
      // real log line — proves the wrapper actually executed and the
      // failure path produced a meaningful operator-facing message.
      const failureLines = logs.filter((l) => l.includes("failed to start"));
      expect(failureLines.length).toBeGreaterThan(0);
      expect(failureLines[0]).toContain("warn");
      // Should not have a "listening on" line — that branch must not
      // run when start() rejected.
      expect(logs.some((l) => l.includes("listening on"))).toBe(false);

      // And nothing escaped as an unhandled rejection.
      expect(unhandledReason).toBeNull();
    } finally {
      try { second.stop(); } catch { /* ignore */ }
    }
  });

  it("logs a 'failed to start' warning and does not throw when the port is held by a bare TCP server", async () => {
    tempDir = makeTempSocketDir();

    // Bind a bare `net` server on port 0 and read back the real port —
    // proves the resilience wrapper works against ANY process that
    // happens to hold the port, not just another WsGateway.
    heldPort = await bindEphemeral("127.0.0.1");
    expect(heldPort.port).toBeGreaterThan(0);

    let unhandledReason: unknown = null;
    unhandled = (reason: unknown): void => { unhandledReason = reason; };
    process.on("unhandledRejection", unhandled);

    const logs: string[] = [];
    const logFn = (msg: string, level?: string): void => {
      logs.push(`[${level ?? "info"}] ${msg}`);
    };

    const second = makeGateway("loser-token-2", heldPort.port);
    try {
      await startGatewayNonFatal(second, "127.0.0.1", logFn);

      await new Promise<void>((resolve) => setImmediate(resolve));

      const failureLines = logs.filter((l) => l.includes("failed to start"));
      expect(failureLines.length).toBeGreaterThan(0);
      expect(failureLines[0]).toContain("warn");
      expect(logs.some((l) => l.includes("listening on"))).toBe(false);
      expect(unhandledReason).toBeNull();
    } finally {
      try { second.stop(); } catch { /* ignore */ }
    }
  });
});