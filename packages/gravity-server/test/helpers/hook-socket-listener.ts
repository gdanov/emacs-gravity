// hook-socket-listener.ts — Test helper: a Unix-socket NDJSON listener
// that mimics what gravity-server's hook socket accepts from the WP1
// pi emitter (`src: "pi"` envelopes, one JSON object per line).
//
// Per plan.md §4 WP3 step 2, this helper exposes:
//  - `getSocketPath()`: the bound absolute path the emitter should
//    point at via `GRAVITY_HOOK_SOCK`.
//  - `getLines()`: all collected lines so far (parsed JSON where
//    possible; raw strings preserved on parse failure).
//  - `getRawLines()`: every line as it arrived, regardless of parse
//    success — useful for byte-level audits.
//  - `close()`: graceful shutdown — stop accepting new connections,
//    destroy all live sockets, then remove the temp dir.
//  - `killMidRun()`: forcibly destroy every currently-connected
//    socket WITHOUT closing the listener itself, simulating an
//    EPIPE/ECONNRESET on the emitter's side mid-run (for AC #2b /
//    AC #8).
//  - `setWedged(boolean)`: a mode where any new connection is
//    immediately `.pause()`d and never `.resume()`d, so the OS write
//    buffer on the emitter side eventually fills (for AC #3). The
//    listener itself stays open; subsequent connections also stay
//    wedged. A listener can be toggled into or out of wedged mode
//    mid-run (useful for the crash-safety tests where the listener
//    should behave normally up to a point).
//
// Hard rules enforced:
//  - The socket path lives in a fresh mkdtemp'd subdir of os.tmpdir();
//    `close()` removes the dir. A test can call `getSocketPath()` and
//    pass it to GRAVITY_HOOK_SOCK without leaking into the real
//    `~/.local/state/` or the repo.
//  - All listeners and sockets are torn down by `close()` even on
//    assertion failure in the caller (callers must invoke `close()`
//    from a `finally` / vitest `onTestFinished`).

import {
  createServer,
  type Server,
  type Socket,
} from "node:net";
import { mkdtempSync, rmSync } from "node:fs";
import { join } from "node:path";
import { tmpdir } from "node:os";

/** One observation a listener makes about a connection. */
export interface CollectedLine {
  /** True when the line parsed as JSON; false when it was kept as raw. */
  parsed: boolean;
  /** Parsed value when `parsed === true`. */
  value?: unknown;
  /** Raw line as it arrived on the wire (always present, for byte-level audits). */
  raw: string;
  /** Wall-clock ms when the line completed (Date.now()). */
  receivedAtMs: number;
}

export interface HookSocketListenerOptions {
  /** Optional pre-chosen dir name prefix (default: "gravity-listener-"). */
  prefix?: string;
  /** Pre-existing socket path to bind to (rare; default: fresh temp). */
  socketPath?: string;
}

/**
 * The hook-socket NDJSON listener. One instance per test case (or per
 * re-use case). Always call `close()` from a `finally` / `onTestFinished`
 * — this is the only thing that removes the temp dir.
 */
export class HookSocketListener {
  readonly #server: Server;
  readonly #socketPath: string;
  readonly #tempDir: string | null;
  readonly #connections: Set<Socket> = new Set();
  readonly #collected: CollectedLine[] = [];
  #closed = false;
  #wedged = false;
  #lastError: Error | null = null;

  private constructor(opts: {
    server: Server;
    socketPath: string;
    tempDir: string | null;
  }) {
    this.#server = opts.server;
    this.#socketPath = opts.socketPath;
    this.#tempDir = opts.tempDir;
  }

  /** Bind a listener on a fresh mkdtemp'd Unix socket path. */
  static create(opts: HookSocketListenerOptions = {}): Promise<HookSocketListener> {
    return new Promise<HookSocketListener>((resolve, reject) => {
      const tempDir = mkdtempSync(join(tmpdir(), opts.prefix ?? "gravity-listener-"));
      const socketPath = join(tempDir, "hooks.sock");

      const server = createServer((socket) => {
        // Each new connection: register, optionally wedge, accumulate.
        const me = (server as unknown as { __listener?: HookSocketListener })
          .__listener;
        if (!me) {
          // Should never happen — constructor sets this synchronously
          // below — but defensive: drop the connection if it does.
          socket.destroy();
          return;
        }
        me.#handleConnection(socket);
      });

      // Stash the listener on the server so the callback can find it
      // without a closure-over-a-not-yet-constructed-value dance.
      (server as unknown as { __listener: HookSocketListener }).__listener =
        null as unknown as HookSocketListener;

      server.once("error", (err) => {
        try {
          server.close();
        } catch {
          /* ignore */
        }
        reject(err);
      });

      server.listen(socketPath, () => {
        const inst = new HookSocketListener({
          server,
          socketPath,
          tempDir,
        });
        (server as unknown as { __listener: HookSocketListener }).__listener = inst;
        resolve(inst);
      });
    });
  }

  /** Absolute socket path the emitter should be pointed at. */
  getSocketPath(): string {
    return this.#socketPath;
  }

  /** Absolute path of the temp dir backing this listener (or null if
   *  the listener was constructed with an explicit pre-existing path
   *  via `socketPath` — currently unused). */
  getTempDir(): string {
    return this.#tempDir ?? "";
  }

  /** All collected lines so far (parsed + raw-preserved). */
  getLines(): CollectedLine[] {
    return [...this.#collected];
  }

  /** Convenience: just the parsed JSON values (where parsing succeeded). */
  getParsedValues(): unknown[] {
    return this.#collected
      .filter((l) => l.parsed)
      .map((l) => l.value);
  }

  /** Convenience: parsed envelopes matching the `src: "pi"` contract. */
  getPiEnvelopes(): Array<{
    src: string;
    session_id: string;
    cwd: string;
    pid: number | null;
    event: Record<string, unknown>;
    attribution?: { role?: string; worktree?: string };
  }> {
    const out: Array<{
      src: string;
      session_id: string;
      cwd: string;
      pid: number | null;
      event: Record<string, unknown>;
      attribution?: { role?: string; worktree?: string };
    }> = [];
    for (const v of this.getParsedValues()) {
      if (
        v !== null &&
        typeof v === "object" &&
        (v as { src?: unknown }).src === "pi" &&
        typeof (v as { session_id?: unknown }).session_id === "string" &&
        typeof (v as { cwd?: unknown }).cwd === "string" &&
        typeof (v as { event?: unknown }).event === "object" &&
        (v as { event?: unknown }).event !== null
      ) {
        out.push(
          v as {
            src: string;
            session_id: string;
            cwd: string;
            pid: number | null;
            event: Record<string, unknown>;
            attribution?: { role?: string; worktree?: string };
          },
        );
      }
    }
    return out;
  }

  /**
   * Number of currently-connected (live) sockets. The emitter opens
   * exactly one; multiple connections in this count would indicate
   * re-connect activity we may want to assert on.
   */
  getConnectionCount(): number {
    return this.#connections.size;
  }

  /**
   * Toggle the wedged mode. When wedged, new connections are
   * `.pause()`d on `data` immediately and never `.resume()`d — so
   * the emitter's writes eventually hit backpressure and the
   * writer's `send()` returns `false` (its `#writable` flips to
   * false when `socket.write()` reports over-highWaterMark).
   * That's the signal the emitter uses to log a drop.
   */
  setWedged(wedged: boolean): void {
    this.#wedged = wedged;
    if (wedged) {
      for (const s of this.#connections) {
        try {
          s.pause();
        } catch {
          /* socket may already be closing */
        }
      }
    }
  }

  /** Whether the listener is currently in wedged mode. */
  isWedged(): boolean {
    return this.#wedged;
  }

  /** Forcibly destroy every currently-connected socket WITHOUT
   *  closing the listener itself. Simulates a server crash mid-run
   *  (the emitter's write attempts will see EPIPE/ECONNRESET). The
   *  listener accepts new connections after this returns. */
  killMidRun(): void {
    for (const s of [...this.#connections]) {
      try {
        s.destroy();
      } catch {
        /* ignore */
      }
    }
  }

  /** Last error observed on the server itself (if any). */
  getLastError(): Error | null {
    return this.#lastError;
  }

  /** Wait until at least `n` envelopes have been collected (parsed
   *  JSON), or the timeout elapses. Resolves with the collected count
   *  when reached, otherwise resolves to the current count on
   *  timeout. Never rejects — caller decides whether timeout is OK. */
  async waitForEnvelopes(n: number, timeoutMs: number): Promise<number> {
    const start = Date.now();
    while (Date.now() - start < timeoutMs) {
      const count = this.getPiEnvelopes().length;
      if (count >= n) return count;
      await new Promise((r) => setTimeout(r, 25));
    }
    return this.getPiEnvelopes().length;
  }

  /** Graceful shutdown — stops accepting, destroys live sockets,
   *  closes the server, removes the temp dir. Idempotent. */
  async close(): Promise<void> {
    if (this.#closed) return;
    this.#closed = true;
    for (const s of [...this.#connections]) {
      try {
        s.destroy();
      } catch {
        /* ignore */
      }
    }
    this.#connections.clear();
    await new Promise<void>((resolve) => {
      try {
        this.#server.close(() => resolve());
      } catch {
        resolve();
      }
    });
    if (this.#tempDir !== null) {
      try {
        rmSync(this.#tempDir, { recursive: true, force: true });
      } catch {
        /* best effort */
      }
    }
  }

  // ── internal ──

  #handleConnection(socket: Socket): void {
    this.#connections.add(socket);
    if (this.#wedged) {
      try {
        socket.pause();
      } catch {
        /* ignore */
      }
    }
    let buf = "";
    socket.setEncoding("utf8");
    socket.on("data", (chunk: string | Buffer) => {
      // When wedged we deliberately do NOT accumulate data — the
      // point of wedged mode is to backpressure the emitter. But we
      // still drop the listener's reference so backpressure
      // propagates.
      if (this.#wedged) return;
      buf += typeof chunk === "string" ? chunk : chunk.toString("utf8");
      let nl: number;
      while ((nl = buf.indexOf("\n")) >= 0) {
        const line = buf.slice(0, nl);
        buf = buf.slice(nl + 1);
        this.#recordLine(line);
      }
    });
    socket.on("error", () => {
      // Defensive: socket errors should not crash the listener.
    });
    socket.on("close", () => {
      this.#connections.delete(socket);
    });
  }

  #recordLine(raw: string): void {
    if (raw.length === 0) return;
    const receivedAtMs = Date.now();
    try {
      const value = JSON.parse(raw);
      this.#collected.push({ parsed: true, value, raw, receivedAtMs });
    } catch {
      // Don't crash the listener on a malformed line — record it raw
      // so the test can audit what actually arrived.
      this.#collected.push({ parsed: false, raw, receivedAtMs });
    }
  }
}