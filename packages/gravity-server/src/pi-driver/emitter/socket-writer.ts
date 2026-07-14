// socket-writer.ts — Unix-domain-socket writer with lazy connect and
// detached fixed-cadence reconnect.
//
// Per the spec:
//   - Connect lazily at session_start (never from the extension factory).
//   - `connect()` is idempotent; calling twice is safe.
//   - `connect()` MUST NOT throw synchronously — register 'error'/'close'
//     listeners that mark the writer disconnected and schedule exactly
//     one reconnect attempt after `reconnectIntervalMs`.
//   - `send(envelope)` returns true if the JSON+"\n" line was handed to
//     the OS socket write buffer, false if not currently connected.
//   - `flush(timeoutMs)` (used only from session_shutdown) waits up to
//     timeoutMs for the socket's write buffer to drain (or resolves
//     immediately when not connected).
//   - `close()` clears the reconnect timer and destroys the socket.
//
// Reconnect policy: fixed 5s cadence, never more than one timer in
// flight, never tight-loops. The detach `.catch()` ensures no
// unhandled rejection escapes into headless pi.

import { connect as netConnect, type Socket } from "node:net";
import { unlink } from "node:fs/promises";

/** Default reconnect interval in milliseconds. */
export const DEFAULT_RECONNECT_INTERVAL_MS = 5000;

export interface SocketWriterOptions {
  socketPath: string;
  reconnectIntervalMs?: number;
}

export class SocketWriter {
  readonly #socketPath: string;
  readonly #reconnectIntervalMs: number;

  #socket: Socket | null = null;
  #connected = false;
  /**
   * Whether the underlying socket is currently able to accept writes
   * without backpressuring. Node's `socket.write()` returns `false`
   * when its internal buffer exceeds the stream's `highWaterMark` —
   * meaning the OS/consumer isn't draining fast enough. Ignoring that
   * signal and continuing to write would buffer unbounded amounts of
   * data in-process, which is exactly what the emitter's bounded
   * queue is designed to prevent for a wedged server. We track it
   * explicitly: when `socket.write()` returns `false`, we set
   * `#writable = false` and stop calling `.write()` until the
   * `'drain'` event fires. `send()` then returns `false` to the
   * caller, signaling "this event is lost" so the index.ts drain
   * loop logs it as a drop.
   */
  #writable = true;
  /** Timer handle for a scheduled reconnect (null = none pending). */
  #reconnectTimer: NodeJS.Timeout | null = null;
  /** Resolver for the in-flight flush() promise (null = none). */
  #flushResolver: (() => void) | null = null;
  /** Reusable per-call JSON serializer. */
  readonly #encode: (envelope: unknown) => string;

  constructor(options: SocketWriterOptions) {
    if (typeof options.socketPath !== "string" || options.socketPath.length === 0) {
      throw new TypeError("SocketWriter requires a non-empty socketPath");
    }
    this.#socketPath = options.socketPath;
    this.#reconnectIntervalMs =
      options.reconnectIntervalMs ?? DEFAULT_RECONNECT_INTERVAL_MS;
    this.#encode = (envelope: unknown): string => `${JSON.stringify(envelope)}\n`;
  }

  /** True when the underlying socket is in a writable, connected state. */
  get connected(): boolean {
    return this.#connected;
  }

  /** The configured socket path (read-only). */
  get socketPath(): string {
    return this.#socketPath;
  }

  /**
   * Open the socket. Lazy — never called from the extension factory; the
   * factory constructs the writer, `session_start` calls `connect()`.
   *
   * Idempotent: if already connected or already in the middle of a
   * pending reconnect, this call is a no-op.
   *
   * Never throws synchronously. Any error from `net.connect` is caught
   * by the 'error' listener attached below.
   */
  connect(): void {
    if (this.#connected) return;
    if (this.#reconnectTimer !== null) return;
    this.#openSocket();
  }

  /** Open a fresh socket and wire its listeners. Internal. */
  #openSocket(): void {
    const sock = netConnect(this.#socketPath);
    this.#socket = sock;
    // A fresh socket starts writable until proven otherwise.
    this.#writable = true;

    sock.on("error", () => {
      // Swallow on the socket; #close listener will mark us disconnected
      // and schedule a single reconnect.
    });

    sock.on("connect", () => {
      this.#connected = true;
      this.#writable = true;
    });

    // Node emits 'drain' once the consumer (here: our listener) has
    // pulled enough bytes out of the OS / internal buffer for the
    // socket to be writable again. Register ONCE per socket — repeated
    // .on() calls would stack listeners across reconnects, but since
    // we attach a fresh listener on each #openSocket() and the old
    // socket gets destroyed, that's fine. If the socket is closed
    // while backpressured, the 'close' listener (registered below)
    // resets state without ever needing 'drain'.
    sock.on("drain", () => {
      this.#writable = true;
    });

    sock.on("close", () => {
      this.#connected = false;
      this.#writable = false;
      this.#socket = null;
      // If flush() was awaiting a drain, resolve it now — connection
      // ended, there's nothing more to wait for.
      const r = this.#flushResolver;
      if (r) {
        this.#flushResolver = null;
        r();
      }
      // Schedule exactly one reconnect attempt.
      this.#scheduleReconnect();
    });

    // The 'connect' event may have fired synchronously inside
    // net.connect (rare on Unix domain sockets but possible). If we
    // never received 'connect' and never received 'close' either (still
    // pending), that's fine — listeners will fire later.
  }

  /** Schedule exactly one reconnect attempt. No-op if already pending. */
  #scheduleReconnect(): void {
    if (this.#reconnectTimer !== null) return;
    this.#reconnectTimer = setTimeout(() => {
      this.#reconnectTimer = null;
      // Detached so an error during connect (e.g. ENOENT) doesn't
      // bubble into the agent loop.
      Promise.resolve()
        .then(() => {
          this.#openSocket();
        })
        .catch(() => {
          /* swallow — listener chain will reschedule */
        });
    }, this.#reconnectIntervalMs);
    // Allow the process to exit naturally if nothing else holds it up.
    this.#reconnectTimer.unref?.();
  }

  /**
   * Encode and write one envelope as a newline-delimited JSON line.
   * Returns true iff the data was handed to the live socket's write
   * buffer; false iff we're not currently connected OR the socket is
   * backpressured (caller treats false as "this event is lost" and
   * logs it as a drop).
   *
   * Backpressure handling: `socket.write()` returns `false` when the
   * Node-internal buffer exceeds the stream's `highWaterMark`. We
   * capture that signal: when we see `false`, we flip
   * `#writable = false` and do NOT call `.write()` again until the
   * socket emits `'drain'` (which restores `#writable = true`). This
   * prevents unbounded in-process buffering when the consumer is
   * wedged or too slow — the right behavior for a bounded-queue
   * emitter. While backpressured, `send()` returns `false` WITHOUT
   * calling `.write()`, so the caller (index.ts's drain loop) treats
   * it as a drop and logs it.
   *
   * Always returns synchronously. Never throws.
   */
  send(envelope: unknown): boolean {
    if (!this.#connected || this.#socket === null) return false;
    if (!this.#writable) return false;
    try {
      const line = this.#encode(envelope);
      const accepted = this.#socket.write(line);
      if (!accepted) {
        // Node told us the buffer is over the highWaterMark; stop
        // writing until 'drain' fires (registered in #openSocket).
        // Until then, send() returns false so the caller treats the
        // event as a drop.
        this.#writable = false;
        return false;
      }
      return true;
    } catch {
      // Defensive: if write throws (e.g. socket destroyed mid-call),
      // treat as a send failure. The 'close' listener will mark
      // disconnected and schedule a reconnect.
      this.#writable = false;
      return false;
    }
  }

  /**
   * Best-effort bounded wait for the socket's write buffer to drain.
   *
   * Behavior:
   *  - If not connected → resolve immediately (nothing to flush).
   *  - If connected → wait up to `timeoutMs` for the socket's write
   *    buffer to become empty AND a successful drain microtask to fire.
   *    Resolves early if the socket closes.
   *  - Never hangs past `timeoutMs`.
   *  - Never throws.
   *
   * Used ONLY from the session_shutdown handler in index.ts.
   */
  flush(timeoutMs: number): Promise<void> {
    return new Promise<void>((resolve) => {
      const finish = (): void => {
        this.#flushResolver = null;
        resolve();
      };
      if (!this.#connected || this.#socket === null) {
        resolve();
        return;
      }
      // Two paths to resolution:
      //  (a) Socket's write buffer drains AND a successful drain
      //      microtask runs. We model this as a single rAF-like wait.
      //  (b) The 'close' listener resolves us (via #flushResolver).
      //  (c) The timeout fires.
      this.#flushResolver = () => finish();
      const sock = this.#socket;
      const timeout = setTimeout(() => finish(), timeoutMs);
      timeout.unref?.();
      // Try to drain. If writeBuffer is already empty, this fires
      // essentially immediately; otherwise it fires when the buffer
      // empties.
      try {
        sock.write("", () => {
          if (this.#flushResolver !== null) {
            clearTimeout(timeout);
            finish();
          }
        });
      } catch {
        clearTimeout(timeout);
        finish();
      }
    });
  }

  /**
   * Clear any pending reconnect timer and destroy the socket if still
   * connected. Never throws.
   */
  close(): void {
    if (this.#reconnectTimer !== null) {
      clearTimeout(this.#reconnectTimer);
      this.#reconnectTimer = null;
    }
    const sock = this.#socket;
    this.#socket = null;
    this.#connected = false;
    this.#writable = false;
    if (sock) {
      try {
        sock.destroy();
      } catch {
        /* swallow */
      }
    }
    const r = this.#flushResolver;
    if (r) {
      this.#flushResolver = null;
      // Resolve any pending flush so callers don't hang forever.
      r();
    }
  }

  /**
   * Unlink the underlying socket inode (best-effort). Used by tests for
   * cleanup; not part of the public hot-path contract. Tolerates ENOENT.
   */
  async unlinkSocketFile(): Promise<void> {
    try {
      await unlink(this.#socketPath);
    } catch (err: unknown) {
      const code = (err as NodeJS.ErrnoException | undefined)?.code;
      if (code !== "ENOENT") {
        // Re-throw for visibility; tests can ignore.
        throw err;
      }
    }
  }
}