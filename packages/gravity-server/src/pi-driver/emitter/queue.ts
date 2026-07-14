// queue.ts — Bounded FIFO queue with drop-oldest overflow handling.
//
// Why this exists: per the spec, every pi.on(...) handler is synchronous
// (no `await`, no socket I/O). Handlers push envelopes onto this queue
// and return; a detached drain loop flushes them to the socket. When the
// socket is wedged or the server is offline, the queue fills up — and we
// MUST drop OLDEST items (most recent data is most useful for live
// status), so a 750-slot cap and physical removal (not just slot
// overwrite) is the policy here.
//
// On every drop we track:
//   - droppedCount: how many were evicted since the last drain
//   - since: ISO timestamp of the FIRST drop in this contiguous streak
// On the next successful drain, a single DropMarker is emitted ahead of
// the rest of the drained items (so the server sees "N items dropped
// since <ts>" once, not N times). The marker is reset to null once it
// is drained — meaning a fresh drop streak starts a new marker.

/** Sentinel item the queue inserts ahead of a drained batch when drops
 *  occurred. The server's translator does not recognize this type and
 *  logs-and-ignores it; the emitter also writes each drop to its own
 *  log file (best-effort + reliable dual record). */
export interface DropMarker {
  type: "gravity_emitter_drop";
  droppedCount: number;
  /** ISO-8601 timestamp of the first drop in this streak. */
  since: string;
}

export interface BoundedQueueOptions {
  capacity: number;
}

/** A FIFO queue with a fixed capacity. Overflow drops the oldest item. */
export class BoundedQueue<T> {
  readonly #capacity: number;
  #items: T[] = [];
  /** Count of items dropped since the last marker was drained. */
  #droppedCount = 0;
  /** ISO timestamp of the first drop in the current streak. */
  #since: string | null = null;

  constructor(options: BoundedQueueOptions) {
    if (!Number.isInteger(options.capacity) || options.capacity <= 0) {
      throw new RangeError(
        `BoundedQueue capacity must be a positive integer, got ${options.capacity}`,
      );
    }
    this.#capacity = options.capacity;
  }

  /**
   * Push one item. On overflow, the OLDEST item is physically removed
   * (Array.prototype.shift, not just an index overwrite) so the queue
   * truly forgets the dropped data — the drain loop never sees stale
   * payloads surfacing from a wraparound slot.
   *
   * Drop accounting: every drop increments #droppedCount and (if no
   * streak is in flight) stamps #since to the current time. The marker
   * is reset to null the moment `drainDropMarker()` returns it; a fresh
   * drop streak starts a new one.
   */
  push(item: T): void {
    if (this.#items.length >= this.#capacity) {
      this.#items.shift();
      this.#droppedCount += 1;
      if (this.#since === null) {
        this.#since = new Date().toISOString();
      }
    }
    this.#items.push(item);
  }

  /**
   * Return and CLEAR the pending drop marker, or null if no drops are
   * pending. Called by the drain loop ahead of `drainAll()` so the
   * server sees at most one drop marker per drop streak.
   */
  drainDropMarker(): DropMarker | null {
    if (this.#droppedCount === 0 || this.#since === null) return null;
    const marker: DropMarker = {
      type: "gravity_emitter_drop",
      droppedCount: this.#droppedCount,
      since: this.#since,
    };
    this.#droppedCount = 0;
    this.#since = null;
    return marker;
  }

  /**
   * Remove and return everything currently queued in FIFO order. After
   * this call the queue is empty. Does NOT touch drop accounting — call
   * `drainDropMarker()` first if you want the marker in the same batch.
   */
  drainAll(): T[] {
    if (this.#items.length === 0) return [];
    const out = this.#items;
    this.#items = [];
    return out;
  }

  /** Current queue length (NOT including any pending drop marker). */
  get size(): number {
    return this.#items.length;
  }

  /** Configured capacity. Read-only. */
  get capacity(): number {
    return this.#capacity;
  }

  /** Total drops that have occurred since the last drain (for tests). */
  get droppedCount(): number {
    return this.#droppedCount;
  }
}