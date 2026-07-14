// queue.test.ts — Tests for BoundedQueue's drop-oldest behavior and
// drop-marker semantics.

import { describe, it, expect } from "vitest";
import { BoundedQueue } from "./queue.js";

describe("BoundedQueue — construction", () => {
  it("rejects non-positive capacities", () => {
    expect(() => new BoundedQueue({ capacity: 0 })).toThrow(RangeError);
    expect(() => new BoundedQueue({ capacity: -1 })).toThrow(RangeError);
    expect(() => new BoundedQueue({ capacity: 1.5 })).toThrow(RangeError);
  });

  it("reports capacity and starts empty", () => {
    const q = new BoundedQueue<string>({ capacity: 3 });
    expect(q.capacity).toBe(3);
    expect(q.size).toBe(0);
    expect(q.droppedCount).toBe(0);
  });
});

describe("BoundedQueue — basic push/drain", () => {
  it("preserves FIFO order and reports size", () => {
    const q = new BoundedQueue<number>({ capacity: 5 });
    q.push(1);
    q.push(2);
    q.push(3);
    expect(q.size).toBe(3);
    expect(q.drainAll()).toEqual([1, 2, 3]);
    expect(q.size).toBe(0);
    expect(q.drainAll()).toEqual([]);
  });
});

describe("BoundedQueue — overflow drops oldest", () => {
  it("physically removes the oldest item when full (not overwrites a slot)", () => {
    const q = new BoundedQueue<number>({ capacity: 3 });
    q.push(1);
    q.push(2);
    q.push(3);
    // Queue is full. Push one more — oldest (1) must be gone for good.
    q.push(4);
    expect(q.size).toBe(3);
    expect(q.drainAll()).toEqual([2, 3, 4]);

    // Re-fill from empty; confirm 1 does not resurface from a phantom
    // ring slot. With capacity 3 and 5 pushes, the queue ends at the
    // last 3 items — and crucially none of them is the original 1.
    q.push(5);
    q.push(6);
    q.push(7);
    q.push(8);
    q.push(9);
    const final = q.drainAll();
    expect(final).toEqual([7, 8, 9]);
    expect(final).not.toContain(1);
  });

  it("tracks a single contiguous drop streak in droppedCount", () => {
    const q = new BoundedQueue<number>({ capacity: 2 });
    q.push(1);
    q.push(2);
    q.push(3); // drops 1
    q.push(4); // drops 2
    q.push(5); // drops 3
    expect(q.size).toBe(2);
    expect(q.droppedCount).toBe(3);
  });
});

describe("BoundedQueue — drop marker lifecycle", () => {
  it("returns null when no drops have occurred", () => {
    const q = new BoundedQueue<number>({ capacity: 2 });
    q.push(1);
    expect(q.drainDropMarker()).toBeNull();
  });

  it("emits a marker with the count and an ISO 'since' timestamp", () => {
    const q = new BoundedQueue<number>({ capacity: 1 });
    q.push(1);
    q.push(2); // drop
    q.push(3); // drop
    const m = q.drainDropMarker();
    expect(m).not.toBeNull();
    expect(m?.type).toBe("gravity_emitter_drop");
    expect(m?.droppedCount).toBe(2);
    expect(m?.since).toMatch(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/);
    // Marker cleared — next call returns null.
    expect(q.drainDropMarker()).toBeNull();
    expect(q.droppedCount).toBe(0);
  });

  it("starts a fresh streak + timestamp after a marker is drained", async () => {
    const q = new BoundedQueue<number>({ capacity: 1 });
    q.push(1);
    q.push(2); // drop #1
    const first = q.drainDropMarker();
    expect(first?.droppedCount).toBe(1);
    // drainDropMarker() does NOT clear items — drainAll() first so the
    // next streak starts from a known-empty queue.
    q.drainAll();

    // Tiny pause to make timestamps measurably different.
    await new Promise((r) => setTimeout(r, 5));

    q.push(3);
    q.push(4); // drop of a fresh streak
    const second = q.drainDropMarker();
    expect(second?.droppedCount).toBe(1);
    expect(second?.since).not.toBe(first?.since);
  });

  it("typical drain flow: marker first, then items", () => {
    const q = new BoundedQueue<number>({ capacity: 3 });
    q.push(1);
    q.push(2);
    q.push(3);
    q.push(4); // drops 1
    const marker = q.drainDropMarker();
    const items = q.drainAll();
    expect(marker?.droppedCount).toBe(1);
    expect(items).toEqual([2, 3, 4]);
  });
});