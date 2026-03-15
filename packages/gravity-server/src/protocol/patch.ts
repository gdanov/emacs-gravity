// patch.ts — Patch emission helpers

import type { Patch } from "@gravity/shared";

/** Collect patches during a state mutation, then emit as a batch. */
export class PatchCollector {
  private patches: Patch[] = [];

  add(patch: Patch): void {
    this.patches.push(patch);
  }

  /** Drain all collected patches and return them. */
  drain(): Patch[] {
    const result = this.patches;
    this.patches = [];
    return result;
  }

  get length(): number {
    return this.patches.length;
  }
}
