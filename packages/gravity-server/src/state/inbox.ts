// inbox.ts — Inbox manager for bidirectional flows
//
// Tracks pending responses for PermissionRequest, AskUserQuestion, and plan review.
// Maps inbox items to the hook socket connection that's waiting for a response.

import type { InboxItem, InboxItemType, HookSocketResponse } from "@gravity/shared";
import type { Socket } from "net";

export interface PendingResponse {
  inboxItem: InboxItem;
  hookSocket: Socket;
}

export class InboxManager {
  private counter = 0;
  private items: InboxItem[] = [];
  private pending = new Map<number, PendingResponse>();

  /** Add an inbox item. Returns the new item. */
  add(
    type: InboxItemType,
    sessionId: string,
    project: string | null,
    label: string,
    summary: string,
    data: Record<string, unknown>,
    hookSocket?: Socket,
  ): InboxItem {
    this.counter++;
    const item: InboxItem = {
      id: this.counter,
      type,
      sessionId,
      project,
      label,
      timestamp: Date.now(),
      summary,
      data,
    };
    this.items.unshift(item);

    if (hookSocket) {
      this.pending.set(item.id, { inboxItem: item, hookSocket });
    }

    return item;
  }

  /** Remove an inbox item by ID. */
  remove(id: number): InboxItem | undefined {
    const idx = this.items.findIndex((i) => i.id === id);
    if (idx === -1) return undefined;
    const [removed] = this.items.splice(idx, 1);
    this.pending.delete(id);
    return removed;
  }

  /** Remove all items for a session, optionally filtered by type. */
  removeForSession(sessionId: string, type?: InboxItemType): InboxItem[] {
    const removed: InboxItem[] = [];
    this.items = this.items.filter((item) => {
      if (item.sessionId === sessionId && (!type || item.type === type)) {
        removed.push(item);
        this.pending.delete(item.id);
        return false;
      }
      return true;
    });
    return removed;
  }

  /** Find an inbox item by ID. */
  find(id: number): InboxItem | undefined {
    return this.items.find((i) => i.id === id);
  }

  /** Get the pending response for an inbox item. */
  getPending(id: number): PendingResponse | undefined {
    return this.pending.get(id);
  }

  /** Respond to a pending item: write response to hook socket, remove from pending. */
  respond(id: number, response: HookSocketResponse): boolean {
    const pending = this.pending.get(id);
    if (!pending) return false;

    try {
      pending.hookSocket.write(JSON.stringify(response) + "\n");
      pending.hookSocket.end();
    } catch {
      // Socket may have closed — ignore
    }

    this.pending.delete(id);
    this.remove(id);
    return true;
  }

  /** All current items. */
  all(): InboxItem[] {
    return this.items;
  }
}
