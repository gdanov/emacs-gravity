// inbox.ts — Inbox service for bidirectional flows
//
// Replaces state/inbox.ts class with a ServiceMap service.
// Tracks pending responses for PermissionRequest, AskUserQuestion, and plan review.

import { Effect, Layer, ServiceMap } from "effect";
import type { InboxItem, InboxItemType, HookSocketResponse } from "@gravity/shared";
import type { Socket } from "net";

export interface PendingResponse {
  readonly inboxItem: InboxItem;
  readonly hookSocket: Socket;
}

export interface InboxService {
  readonly add: (
    type: InboxItemType,
    sessionId: string,
    project: string | null,
    label: string,
    summary: string,
    data: Record<string, unknown>,
    hookSocket?: Socket,
  ) => InboxItem;
  readonly remove: (id: number) => InboxItem | undefined;
  readonly removeForSession: (sessionId: string, type?: InboxItemType) => InboxItem[];
  readonly removeStaleForSession: (sessionId: string, type?: InboxItemType) => InboxItem[];
  readonly forceCloseStaleForSession: (
    sessionId: string,
    preserveToolUseId?: string,
  ) => InboxItem[];
  readonly find: (id: number) => InboxItem | undefined;
  readonly getPending: (id: number) => PendingResponse | undefined;
  readonly respond: (id: number, response: HookSocketResponse) => Effect.Effect<boolean>;
  readonly removeBySocket: (socket: Socket) => InboxItem[];
  readonly all: () => InboxItem[];
}

export const Inbox = ServiceMap.Service<InboxService>("Inbox");

export function makeInbox(): InboxService {
  let counter = 0;
  const items: InboxItem[] = [];
  const pending = new Map<number, PendingResponse>();

  const closePendingSocket = (itemId: number): void => {
    const p = pending.get(itemId);
    if (!p) return;
    try {
      p.hookSocket.write(JSON.stringify({}) + "\n");
      p.hookSocket.end();
    } catch { /* socket may already be closed */ }
    pending.delete(itemId);
  };

  const removeItem = (id: number): InboxItem | undefined => {
    const idx = items.findIndex((i) => i.id === id);
    if (idx === -1) return undefined;
    const [removed] = items.splice(idx, 1);
    pending.delete(id);
    return removed;
  };

  return {
    add: (type, sessionId, project, label, summary, data, hookSocket) => {
      counter++;
      const item: InboxItem = {
        id: counter,
        type,
        sessionId,
        project,
        label,
        timestamp: Date.now(),
        summary,
        data,
      };
      items.unshift(item);
      if (hookSocket) {
        pending.set(item.id, { inboxItem: item, hookSocket });
      }
      return item;
    },

    remove: removeItem,

    removeForSession: (sessionId, type?) => {
      const removed: InboxItem[] = [];
      for (let i = items.length - 1; i >= 0; i--) {
        const item = items[i];
        if (item.sessionId === sessionId && (!type || item.type === type)) {
          items.splice(i, 1);
          pending.delete(item.id);
          removed.push(item);
        }
      }
      return removed;
    },

    removeStaleForSession: (sessionId, type?) => {
      const removed: InboxItem[] = [];
      for (let i = items.length - 1; i >= 0; i--) {
        const item = items[i];
        if (item.sessionId === sessionId && (!type || item.type === type)) {
          if (pending.has(item.id)) continue;
          items.splice(i, 1);
          removed.push(item);
        }
      }
      return removed;
    },

    // `preserveToolUseId`: when set, an item whose hook payload carries the
    // same `tool_use_id` is NOT closed. The generic PreToolUse hook and a
    // sibling bidirectional hook (AskUserQuestionIntercept, or the
    // PermissionRequest behind ExitPlanMode) belong to the SAME tool
    // invocation — superseding here would force-close the live question /
    // plan-review item microseconds after it was created. Supersede must
    // only reap items from PRIOR, different tool invocations.
    forceCloseStaleForSession: (sessionId, preserveToolUseId) => {
      const removed: InboxItem[] = [];
      for (let i = items.length - 1; i >= 0; i--) {
        const item = items[i];
        if (item.sessionId !== sessionId) continue;
        if (
          preserveToolUseId !== undefined &&
          (item.data as Record<string, unknown> | undefined)?.tool_use_id ===
            preserveToolUseId
        ) {
          continue;
        }
        closePendingSocket(item.id);
        items.splice(i, 1);
        removed.push(item);
      }
      return removed;
    },

    find: (id) => items.find((i) => i.id === id),

    getPending: (id) => pending.get(id),

    respond: (id, response) =>
      Effect.sync(() => {
        const p = pending.get(id);
        if (!p) return false;
        try {
          p.hookSocket.write(JSON.stringify(response) + "\n");
          p.hookSocket.end();
        } catch { /* socket may already be closed */ }
        pending.delete(id);
        removeItem(id);
        return true;
      }),

    removeBySocket: (socket) => {
      const removed: InboxItem[] = [];
      for (const [id, p] of pending) {
        if (p.hookSocket === socket) {
          const item = removeItem(id);
          if (item) removed.push(item);
        }
      }
      return removed;
    },

    all: () => [...items],
  };
}

export const InboxLive = Layer.succeed(Inbox, makeInbox());

export const InboxTest = () => Layer.succeed(Inbox, makeInbox());
