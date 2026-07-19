// morph.ts — minimal keyed DOM reconciliation for the browser client.
//
// The renderer builds a fresh subtree per server message; `morphEl`
// folds that subtree into the live DOM instead of swapping it wholesale.
// Nodes are matched by key (`data-key` on transcript cards,
// `data-session-id` on fleet rows) or positionally for unkeyed nodes;
// a matched node that `isEqualNode`s its replacement is left untouched.
// Keeping unchanged nodes alive is what preserves the browser's scroll
// anchor and <details> toggle state across updates — the whole point of
// this module. All event handling is delegated at the document level
// (see index.ts), so nodes carry no listeners and can be kept or
// replaced freely.

export function morphEl(live: HTMLElement, next: HTMLElement): void {
  syncAttrs(live, next);
  morphChildren(live, next);
}

function keyOf(node: Node): string | null {
  if (!(node instanceof HTMLElement)) return null;
  return node.dataset.key ?? node.dataset.sessionId ?? null;
}

function sameType(a: Node, b: Node): boolean {
  if (a.nodeType !== b.nodeType) return false;
  if (a instanceof Element && b instanceof Element) return a.tagName === b.tagName;
  return true;
}

function syncAttrs(live: Element, next: Element): void {
  for (const attr of Array.from(next.attributes)) {
    if (live.getAttribute(attr.name) !== attr.value) {
      live.setAttribute(attr.name, attr.value);
    }
  }
  for (const attr of Array.from(live.attributes)) {
    if (!next.hasAttribute(attr.name)) live.removeAttribute(attr.name);
  }
}

function patchNode(live: Node, next: Node): void {
  if (live.nodeType === Node.TEXT_NODE) {
    if (live.nodeValue !== next.nodeValue) live.nodeValue = next.nodeValue;
    return;
  }
  if (live instanceof HTMLElement && next instanceof HTMLElement) {
    if (live.isEqualNode(next)) return;
    syncAttrs(live, next);
    morphChildren(live, next);
  }
}

/**
 * Reconcile `live`'s children against `next`'s. Keyed children are
 * matched anywhere in the old list (and moved into place); unkeyed
 * children match positionally when node type and tag agree. Matched
 * nodes are patched recursively, unmatched new nodes are adopted from
 * `next`, and old nodes that nothing matched are removed.
 */
function morphChildren(live: HTMLElement, next: HTMLElement): void {
  const liveKeyed = new Map<string, HTMLElement>();
  for (const child of Array.from(live.children)) {
    const k = keyOf(child);
    if (k) liveKeyed.set(k, child as HTMLElement);
  }

  const wantedList = Array.from(next.childNodes);
  let ptr: Node | null = live.firstChild;
  for (const wanted of wantedList) {
    const k = keyOf(wanted);
    const keyedMatch = k ? liveKeyed.get(k) ?? null : null;
    const match = keyedMatch && sameType(keyedMatch, wanted)
      ? keyedMatch
      : !k && ptr && keyOf(ptr) == null && sameType(ptr, wanted)
        ? ptr
        : null;
    if (match) {
      if (match === ptr) {
        ptr = ptr.nextSibling;
      } else {
        live.insertBefore(match, ptr);
      }
      patchNode(match, wanted);
    } else {
      // Adopting `wanted` removes it from `next`; `wantedList` is a
      // snapshot, so iteration is unaffected.
      live.insertBefore(wanted, ptr);
    }
  }

  // Everything from `ptr` on was never matched or moved — stale nodes.
  while (ptr) {
    const nextSibling: Node | null = ptr.nextSibling;
    live.removeChild(ptr);
    ptr = nextSibling;
  }
}
