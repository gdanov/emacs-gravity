# Plan: Fix AskUserQuestion supersede regression + remove push terminal communication

> Status: **in progress** (started 2026-05-19, branch `fix/pi-driver`)
>
> Done: Phase 0 (scoping; tool_use_id identity confirmed structurally for
> AskUserQuestion, deferred-to-test for ExitPlanMode), Phase 1.1–1.3
> (forceCloseStaleForSession preserveToolUseId guard + supersede wiring +
> handlePermissionRequest skip), Phase 3.2 (preserve-contract tests).
> 191/191 gravity-server tests green, tsc clean. **Not committed** (awaiting
> explicit request per source-control rule).
>
> Done (uncommitted): Phase 3.1 — extracted `processHookMessage` to a
> module-level DI function + entrypoint guard (`isEntrypoint`) so the
> module is importable without starting sockets. Phase 3.3 — socket-free
> harness (`test/hook-message-harness.test.ts`): FakeTerminals + simulated
> poll; AskUserQuestion liveness invariant (both hook orderings),
> supersede-still-works, ExitPlanMode guard tests (Phase 0.2 open item
> documented in-test). 197/197 vitest, tsc clean, bundle builds.
>
> Behaviour preservation: extraction was a verbatim move (closure→deps.*);
> bracketed by the full unit suite (191→197) + tsc. True before/after
> characterization impossible (zero prior coverage of this path) — locked
> in going forward by the new harness instead.
>
> Next: **STOP — ask before Phase 2** (push removal, breaking).

## Objective

1. **Root cause fix:** stop the generic `PreToolUse` from force-closing the
   concurrent `AskUserQuestionIntercept` question item (same tool invocation).
   Generalizes to ExitPlanMode plan-review.
2. **Remove push terminal communication:** pull becomes the sole delivery
   path; client polls *immediately* on an inbox signal.
3. Test infrastructure that makes both safe and covers the only remaining
   delivery path.

## Root cause (established)

For one `AskUserQuestion`, three hooks fire (all original since `a9c7e4c`):

1. `AskUserQuestionIntercept` (PreToolUse matcher=`AskUserQuestion`,
   bidirectional) → creates `question` inbox item + pending socket.
2. generic `PreToolUse` (matcher=`''`, not bidirectional) → supersede block →
   `forceCloseStaleForSession()` → **force-closes the question item** (writes
   `{}` to intercept socket).
3. `PermissionRequest` (matcher=`''`, bidirectional) → creates `permission`
   item → survives → Emacs renders it via `json-encode` fallback.

Push masked this (item delivered synchronously in the ~6ms window). The
**pull-mode default flip `dd4c850` (2026-05-06)** exposed it: a contentless
signal + later poll never catches a 6ms-lived item.

`removeStaleForSession` already preserves pending items
(`inbox.ts:101`). Only `forceCloseStaleForSession` kills the pending
question item. Minimal correct guard: do not force-close a pending item
whose `data.tool_use_id` equals the triggering event's `tool_use_id` — the
generic `PreToolUse` and the intercept are the *same* tool invocation, not a
stale prior interaction. Also protects ExitPlanMode plan-review.

## Phases

### Phase 0 — Scoping & safety net
- 0.1 Two separate commits (fix; push-removal).
- 0.2 Verify `tool_use_id` present + identical across sibling hooks.
- 0.3 Document current symptom repro.

### Phase 1 — Root-cause fix (server, shippable alone)
- 1.1 `forceCloseStaleForSession(sessionId, preserveToolUseId?)` — skip item
  whose `data.tool_use_id === preserveToolUseId`. Undefined → unchanged.
- 1.2 Supersede block passes incoming `tool_use_id`.
- 1.3 Keep `handlePermissionRequest` AskUserQuestion skip+unblock (done).
- 1.4 ExitPlanMode plan-review now also preserved — intended; callout + test.

### Phase 2 — Remove push terminal communication
- 2.1 Audit every `terminals.broadcast(`; classify push-only vs pull.
- 2.2 Delete `PULL_MODE`/`GRAVITY_PUSH_MODE` + push branches; trim shared
  protocol types.
- 2.3 Emacs client: pull-only; **inbox signal → immediate poll** (bypass idle
  debounce).
- 2.4 Docs (ARCHITECTURE, refactor-implementation, README, MEMORY).

### Phase 3 — Test infrastructure
- 3.1 Socket-free `processHookMessage` harness + `FakeTerminals` + simulated
  poll. Characterization test brackets the extraction.
- 3.2 Invert the `forceCloseStaleForSession` test (preserve same
  tool_use_id; still reap different; undefined → full reap).
- 3.3 Property/liveness test: both hook orderings × poll points ⇒ exactly one
  renderable question item, never permission, intercept socket still pending;
  ExitPlanMode variant.
- 3.4 Pull state-machine: seq monotonic/no-gap; coalesced signal keeps net
  change; create+remove-between-polls contract.
- 3.5 Client ERT driven by recorded server *signals* (not synthetic items);
  asserts immediate poll on inbox signal + option UI opens.
- 3.6 Revive `integration-scenarios` in-sandbox (`$TMPDIR`/loopback sockets).

### Phase 4 — Verify & land
Tests green, byte-compile clean, manual repro, memory update, Conventional
Commits (`fix(server): …`; `refactor(server,emacs)!: remove push …`), PR.

## Acceptance criteria
- CC-hook AskUserQuestion → Emacs option UI within ~one poll, both orderings.
- `forceCloseStaleForSession` preserves same-`tool_use_id` pending item;
  still supersedes genuinely stale prior interactions.
- ExitPlanMode + permission flows unaffected/improved (tested).
- No `PULL_MODE`/`GRAVITY_PUSH_MODE`/dead push code.
- `handleHookMessage` reachable by in-sandbox tests; integration revived.
- Inverted supersede test fails pre-fix, passes post-fix.
