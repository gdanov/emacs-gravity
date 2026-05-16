# Pi Interaction Inventory — Implementation Plan

Gap analysis of pi's full RPC + extension-UI surface (`@mariozechner/pi-coding-agent/docs/rpc.md`) against what emacs-gravity's pi-driver actually wires. Tiers are ordered by whether the item is *required* (correctness) vs. *optional* (enhancement). Each item lists the wire contract, touch points, effort, and acceptance criteria.

## Current coverage (baseline)

Implemented stdin RPCs (12): `prompt`, `steer`, `abort`, `new_session`, `get_state`, `set_model`, `set_thinking_level`, `compact`, `get_session_stats`, `switch_session`, `get_commands`, `extension_ui_response`. Event/stdout side is essentially complete (turn/tool/agent/message/compaction/retry/extension_error/extension_ui_request all translated).

Extension-UI bridge: `confirm` → permission inbox, `select` → question inbox. `input`/`editor` auto-cancelled. Fire-and-forget (`notify`/`setStatus`/`setWidget`/`setTitle`/`set_editor_text`) logged server-side only.

---

## Tier 1 — Required (correctness regression)

### T1.1 — `input` / `editor` extension-UI dialogs → text-entry surface

**Problem.** `gravity-server.ts` (`handlePiExtensionUIRequest`, the `case "input": case "editor":` branch) replies `{cancelled:true}` immediately. Any pi extension calling `ctx.ui.input` / `ctx.ui.editor` (free-text: commit message, clarifying answer, rename, path) has its flow aborted or silently degraded under gravity. A whole category of pi extensions is non-functional.

**Why now.** The historical blocker was "no text-entry surface in Emacs". That surface now exists — the pi compose buffer (`claude-gravity-compose-prompt`, `backend = 'pi'`). The blocker is gone.

**Wire contract** (pi `docs/rpc.md` "Extension UI Protocol"):

- Request (stdout): `{type:"extension_ui_request", id, method:"input"|"editor", title?, message?, placeholder?, prefill?, timeout?}`.
- Response (stdin): value response `{type:"extension_ui_response", id, value:<string>}`; cancellation `{type:"extension_ui_response", id, cancelled:true}`.
- `input` = single-line; `editor` = multi-line. `prefill` seeds the buffer; `placeholder` is hint text.

**Design.** Route `input`/`editor` like `confirm`/`select` already are — into the inbox so any connected terminal can answer — but the action is text entry, not allow/deny or pick-one.

- Server (`gravity-server.ts`): add an inbox item type for text input (or reuse `question` with a `pi_ui.method` discriminator carrying `prefill`/`placeholder`/`title`/`message`). Track in `pendingPiUIResponses` keyed by inbox id → `{sessionId, piRequestId, method}` (already the pattern). On terminal response, `sendResponse({value})` or `{cancelled:true}`.
- Shared types: extend the terminal→server action union with a text-answer message (e.g. `action.pi-input { itemId, value | cancelled }`), or reuse `action.question` if the existing answers-array shape fits (single element = value).
- Emacs (`claude-gravity-actions.el` + reuse compose machinery): on the new inbox item, open a compose-style buffer pre-seeded with `prefill`, header showing `title`/`message`, `C-c C-c` → send value, `C-c C-k` → cancel. `editor` = full buffer; `input` = same buffer but single logical line (still fine to reuse).
- Lifecycle: pi exit must drop pending input dialogs (the `onLifecycle "stop"/"error"` path already iterates `pendingPiUIResponses` by `sessionId` — extend it to cover the new type).

**Touch points.** `packages/gravity-server/src/gravity-server.ts` (extension-UI handler + pi-exit cleanup), `packages/shared/src/types.ts` (action message), `packages/gravity-server/src/protocol/messages.ts` (allowlist), `claude-gravity-actions.el` (inbox render + action), reuse `claude-gravity-tmux.el` compose buffer.

**Effort.** Medium (≈ a day). Mostly inbox plumbing; the text surface is reuse.

**Acceptance.**
- A pi extension calling `ctx.ui.input({title,prefill})` produces an inbox item; answering it returns `{value}` to pi and the extension proceeds.
- `ctx.ui.editor` multi-line content round-trips intact (newlines preserved).
- Cancelling (C-c C-k / killing the buffer) sends `{cancelled:true}`; the extension takes its default path.
- pi process exit removes any pending input dialog from the inbox on all terminals.
- ERT: inbox item creation + value/cancel response shaping. Vitest: server response payload for value vs cancel.

---

## Tier 2 — High value, low effort

### T2.1 — `notify` → Emacs notice

**Problem.** Fire-and-forget `notify` (`{notifyType:"info"|"warning"|"error", message}`) is server-log-only (`gravity-server.ts` fire-and-forget branch). Users never see pi-extension notifications.

**Design.** In the fire-and-forget branch, for `method === "notify"` broadcast the existing `notice` server message (`ServerPushMessage` `{type:"notice", level, text}`) mapping `notifyType` → level. Emacs already has a notice handler (`claude-gravity--handle-notice`). No new protocol.

**Touch points.** `gravity-server.ts` (one branch), no shared-type change (notice already exists), no Emacs change if the notice handler already surfaces text.

**Effort.** Trivial (≈ 1 hour).

**Acceptance.** A pi extension `ctx.ui.notify("done","info")` shows as a gravity notice; `error` maps to error level. No response sent (fire-and-forget).

### T2.2 — `set_session_name` + `get_state.sessionName`

**Problem.** Pi sessions render as opaque `pi-mp8na1e3-…` slugs in the overview. No way to label them; pi's own `sessionName` is never read back.

**Design.**
- Driver: add `setSessionName(name)` (stdin `{type:"set_session_name", name}`) and read `sessionName` from the existing `get_state` response (already fetched in `captureSessionFile`).
- Server: on `get_state`, if `sessionName` present, `updateMeta(session,{displayName})` → existing `set_meta` patch path. Add `pi.set-session-name` terminal action → driver call.
- Emacs: `claude-gravity--pi-set-session-name` interactive (read-string), send action. Optionally bind under `S`.

**Touch points.** `pi-driver/{types,protocol,spawn,mod}.ts`, `gravity-server.ts` (get_state handler + action), `protocol/messages.ts`, `claude-gravity-client.el`.

**Effort.** Small (≈ half day).

**Acceptance.** Setting a name updates the overview label live (via `set_meta`); restarting/reconnecting still shows it (pi persisted it, `get_state` reads it back).

### T2.3 — `get_available_models` backing the model picker

**Problem.** `claude-gravity-set-model` requires hand-typed provider + modelId — guess-the-string footgun.

**Design.**
- Driver: `getAvailableModels()` (stdin `{type:"get_available_models"}` → `data.models: Model[]` per `docs/rpc.md` "Model" type: `{provider, modelId, name, …}`).
- Server: cache on session (mirror `set_pi_commands` pattern: `set_pi_models` patch + `session.piModels`), or fetch on demand via a `pi.get-models` request/response. Cache-on-session is consistent with the command-inventory design and survives reconnects.
- Emacs: `claude-gravity-set-model` for pi → `completing-read` over `:pi-models` (label `provider/modelId — name`), send `pi.set-model`.

**Touch points.** Same set as `get_commands` (it is the same pattern — types, spawn, mod, session.ts mutation, patch op, client patch handler, Emacs picker).

**Effort.** Small–medium (≈ half–one day) — largely mechanical, mirrors the just-shipped `set_pi_commands`.

**Acceptance.** Model picker lists pi's actual models; selecting one calls `set_model`; list refreshes on session start and survives a client reconnect.

---

## Tier 3 — Optional, defer unless needed

### T3.1 — `fork` / `get_fork_messages`

Pi's branch-a-session workflow (its `/fork`, double-Esc `fork` action). gravity has `switch_session`/resume but no fork. Needs a UI concept (fork-from-here on a turn? new session entry?). Wire: `{type:"fork", …}` → new session id; `get_fork_messages` for the forked transcript. Medium effort, mostly UX design. Defer unless forking is a routine workflow.

### T3.2 — `cycle_model` / `cycle_thinking_level`

Pi's Ctrl+P ergonomics — cycle through `enabledModels` / thinking levels without naming one. Minor convenience over the explicit setters already wired. Small effort; low marginal value given T2.3 gives a real picker.

### T3.3 — `follow_up`

Distinct from `steer`: `steer` interrupts mid-run (drops remaining tools); `follow_up` delivers after the agent finishes. Compose buffer could offer both via prefix arg or a separate key. Wire: `{type:"follow_up", message, images?}`. Small effort; mild value.

### T3.4 — `setStatus` / `setWidget` / `setTitle` (fire-and-forget)

Richer than `notify`: persistent status key/text, multi-line widget above/below editor, window title. Only worth it if pi extensions you actually run rely on them. Would need a per-session status/widget surface in the session buffer. Medium effort; speculative until a concrete extension needs it.

---

## Explicitly skipped (redundant or wrong layer)

- `get_messages` / `get_last_assistant_text` — gravity already reconstructs the full turn tree from the event stream; raw message fetch is redundant for the UI.
- `set_auto_compaction` / `set_auto_retry` / `abort_retry` — config toggles; the auto behaviors already work and surface via events. Belongs in pi `settings.json`, not a gravity command. (`abort_retry` is minor and rare.)
- `export_html` — pi can do this standalone; not core to the gravity UI.
- `bash` / `abort_bash` — pi user-bash (`!cmd`); no gravity surface concept, niche.
- `set_steering_mode` / `set_follow_up_mode` — queue-mode config; default is fine, niche.

---

## Suggested sequencing

1. **T1.1** (required — closes the extension correctness regression; reuses the compose surface).
2. **T2.1** (trivial, immediate visibility win — bundle with T1.1).
3. **T2.2**, **T2.3** (small, mechanical; T2.3 mirrors the shipped `set_pi_commands` pattern).
4. Re-evaluate Tier 3 against actual pi-extension usage; implement on demand only.
