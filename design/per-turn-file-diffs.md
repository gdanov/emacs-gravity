# Design: Per-Turn Edited-Files Summary with Consolidated Diff

## Goal

Each turn must show a **summary of files edited during that turn**, and for every
file a **single diff** — even when the file was edited multiple times in the
turn. The diff is the net change `baseline → final`, not a stack of per-edit
diffs.

Example target UI inside a turn:

```
  Tasks (1/2)
    [x] ...

  Edited Files (2)  +131 −42
    claude-gravity-ui.el       +88 −20   3 edits
    packages/.../session.ts    +43 −22   1 edit
```

Expanding a file entry shows the consolidated unified diff.

## Why this is non-trivial

A file edited 3 times in a turn produces 3 separate tool results. Each
`Edit`/`Write` PostToolUse carries a `structuredPatch` computed against a
*different* file version (V0→V1, V1→V2, V2→V3). Naively stacking them shows the
same region three times with shifting line numbers. We need one diff `V0→V3`.

The only robust way to produce `V0→V3` is to have the file's **full content at
the turn boundary** (`baseline`) and **after the last edit** (`final`), then
diff those two strings. The hard part is obtaining `baseline` without a race.

## Current state (what exists today)

| Concern | Today |
|---|---|
| File tracking | `trackFile()` (server `session.ts`) emits `track_file` patches; `Session.files` is a **session-level** `Record<path, {ops, lastTouched}>`. **No per-turn association.** |
| Per-tool diff | Emacs `claude-gravity--insert-edit-diff` renders one Edit's diff inline in the tool detail, from `result.structuredPatch` (camelCase) or `old_string`/`new_string`. |
| Diff renderer | `claude-gravity--insert-structured-patch` (diff.el) renders a hunk vector `[{oldStart,oldLines,newStart,newLines,lines}]` with word-level refinement. **Reusable as-is.** |
| Session Files section | `claude-gravity-insert-files` (render.el ~919) — session-level list, no diffs. |
| Server diff libs | None. `diff` npm package not a dependency. |

### Verified data shape (from real transcripts)

`Edit` PostToolUse `tool_response`:
```
{ filePath, oldString, newString, originalFile, structuredPatch, userModified, replaceAll }
```
`Write` PostToolUse `tool_response`:
```
{ type: "create"|"update", filePath, content, structuredPatch, originalFile, userModified }
```

Key facts:
- `structuredPatch` is **present and well-formed** for `Edit` and for `Write`
  updates; `[]` for `Write` create.
- `originalFile` is **`null` in practice** — do **not** rely on it for baseline.
- `structuredPatch` hunk shape == jsdiff's `structuredPatch()` output ==
  the shape Emacs `--insert-structured-patch` already renders.

## Core approach: reconstruct baseline by reverse-applying the first edit

We never need to touch the bridge or make `PreToolUse` bidirectional. All work
happens **server-side**, in `PostToolUse` (the edit is already settled — no
race on the edited file itself).

For each `(turn, path)` pair, the server maintains scratch state:

```
FileDiffScratch {
  baseline: string   // file content immediately before the turn's FIRST edit
  current:  string   // file content after the most recent edit
}
```

On `PostToolUse` for an edit-class tool (`Edit`, `Write`, `MultiEdit`,
`NotebookEdit`) that succeeded:

1. **Read the file from disk** → `current` (exact; the edit is done).
2. If this `(turn, path)` is new (first edit of the file *in this turn*):
   - Reconstruct `baseline` by **reverse-applying this tool's
     `structuredPatch`** to `current`. Reverse-apply is exact for well-formed
     patches (swap `+`/`-`, swap old/new coords, apply).
   - `Write` create: `baseline = ""`.
3. Else: just update `current` (baseline is immutable — it is V0).
4. Compute the consolidated diff `structuredPatch(baseline, current)`.
5. Emit an `update_turn_file` patch carrying the full `FileDiff`.

Because `baseline` is captured once and `current` is re-read from disk on every
edit, the approach is **self-healing**: it handles N edits, `MultiEdit`,
`replace_all`, external modifications between edits, and mixed
`Edit`/`Write` sequences uniformly. Only the *first* edit per file per turn
needs a usable `structuredPatch`.

### Why not the alternatives

- **Bridge reads file at `PreToolUse`.** Reliable baseline, but adds file I/O
  to the "thin shim", and `PreToolUse` is fire-and-forget — the bridge exits
  before reading completes unless we make it block. Rejected: violates the
  thin-bridge principle for no gain over reverse-apply.
- **Compose the N per-edit `structuredPatch`es.** Patch composition with
  line-offset arithmetic across overlapping hunks is error-prone. Rejected:
  reverse-apply + re-diff is exact and simpler.
- **Client-side computation in Emacs.** Violates "server owns state, terminals
  are dumb". Would duplicate reverse-apply logic per client (Emacs, menubar,
  future web). Rejected.

## Data model changes

### `packages/shared/src/types.ts`

New interface:
```typescript
/** One file's net change within a single turn. */
export interface FileDiff {
  path: string;
  /** Tool ops applied this turn, in order, e.g. ["edit","edit","write"]. */
  ops: string[];
  editCount: number;
  status: "created" | "modified" | "deleted";
  added: number;    // lines added across the consolidated diff
  removed: number;  // lines removed
  /**
   * Consolidated baseline→final diff in structuredPatch hunk format.
   * null when the diff could not be computed (see Fallbacks) or was
   * dropped for size — terminals then render the entry path-only.
   */
  hunks: StructuredPatchHunk[] | null;
  /** True when `hunks` was elided because the diff exceeded the size cap. */
  truncated: boolean;
}

export interface StructuredPatchHunk {
  oldStart: number; oldLines: number;
  newStart: number; newLines: number;
  lines: string[];  // each prefixed " " | "-" | "+"
}
```

Extend `TurnNode`:
```typescript
export interface TurnNode {
  // ...existing...
  editedFiles: FileDiff[];   // NEW — populated incrementally
}
```

New `Patch` variant:
```typescript
| { op: "update_turn_file"; turnNumber: number; file: FileDiff }
```
`update_turn_file` has **upsert** semantics: replace the `FileDiff` in
`turn.editedFiles` whose `path` matches `file.path`, else append. One patch per
edit; idempotent; safe to re-apply on resync.

## Server implementation

### New module: `packages/gravity-server/src/enrichment/file-diff.ts`

Owns all consolidated-diff logic. Effect-style, pure where possible.

- **Scratch store** — module-level `Map<sessionId, Map<turnKey, FileDiffScratch>>`,
  `turnKey = `${turnNumber}\0${path}``. **Not** part of `Session`, so it never
  enters `session.snapshot`. Cleared on `SessionEnd`.
- `recordEdit(sessionId, turnNumber, toolName, toolInput, toolResponse): Effect<Patch[]>`
  - Resolves the absolute `file_path`.
  - `Fs.readFile` → `current` (the server already reads local files for
    transcript enrichment; reuse that capability).
  - First time for `(turn,path)`: `baseline = reverseApply(toolResponse.structuredPatch, current)`.
  - Recompute `FileDiff` (status / added / removed / hunks) via
    `structuredPatch(baseline, current)`.
  - Return `[{ op: "update_turn_file", turnNumber, file }]`.
- `clearSession(sessionId)` — drop scratch on `SessionEnd`.

### Diff library

Add the **`diff`** npm package (jsdiff) to `gravity-server`:
- `structuredPatch(oldName, newName, oldStr, newStr)` → consolidated hunks
  (exact shape `StructuredPatchHunk`).
- `applyPatch(str, patch)` + `reversePatch(patch)` → exact reverse-apply for
  baseline reconstruction.

jsdiff is pure and synchronous → wrap calls in `Effect.try` (typed error
`FileDiffError`), per the project's Effect rules. It bundles cleanly into
`dist/gravity-server.mjs` via esbuild.

### Wiring in `event-handler.ts` → `handlePostToolUse`

After the existing `trackFile(...)` call, add:
```typescript
const EDIT_TOOLS = new Set(["Edit", "Write", "MultiEdit", "NotebookEdit"]);
if (EDIT_TOOLS.has(toolName) && toolResponseLooksSuccessful) {
  patches.push(...yield* recordEdit(
    session.sessionId, tool.turn, toolName,
    ctx.data.tool_input, toolResponse));
}
```
`tool.turn` is already known (the completed tool carries its turn number).
Read `structuredPatch` from `toolResponse` **before** any result-stripping —
`completeTool` may strip bloated fields from the stored result, but
`file-diff.ts` consumes the raw `toolResponse`.

Also extend `trackFile()`'s tool switch to recognise `MultiEdit`/`NotebookEdit`
as `op: "edit"` so the session-level Files section stays consistent.

### Turn finalization

No special handling needed: `editedFiles` is just a `TurnNode` field, so it is
included in `session.snapshot` automatically and survives `freeze_turn`.
On `SessionEnd`, call `clearSession()` to free scratch strings.

*Optional hardening:* on `freeze_turn`, re-read each edited file once and
re-emit `update_turn_file` to absorb any post-last-edit settling. Low value;
defer.

## Emacs client changes

### `claude-gravity-state.el` / `claude-gravity-session.el`
Turn-node alists gain an `edited-files` key (list of file-diff alists).
Pre-allocate it to `nil` at turn creation (the project's
`setf alist-get`-on-new-key pitfall — see MEMORY.md).

### `claude-gravity-client.el` — `claude-gravity--apply-patch`
New `pcase` branch `"update_turn_file"`:
- Locate the turn by `turnNumber`.
- Upsert the file-diff alist into the turn's `edited-files` by `path`.

### `claude-gravity-render.el` — new `claude-gravity--insert-turn-edited-files`
Modeled on the existing Tasks subsection (render.el ~150):
```elisp
(magit-insert-section (turn-edited-files nil t)
  (magit-insert-heading
    (format "%sEdited Files (%d)  %s"
            (claude-gravity--indent) (length files) totals))
  (dolist (fd files)
    (magit-insert-section (turn-file-diff path t)   ; collapsed by default
      (magit-insert-heading <path  +A −R  N edits>)
      (if hunks
          (claude-gravity--insert-structured-patch hunks "Diff:")   ; REUSE
        (insert <"diff unavailable" / "diff too large — F to open">)))))
```
Call it from `claude-gravity-insert-turns` (render.el ~654-770), placed after
the Tasks subsection, before `stop_text`.

`RET` on a `turn-file-diff` section visits the file (like the existing
`file-entry` section in the session Files list). No new diff-rendering code —
the server emits the exact hunk format `--insert-structured-patch` consumes.

## UI / UX specification

New per-turn subsection, collapsible, default-collapsed when the turn is
frozen and default-expanded for the current turn:

```
── Turns (3) ────────────────────────────────────────────
...
  Edited Files (2)  +131 −42
    claude-gravity-ui.el          +88 −20   3 edits
    packages/.../session.ts       +43 −22   created
```

Expanded file entry:
```
    claude-gravity-ui.el          +88 −20   3 edits
      Diff:
        @@ -120,4 +120,9 @@
           (existing context line)
        - removed line
        + added line
        ...
```

- Header aggregate `+A −B` sums every `FileDiff` in the turn.
- Per-file `status`: `created` / `deleted` shown instead of edit count when
  applicable; otherwise `N edit(s)`.
- Faces: reuse `claude-gravity-diff-added/removed/context/header`,
  `claude-gravity-tool-name` for the path, `claude-gravity-detail-label` for
  counts.
- Update `UI-SPEC.md` §6/§7 area with the new subsection.

### Relationship to the existing per-tool inline diff
Keep it. The per-tool diff (under each `Edit` tool's detail) shows *that single
edit*; the new section is the *turn roll-up*. They coexist — exactly as the
session-level Files section already coexists with per-tool diffs. Users who
find the per-tool diff redundant can collapse tool details; no behavioural
removal in this feature.

## Edge cases & fallbacks

| Case | Handling |
|---|---|
| `Write` create (new file) | `baseline = ""`, `status = "created"`, whole file as additions. |
| `Write` update | `structuredPatch` is the full V0→V1 diff; reverse-apply works. |
| First edit is `NotebookEdit` (non-standard patch) | Reverse-apply may fail → `hunks = null`, entry rendered path-only ("diff unavailable"). |
| `structuredPatch` empty/missing on first edit | Fallback: reconstruct `baseline` from `oldString`/`newString` (reverse string-replace, honoring `replace_all`). If still impossible → `hunks = null`. |
| `replace_all` Edit | `structuredPatch` already contains all hunks; reverse-apply handles it. |
| File unreadable/deleted at `PostToolUse` | Skip the read; if `current` unknown, emit `FileDiff` with `hunks=null` and a note, or skip the file. |
| Same file edited in turn 1 and turn 3 | Two independent `(turn,path)` scratch entries → each turn shows only its own net change. Correct. |
| Pre-prompt edits (turn 0) | Turn 0 gets an Edited Files section like any turn. |
| Reverse-apply mismatch (`applyPatch` returns false) | Fall back to `oldString`/`newString`; else `hunks = null`. Never crash. |

## Performance & size caps

- One small local file read per edit `PostToolUse`. Negligible — the server
  already reads multi-MB transcript files routinely.
- **Diff size cap:** if the consolidated diff exceeds a threshold
  (e.g. 800 lines or ~64 KB serialized), set `hunks = null`, `truncated = true`,
  keep `added`/`removed` counts. Terminals render "diff too large — open file".
  Prevents bloating `session.snapshot`.
- Emacs render already truncates via `claude-gravity-diff-max-lines`.
- Scratch `baseline`/`current` strings are dropped on `SessionEnd`; only the
  computed `FileDiff` (hunks) persists in `TurnNode`.

## Testing

**Server (`gravity-server` vitest):**
- Single edit → `FileDiff` equals the tool's own `structuredPatch`.
- Three sequential edits to one file → one consolidated diff `V0→V3`;
  `editCount = 3`; no duplicated regions.
- `Write` create → `status="created"`, baseline empty.
- `Edit` then `Write` (overwrite) same file same turn → consolidated diff.
- `MultiEdit` → single `update_turn_file`, correct net diff.
- Reverse-apply failure path → `hunks = null`, no throw.
- Same file across two turns → two independent `editedFiles` entries.
- Size cap → `truncated = true`, `hunks = null`.

**Emacs (ERT):**
- `update_turn_file` patch upserts by path (insert then replace).
- Render: "Edited Files (N)" heading, aggregate counts, expand shows diff,
  `hunks=null` renders path-only.

**E2E:** extend the dockerized harness — drive a multi-edit turn, assert the
section renders one consolidated diff.

## Implementation phases

1. **Shared types** — `FileDiff`, `StructuredPatchHunk`, `TurnNode.editedFiles`,
   `update_turn_file` patch.
2. **Server** — add `diff` dep; `enrichment/file-diff.ts` (reverse-apply +
   consolidate); wire into `handlePostToolUse`; extend `trackFile` tool set;
   `clearSession` on `SessionEnd`. Vitest.
3. **Emacs** — turn-node `edited-files` key; `update_turn_file` patch branch;
   `claude-gravity--insert-turn-edited-files` reusing `--insert-structured-patch`;
   call from `claude-gravity-insert-turns`. ERT.
4. **Docs** — `UI-SPEC.md` subsection; `ARCHITECTURE.md` patch list;
   `docs/session-data-model.md` turn-node `edited-files`.
5. **Polish** — size cap, `NotebookEdit` handling, E2E test.

## Open decisions (recommendations in **bold**)

1. Section placement within a turn — **after Tasks, before `stop_text`**.
2. Coexist with per-tool inline diffs — **yes, coexist** (feature is additive).
3. Diff library — **jsdiff (`diff`)** vs hand-rolled LCS. jsdiff gives
   reverse-apply + generation for free; **use jsdiff**.
4. Diff transport format — **structuredPatch hunks** (reuses Emacs renderer,
   matches jsdiff output) vs raw unified-diff string.
