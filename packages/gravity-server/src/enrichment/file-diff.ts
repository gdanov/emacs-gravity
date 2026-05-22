// file-diff.ts — Per-turn consolidated file-diff computation
//
// Each turn shows a summary of the files it edited. A file edited N times
// in one turn must show ONE consolidated diff: the file content before the
// turn's first edit (baseline) versus the content after the last edit
// (final / current).
//
// The hard part is obtaining `baseline` without a race. We never touch the
// bridge or PreToolUse. Instead, on the FIRST edit of a (turn, path) pair,
// the server reconstructs `baseline` by reverse-applying that tool's own
// `structuredPatch` to the on-disk file content. `current` is re-read from
// disk on every edit, so the approach is self-healing across N edits,
// MultiEdit, replace_all, and mixed Edit/Write sequences.
//
// Module-internal scratch state — NOT part of `Session`, so it never
// enters `session.snapshot`. Cleared on SessionEnd via `clearSession`.

import { Effect } from "effect";
import {
  structuredPatch as jsStructuredPatch,
  reversePatch as jsReversePatch,
  applyPatch as jsApplyPatch,
  type StructuredPatch,
} from "diff";
import { readFileSync } from "fs";
import type { FileDiff, Patch, Session, StructuredPatchHunk, TurnNode } from "@gravity/shared";

// ── Size caps ────────────────────────────────────────────────────────

/** Max lines in a consolidated diff before `hunks` is elided for size. */
const MAX_DIFF_LINES = 800;
/** Max serialized byte size of `hunks` before it is elided for size. */
const MAX_DIFF_BYTES = 64 * 1024;

// ── Scratch store ────────────────────────────────────────────────────

/** Per-(turn,path) reconstructed file versions held while a turn is open. */
interface FileDiffScratch {
  /** File content immediately before the turn's FIRST edit of this file. */
  baseline: string;
  /** File content after the most recent edit of this file. */
  current: string;
  /** Edit-class tool ops applied this turn, in order. */
  ops: string[];
}

/** turnKey = `${turnNumber} ${path}`. */
type TurnKey = string;

/**
 * Server-internal scratch: sessionId → (turnKey → scratch). Deliberately
 * NOT a `Session` field — it holds full file contents and must never be
 * serialized into a snapshot. Dropped on SessionEnd.
 */
const scratchStore = new Map<string, Map<TurnKey, FileDiffScratch>>();

function turnKey(turnNumber: number, path: string): TurnKey {
  return `${turnNumber} ${path}`;
}

function sessionScratch(sessionId: string): Map<TurnKey, FileDiffScratch> {
  let m = scratchStore.get(sessionId);
  if (!m) {
    m = new Map();
    scratchStore.set(sessionId, m);
  }
  return m;
}

/** Drop a session's scratch. Called from the SessionEnd handler. */
export function clearSession(sessionId: string): void {
  scratchStore.delete(sessionId);
}

// ── Pure helpers ─────────────────────────────────────────────────────

/** Map an edit-class tool name to a short op label for `FileDiff.ops`. */
function opLabel(toolName: string): string {
  switch (toolName) {
    case "Write": return "write";
    case "MultiEdit": return "edit";
    case "NotebookEdit": return "edit";
    case "Edit": return "edit";
    default: return "edit";
  }
}

/** Resolve the file path from an edit-class tool's input. */
function resolvePath(toolInput: Record<string, unknown> | undefined): string | null {
  if (!toolInput) return null;
  const fp = toolInput.file_path ?? toolInput.filePath ?? toolInput.notebook_path;
  return typeof fp === "string" && fp.length > 0 ? fp : null;
}

/** Read a file from disk, degrading to `null` rather than throwing. */
const readFileSafe = (path: string): Effect.Effect<string | null> =>
  Effect.match(
    Effect.try({
      try: () => readFileSync(path, "utf-8"),
      catch: (cause) => cause,
    }),
    { onFailure: () => null, onSuccess: (s) => s },
  );

/** Coerce a value to a well-formed structuredPatch hunk array, or null. */
function asHunks(value: unknown): StructuredPatchHunk[] | null {
  if (!Array.isArray(value)) return null;
  const hunks: StructuredPatchHunk[] = [];
  for (const h of value) {
    if (h == null || typeof h !== "object") return null;
    const rec = h as Record<string, unknown>;
    if (
      typeof rec.oldStart !== "number" ||
      typeof rec.oldLines !== "number" ||
      typeof rec.newStart !== "number" ||
      typeof rec.newLines !== "number" ||
      !Array.isArray(rec.lines)
    ) {
      return null;
    }
    hunks.push({
      oldStart: rec.oldStart,
      oldLines: rec.oldLines,
      newStart: rec.newStart,
      newLines: rec.newLines,
      lines: (rec.lines as unknown[]).map((l) => String(l)),
    });
  }
  return hunks;
}

/** Pull the raw `structuredPatch` field out of a tool response. */
function extractStructuredPatch(toolResponse: unknown): StructuredPatchHunk[] | null {
  if (toolResponse == null || typeof toolResponse !== "object") return null;
  const rec = toolResponse as Record<string, unknown>;
  // Claude Code uses camelCase `structuredPatch`; tolerate snake_case too.
  return asHunks(rec.structuredPatch) ?? asHunks(rec.structured_patch);
}

/**
 * Reverse-apply a structuredPatch to `current`, yielding the content
 * BEFORE the patch was applied. Returns null on any mismatch — never
 * throws.
 */
function reverseApply(
  hunks: StructuredPatchHunk[],
  current: string,
): string | null {
  try {
    const patch: StructuredPatch = {
      oldFileName: "a",
      newFileName: "b",
      oldHeader: undefined,
      newHeader: undefined,
      hunks: hunks as StructuredPatch["hunks"],
    };
    const reversed = jsReversePatch(patch);
    const applied = jsApplyPatch(current, reversed);
    return applied === false ? null : applied;
  } catch {
    return null;
  }
}

/**
 * Fallback baseline reconstruction from oldString/newString. Reverses the
 * single string replacement Claude Code's `Edit` tool performed, honoring
 * `replace_all`. Returns null when the replacement cannot be unambiguously
 * reversed.
 */
function reverseStringReplace(
  current: string,
  toolInput: Record<string, unknown> | undefined,
): string | null {
  if (!toolInput) return null;
  const oldString = toolInput.old_string ?? toolInput.oldString;
  const newString = toolInput.new_string ?? toolInput.newString;
  if (typeof oldString !== "string" || typeof newString !== "string") return null;
  const replaceAll = toolInput.replace_all === true || toolInput.replaceAll === true;
  // To get baseline we undo current's `newString` back to `oldString`.
  if (newString.length === 0) {
    // Pure insertion — can't locate where to re-remove it. Give up.
    return null;
  }
  if (replaceAll) {
    return current.split(newString).join(oldString);
  }
  const idx = current.indexOf(newString);
  if (idx === -1) return null;
  return current.slice(0, idx) + oldString + current.slice(idx + newString.length);
}

/** Count added / removed lines across a hunk array. */
function countLines(hunks: StructuredPatchHunk[]): { added: number; removed: number } {
  let added = 0;
  let removed = 0;
  for (const h of hunks) {
    for (const line of h.lines) {
      if (line.startsWith("+")) added++;
      else if (line.startsWith("-")) removed++;
    }
  }
  return { added, removed };
}

/** Total `lines` count across all hunks. */
function totalLines(hunks: StructuredPatchHunk[]): number {
  return hunks.reduce((acc, h) => acc + h.lines.length, 0);
}

/**
 * Compute the consolidated baseline→current diff and assemble a FileDiff.
 * Applies the size cap. Never throws.
 */
function buildFileDiff(
  path: string,
  ops: string[],
  baseline: string,
  current: string,
): FileDiff {
  const editCount = ops.length;
  let hunks: StructuredPatchHunk[] | null = null;
  try {
    const sp = jsStructuredPatch("a", "b", baseline, current);
    hunks = asHunks(sp.hunks);
  } catch {
    hunks = null;
  }

  const status: FileDiff["status"] =
    baseline.length === 0 && current.length > 0
      ? "created"
      : current.length === 0 && baseline.length > 0
        ? "deleted"
        : "modified";

  if (hunks == null) {
    return { path, ops, editCount, status, added: 0, removed: 0, hunks: null, truncated: false };
  }

  const { added, removed } = countLines(hunks);

  // Size cap: drop the hunks but keep the counts.
  const tooManyLines = totalLines(hunks) > MAX_DIFF_LINES;
  const tooManyBytes = JSON.stringify(hunks).length > MAX_DIFF_BYTES;
  if (tooManyLines || tooManyBytes) {
    return { path, ops, editCount, status, added, removed, hunks: null, truncated: true };
  }

  return { path, ops, editCount, status, added, removed, hunks, truncated: false };
}

/** Upsert a FileDiff into a turn's `editedFiles`, replacing by path. */
function upsertEditedFile(turn: TurnNode, file: FileDiff): void {
  const idx = turn.editedFiles.findIndex((f) => f.path === file.path);
  if (idx >= 0) {
    turn.editedFiles[idx] = file;
  } else {
    turn.editedFiles.push(file);
  }
}

// ── Public API ───────────────────────────────────────────────────────

/**
 * Record an edit-class tool's effect on a file within a turn.
 *
 * Mutates the session's turn — upserts the computed `FileDiff` into
 * `turn.editedFiles` by `path` — AND returns the `update_turn_file`
 * Patch[] (mirrors `completeTool` / `trackFile`: mutate state + return
 * patches). Returns `[]` when there is no usable turn or path.
 *
 * `toolResponse` MUST be the raw tool response (with `structuredPatch`
 * intact) — read it before `completeTool` strips bloated result fields.
 */
export function recordEdit(
  session: Session,
  turnNumber: number,
  toolName: string,
  toolInput: Record<string, unknown> | undefined,
  toolResponse: unknown,
): Effect.Effect<Patch[]> {
  return Effect.gen(function* () {
    const path = resolvePath(toolInput);
    if (!path) return [];

    const turn = session.turns.find((t) => t.turnNumber === turnNumber);
    if (!turn) return [];

    const scratch = sessionScratch(session.sessionId);
    const key = turnKey(turnNumber, path);
    const existing = scratch.get(key);
    const op = opLabel(toolName);

    // Read the on-disk content (the edit is settled — no race).
    const onDisk = yield* readFileSafe(path);

    if (existing) {
      // Subsequent edit: baseline is immutable; refresh `current`.
      existing.ops.push(op);
      if (onDisk != null) existing.current = onDisk;
      const file = buildFileDiff(path, existing.ops, existing.baseline, existing.current);
      upsertEditedFile(turn, file);
      return [{ op: "update_turn_file", turnNumber, file } as Patch];
    }

    // First edit of this (turn, path).
    const respRec =
      toolResponse != null && typeof toolResponse === "object"
        ? (toolResponse as Record<string, unknown>)
        : undefined;
    const isWriteCreate =
      toolName === "Write" &&
      (respRec?.type === "create" ||
        // Write-create: no prior content recorded and the structuredPatch
        // is empty (Claude Code emits `[]` for a fresh-file Write).
        (Array.isArray(respRec?.structuredPatch) && respRec.structuredPatch.length === 0));

    const current = onDisk != null
      ? onDisk
      // File unreadable: fall back to the Write content if available.
      : typeof respRec?.content === "string"
        ? (respRec.content as string)
        : null;

    if (current == null) {
      // Cannot determine final content — emit a path-only entry.
      const file: FileDiff = {
        path,
        ops: [op],
        editCount: 1,
        status: "modified",
        added: 0,
        removed: 0,
        hunks: null,
        truncated: false,
      };
      upsertEditedFile(turn, file);
      return [{ op: "update_turn_file", turnNumber, file } as Patch];
    }

    // Reconstruct baseline.
    let baseline: string | null;
    if (isWriteCreate) {
      baseline = "";
    } else {
      const patchHunks = extractStructuredPatch(toolResponse);
      baseline =
        (patchHunks ? reverseApply(patchHunks, current) : null) ??
        reverseStringReplace(current, toolInput);
    }

    if (baseline == null) {
      // Reverse-apply failed every which way — emit a path-only entry but
      // still record scratch so subsequent edits don't re-attempt baseline
      // reconstruction against a different `current`.
      scratch.set(key, { baseline: current, current, ops: [op] });
      const file: FileDiff = {
        path,
        ops: [op],
        editCount: 1,
        status: "modified",
        added: 0,
        removed: 0,
        hunks: null,
        truncated: false,
      };
      upsertEditedFile(turn, file);
      return [{ op: "update_turn_file", turnNumber, file } as Patch];
    }

    scratch.set(key, { baseline, current, ops: [op] });
    const file = buildFileDiff(path, [op], baseline, current);
    upsertEditedFile(turn, file);
    return [{ op: "update_turn_file", turnNumber, file } as Patch];
  });
}
