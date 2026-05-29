// file-diff.test.ts — Per-turn consolidated file-diff computation.
//
// `recordEdit` reads the edited file from disk (via node:fs), so these
// tests use a real temp directory: each test writes the file's final
// on-disk content, then calls `recordEdit` with the tool response that
// produced that state.

import { describe, it, expect, afterEach, beforeEach } from "vitest";
import { Effect, Layer } from "effect";
import { mkdtempSync, writeFileSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { structuredPatch as jsStructuredPatch } from "diff";
import { recordEdit, clearSession } from "../src/enrichment/file-diff.js";
import { createSession, openTurn } from "../src/state/session.js";
import { handleEvent } from "../src/handlers/event-handler.js";
import { SessionStore, makeSessionStore } from "../src/services/session-store.js";
import { Inbox, makeInbox } from "../src/services/inbox.js";
import { FsTest } from "@gravity/shared";
import type { HookData, Patch, Session, FileDiff } from "@gravity/shared";

// ── Helpers ──────────────────────────────────────────────────────────

let tmp: string;

beforeEach(() => {
  tmp = mkdtempSync(join(tmpdir(), "gravity-filediff-"));
});

afterEach(() => {
  rmSync(tmp, { recursive: true, force: true });
});

/** Build a session with `turnCount` opened turns (turn 0 always exists). */
function makeSession(id = "s1"): Session {
  const s = createSession(id, "/test/project");
  return s;
}

/** Open a real turn and return its number. */
function openNewTurn(s: Session): number {
  openTurn(s);
  return s.currentTurn;
}

/** Write file content to disk under the temp dir and return its path. */
function diskFile(name: string, content: string): string {
  const p = join(tmp, name);
  writeFileSync(p, content, "utf-8");
  return p;
}

/** Claude-Code-shaped `structuredPatch` for an Edit (old → new content). */
function ccStructuredPatch(oldContent: string, newContent: string) {
  return jsStructuredPatch("file", "file", oldContent, newContent).hunks;
}

/** Run a `recordEdit` Effect synchronously. */
function run(eff: Effect.Effect<Patch[]>): Patch[] {
  return Effect.runSync(eff);
}

/** Extract the single FileDiff from an `update_turn_file` patch list. */
function fileOf(patches: Patch[]): FileDiff {
  expect(patches.length).toBe(1);
  const p = patches[0];
  expect(p.op).toBe("update_turn_file");
  return (p as Extract<Patch, { op: "update_turn_file" }>).file;
}

// ── Tests ────────────────────────────────────────────────────────────

describe("file-diff: recordEdit", () => {
  it("single Edit → FileDiff equals the tool's own consolidated diff", () => {
    const s = makeSession();
    const turn = openNewTurn(s);

    const v0 = "line1\nline2\nline3\n";
    const v1 = "line1\nCHANGED\nline3\n";
    const path = diskFile("a.txt", v1);

    const toolResponse = {
      filePath: path,
      oldString: "line2",
      newString: "CHANGED",
      structuredPatch: ccStructuredPatch(v0, v1),
    };

    const patches = run(
      recordEdit(s, turn, "Edit", { file_path: path }, toolResponse),
    );
    const file = fileOf(patches);

    expect(file.path).toBe(path);
    expect(file.editCount).toBe(1);
    expect(file.ops).toEqual(["edit"]);
    expect(file.status).toBe("modified");
    expect(file.truncated).toBe(false);
    expect(file.hunks).not.toBeNull();
    // Consolidated diff equals a fresh V0→V1 structuredPatch.
    expect(file.hunks).toEqual(jsStructuredPatch("a", "b", v0, v1).hunks);
    expect(file.added).toBe(1);
    expect(file.removed).toBe(1);

    // The turn's editedFiles was mutated in place.
    const turnNode = s.turns.find((t) => t.turnNumber === turn)!;
    expect(turnNode.editedFiles.length).toBe(1);
    expect(turnNode.editedFiles[0].path).toBe(path);

    clearSession(s.sessionId);
  });

  it("THREE sequential edits to one file → ONE consolidated V0→V3 diff", () => {
    const s = makeSession();
    const turn = openNewTurn(s);

    const v0 = "a\nb\nc\nd\ne\n";
    const v1 = "a\nB\nc\nd\ne\n";
    const v2 = "a\nB\nc\nD\ne\n";
    const v3 = "a\nB\nc\nD\nE\n";

    // Edit 1 — file on disk is v1.
    let path = diskFile("multi.txt", v1);
    let patches = run(
      recordEdit(s, turn, "Edit", { file_path: path }, {
        filePath: path, structuredPatch: ccStructuredPatch(v0, v1),
      }),
    );
    let file = fileOf(patches);
    expect(file.editCount).toBe(1);

    // Edit 2 — file on disk now v2; its structuredPatch is V1→V2.
    writeFileSync(path, v2, "utf-8");
    patches = run(
      recordEdit(s, turn, "Edit", { file_path: path }, {
        filePath: path, structuredPatch: ccStructuredPatch(v1, v2),
      }),
    );
    file = fileOf(patches);
    expect(file.editCount).toBe(2);

    // Edit 3 — file on disk now v3; its structuredPatch is V2→V3.
    writeFileSync(path, v3, "utf-8");
    patches = run(
      recordEdit(s, turn, "Edit", { file_path: path }, {
        filePath: path, structuredPatch: ccStructuredPatch(v2, v3),
      }),
    );
    file = fileOf(patches);

    // ONE consolidated diff baseline(v0) → final(v3).
    expect(file.editCount).toBe(3);
    expect(file.ops).toEqual(["edit", "edit", "edit"]);
    expect(file.hunks).toEqual(jsStructuredPatch("a", "b", v0, v3).hunks);
    expect(file.added).toBe(3); // B, D, E
    expect(file.removed).toBe(3); // b, d, e

    // No duplicated regions: every changed line appears exactly once.
    const allLines = file.hunks!.flatMap((h) => h.lines);
    const addedLines = allLines.filter((l) => l.startsWith("+"));
    expect(addedLines.sort()).toEqual(["+B", "+D", "+E"]);

    // Turn has exactly one entry for the file.
    const turnNode = s.turns.find((t) => t.turnNumber === turn)!;
    expect(turnNode.editedFiles.length).toBe(1);

    clearSession(s.sessionId);
  });

  it("Write-create → status 'created', baseline empty, whole file added", () => {
    const s = makeSession();
    const turn = openNewTurn(s);

    const content = "new file\nsecond line\n";
    const path = diskFile("created.txt", content);

    const patches = run(
      recordEdit(s, turn, "Write", { file_path: path }, {
        type: "create",
        filePath: path,
        content,
        structuredPatch: [],
      }),
    );
    const file = fileOf(patches);

    expect(file.status).toBe("created");
    expect(file.ops).toEqual(["write"]);
    expect(file.hunks).not.toBeNull();
    expect(file.removed).toBe(0);
    expect(file.added).toBe(2);

    clearSession(s.sessionId);
  });

  it("Edit then Write of the same file in the same turn → consolidated diff", () => {
    const s = makeSession();
    const turn = openNewTurn(s);

    const v0 = "original\ncontent\n";
    const v1 = "ORIGINAL\ncontent\n";
    const v2 = "totally\nrewritten\nfile\n";

    // Edit: v0 → v1.
    const path = diskFile("mixed.txt", v1);
    let patches = run(
      recordEdit(s, turn, "Edit", { file_path: path }, {
        filePath: path, structuredPatch: ccStructuredPatch(v0, v1),
      }),
    );
    expect(fileOf(patches).editCount).toBe(1);

    // Write (overwrite): v1 → v2.
    writeFileSync(path, v2, "utf-8");
    patches = run(
      recordEdit(s, turn, "Write", { file_path: path }, {
        type: "update",
        filePath: path,
        content: v2,
        structuredPatch: ccStructuredPatch(v1, v2),
      }),
    );
    const file = fileOf(patches);

    expect(file.editCount).toBe(2);
    expect(file.ops).toEqual(["edit", "write"]);
    // Consolidated baseline(v0) → final(v2).
    expect(file.hunks).toEqual(jsStructuredPatch("a", "b", v0, v2).hunks);

    clearSession(s.sessionId);
  });

  it("MultiEdit → a single update_turn_file patch with correct net diff", () => {
    const s = makeSession();
    const turn = openNewTurn(s);

    const v0 = "one\ntwo\nthree\nfour\n";
    const v1 = "ONE\ntwo\nthree\nFOUR\n";
    const path = diskFile("multiedit.txt", v1);

    const patches = run(
      recordEdit(s, turn, "MultiEdit", { file_path: path }, {
        filePath: path,
        structuredPatch: ccStructuredPatch(v0, v1),
      }),
    );

    expect(patches.length).toBe(1);
    const file = fileOf(patches);
    expect(file.ops).toEqual(["edit"]);
    expect(file.editCount).toBe(1);
    expect(file.hunks).toEqual(jsStructuredPatch("a", "b", v0, v1).hunks);
    expect(file.added).toBe(2);
    expect(file.removed).toBe(2);

    clearSession(s.sessionId);
  });

  it("reverse-apply failure → hunks=null, no throw, still emits a patch", () => {
    const s = makeSession();
    const turn = openNewTurn(s);

    const path = diskFile("broken.txt", "current content\non disk\n");

    // structuredPatch references context lines that do not exist in the
    // on-disk file → reverse-apply fails; no oldString/newString fallback.
    const bogusPatch = [
      {
        oldStart: 1,
        oldLines: 2,
        newStart: 1,
        newLines: 2,
        lines: [" nonexistent context", "-removed", "+added"],
      },
    ];

    const patches = run(
      recordEdit(s, turn, "Edit", { file_path: path }, {
        filePath: path,
        structuredPatch: bogusPatch,
      }),
    );
    const file = fileOf(patches);

    expect(file.hunks).toBeNull();
    expect(file.path).toBe(path);
    expect(file.editCount).toBe(1);

    // The turn still records the file (path-only entry).
    const turnNode = s.turns.find((t) => t.turnNumber === turn)!;
    expect(turnNode.editedFiles.length).toBe(1);
    expect(turnNode.editedFiles[0].hunks).toBeNull();

    clearSession(s.sessionId);
  });

  it("oldString/newString fallback when structuredPatch is missing", () => {
    const s = makeSession();
    const turn = openNewTurn(s);

    const v1 = "alpha\nGAMMA\ndelta\n";
    const path = diskFile("fallback.txt", v1);

    // No structuredPatch — must reconstruct baseline from old/new strings
    // carried in tool_input (the Edit tool's parameters).
    const patches = run(
      recordEdit(
        s,
        turn,
        "Edit",
        { file_path: path, old_string: "BETA", new_string: "GAMMA" },
        { filePath: path },
      ),
    );
    const file = fileOf(patches);

    expect(file.hunks).not.toBeNull();
    // baseline = "alpha\nBETA\ndelta\n"
    expect(file.added).toBe(1);
    expect(file.removed).toBe(1);

    clearSession(s.sessionId);
  });

  it("same file edited in two different turns → two independent entries", () => {
    const s = makeSession();

    const v0 = "x\ny\nz\n";
    const v1 = "x\nY1\nz\n";
    const v2 = "x\nY1\nZ2\n";
    const path = join(tmp, "twoturns.txt");

    // Turn A: edit v0 → v1.
    const turnA = openNewTurn(s);
    writeFileSync(path, v1, "utf-8");
    const patchesA = run(
      recordEdit(s, turnA, "Edit", { file_path: path }, {
        filePath: path, structuredPatch: ccStructuredPatch(v0, v1),
      }),
    );
    const fileA = fileOf(patchesA);
    expect(fileA.hunks).toEqual(jsStructuredPatch("a", "b", v0, v1).hunks);

    // Turn B: edit v1 → v2 — independent (turn,path) scratch.
    const turnB = openNewTurn(s);
    writeFileSync(path, v2, "utf-8");
    const patchesB = run(
      recordEdit(s, turnB, "Edit", { file_path: path }, {
        filePath: path, structuredPatch: ccStructuredPatch(v1, v2),
      }),
    );
    const fileB = fileOf(patchesB);
    // Turn B's net change is v1 → v2, NOT v0 → v2.
    expect(fileB.hunks).toEqual(jsStructuredPatch("a", "b", v1, v2).hunks);
    expect(fileB.editCount).toBe(1);

    // Each turn has its own single entry.
    const turnNodeA = s.turns.find((t) => t.turnNumber === turnA)!;
    const turnNodeB = s.turns.find((t) => t.turnNumber === turnB)!;
    expect(turnNodeA.editedFiles.length).toBe(1);
    expect(turnNodeB.editedFiles.length).toBe(1);
    expect(turnNodeA.editedFiles[0].hunks).toEqual(fileA.hunks);
    expect(turnNodeB.editedFiles[0].hunks).toEqual(fileB.hunks);

    clearSession(s.sessionId);
  });

  it("size cap → hunks=null, truncated=true, counts preserved", () => {
    const s = makeSession();
    const turn = openNewTurn(s);

    // Baseline empty, final is a huge new file → diff exceeds 800 lines.
    const bigContent = Array.from({ length: 2000 }, (_, i) => `line ${i}`).join("\n") + "\n";
    const path = diskFile("big.txt", bigContent);

    const patches = run(
      recordEdit(s, turn, "Write", { file_path: path }, {
        type: "create",
        filePath: path,
        content: bigContent,
        structuredPatch: [],
      }),
    );
    const file = fileOf(patches);

    expect(file.truncated).toBe(true);
    expect(file.hunks).toBeNull();
    // Counts survive the size-cap elision.
    expect(file.added).toBe(2000);
    expect(file.removed).toBe(0);
    expect(file.status).toBe("created");

    clearSession(s.sessionId);
  });

  it("clearSession drops scratch so a re-edit recomputes baseline fresh", () => {
    const s = makeSession();
    const turn = openNewTurn(s);

    const v0 = "p\nq\nr\n";
    const v1 = "p\nQ\nr\n";
    const v2 = "p\nQ\nR\n";
    const path = diskFile("rebuild.txt", v1);

    run(
      recordEdit(s, turn, "Edit", { file_path: path }, {
        filePath: path, structuredPatch: ccStructuredPatch(v0, v1),
      }),
    );

    // Drop scratch — simulates SessionEnd.
    clearSession(s.sessionId);

    // A new edit on the same (turn,path) now treats this as the FIRST edit
    // again: baseline reconstructed from this patch (v1 → v2).
    writeFileSync(path, v2, "utf-8");
    const patches = run(
      recordEdit(s, turn, "Edit", { file_path: path }, {
        filePath: path, structuredPatch: ccStructuredPatch(v1, v2),
      }),
    );
    const file = fileOf(patches);
    expect(file.editCount).toBe(1);
    expect(file.hunks).toEqual(jsStructuredPatch("a", "b", v1, v2).hunks);

    clearSession(s.sessionId);
  });
});

// ── Integration: full event-handler wiring ───────────────────────────

describe("file-diff: handlePostToolUse wiring", () => {
  /** Drive a hook event through the real event handler. */
  function fireEvent(
    store: ReturnType<typeof makeSessionStore>,
    event: string,
    sessionId: string,
    data: HookData,
  ): Patch[] {
    const layer = Layer.mergeAll(
      Layer.succeed(SessionStore, store),
      Layer.succeed(Inbox, makeInbox()),
      FsTest({}),
    );
    return Effect.runSync(
      Effect.provide(
        handleEvent(event as never, sessionId, "/test/project", data, 123),
        layer,
      ),
    );
  }

  it("PreToolUse + PostToolUse(Edit) emits update_turn_file on the tool's turn", () => {
    const store = makeSessionStore();
    fireEvent(store, "SessionStart", "sess", {});
    // Open a real turn via a user prompt.
    fireEvent(store, "UserPromptSubmit", "sess", { prompt: "edit a file" });

    const v0 = "first\nsecond\nthird\n";
    const v1 = "first\nSECOND\nthird\n";
    const path = diskFile("wired.txt", v1);

    fireEvent(store, "PreToolUse", "sess", {
      tool_name: "Edit",
      tool_use_id: "tu_1",
      tool_input: { file_path: path },
    });

    const patches = fireEvent(store, "PostToolUse", "sess", {
      tool_name: "Edit",
      tool_use_id: "tu_1",
      tool_input: { file_path: path },
      tool_response: {
        filePath: path,
        structuredPatch: ccStructuredPatch(v0, v1),
      },
    });

    const turnFilePatches = patches.filter((p) => p.op === "update_turn_file");
    expect(turnFilePatches.length).toBe(1);
    const tfp = turnFilePatches[0] as Extract<Patch, { op: "update_turn_file" }>;
    expect(tfp.turnNumber).toBe(1); // first user turn
    expect(tfp.file.path).toBe(path);
    expect(tfp.file.hunks).toEqual(jsStructuredPatch("a", "b", v0, v1).hunks);

    // The completed tool's result was stripped of structuredPatch but the
    // FileDiff was still computed from the raw response.
    const session = store.get("sess")!;
    const turn = session.turns.find((t) => t.turnNumber === 1)!;
    expect(turn.editedFiles.length).toBe(1);
    expect(turn.editedFiles[0].hunks).not.toBeNull();

    clearSession("sess");
  });

  it("non-edit tool (Read) does not emit update_turn_file", () => {
    const store = makeSessionStore();
    fireEvent(store, "SessionStart", "sess", {});
    fireEvent(store, "UserPromptSubmit", "sess", { prompt: "read a file" });

    const path = diskFile("readonly.txt", "content\n");
    fireEvent(store, "PreToolUse", "sess", {
      tool_name: "Read",
      tool_use_id: "tu_r",
      tool_input: { file_path: path },
    });
    const patches = fireEvent(store, "PostToolUse", "sess", {
      tool_name: "Read",
      tool_use_id: "tu_r",
      tool_input: { file_path: path },
      tool_response: { content: "content\n" },
    });

    expect(patches.some((p) => p.op === "update_turn_file")).toBe(false);
    clearSession("sess");
  });

  it("SessionEnd clears file-diff scratch", () => {
    const store = makeSessionStore();
    fireEvent(store, "SessionStart", "sess", {});
    fireEvent(store, "UserPromptSubmit", "sess", { prompt: "edit" });

    const v0 = "a\nb\n";
    const v1 = "a\nB\n";
    const v2 = "a\nB2\n";
    const path = diskFile("ended.txt", v1);

    fireEvent(store, "PreToolUse", "sess", {
      tool_name: "Edit", tool_use_id: "tu_1", tool_input: { file_path: path },
    });
    fireEvent(store, "PostToolUse", "sess", {
      tool_name: "Edit", tool_use_id: "tu_1", tool_input: { file_path: path },
      tool_response: { filePath: path, structuredPatch: ccStructuredPatch(v0, v1) },
    });

    fireEvent(store, "SessionEnd", "sess", {});

    // After SessionEnd the scratch is gone — a fresh recordEdit on the same
    // (turn,path) recomputes baseline from the new patch instead of reusing
    // the cleared v0 baseline.
    const session = store.get("sess")!;
    writeFileSync(path, v2, "utf-8");
    const patches = Effect.runSync(
      recordEdit(session, 1, "Edit", { file_path: path }, {
        filePath: path, structuredPatch: ccStructuredPatch(v1, v2),
      }),
    );
    const file = (patches[0] as Extract<Patch, { op: "update_turn_file" }>).file;
    expect(file.editCount).toBe(1); // treated as first edit (scratch cleared)
    expect(file.hunks).toEqual(jsStructuredPatch("a", "b", v1, v2).hunks);

    clearSession("sess");
  });
});
