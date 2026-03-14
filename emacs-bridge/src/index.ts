import { Effect, Layer, pipe } from "effect";
import { existsSync, readFileSync } from "fs";
import { join } from "path";
import { log } from "./log.js";
import { isSafeBashCommand } from "./safe-bash.js";
import { sendToEmacs, sendToEmacsAndWait } from "./socket.js";
import { nextDumpSeq, writeDumpFile } from "./dump.js";
import type { HookData } from "./types.js";
import {
  enrichSessionMetadata,
  enrichSubagentStart,
  enrichSubagentStop,
  enrichSessionEnd,
  enrichToolAttribution,
  enrichPreToolUse,
  enrichPostToolUse,
  enrichStop,
} from "./enrich.js";
import { ProcessIO, ProcessIOLive } from "./services/process-io.js";
import { BridgeConfig, BridgeConfigLive } from "./services/config.js";
import { FsLive } from "./services/fs.js";
import { LoggerLive } from "./services/logger.js";

// Re-export types and transcript functions
export type { HookData, TokenUsage } from "./types.js";
export {
  readTail,
  readHead,
  extractPrecedingContent,
  extractFollowingContent,
  extractTrailingText,
  extractTokenUsage,
  extractTranscriptMeta,
  extractSlug,
} from "./enrichment.js";

// --- Parse stdin ---
const parseStdin = (raw: string): Effect.Effect<HookData> =>
  pipe(
    Effect.try({
      try: () => raw.length > 0 ? JSON.parse(raw) as HookData : {} as HookData,
      catch: (cause) => new Error(String(cause)),
    }),
    Effect.catch(() => Effect.succeed({} as HookData)),
  );

// --- Check ignored sessions ---
const isIgnoredSession = (sessionId: string, cwd: string): boolean => {
  const ignoreFile = join(cwd, ".claude", "gravity-ignored-sessions.json");
  if (existsSync(ignoreFile)) {
    try {
      const ignored = JSON.parse(readFileSync(ignoreFile, "utf-8"));
      return Array.isArray(ignored) && ignored.includes(sessionId);
    } catch {
      return false;
    }
  }
  return false;
};

// --- Main program ---
const program = Effect.gen(function* () {
  const io = yield* Effect.service(ProcessIO);
  const config = yield* Effect.service(BridgeConfig);

  yield* Effect.logDebug(`Process started: ${process.argv.join(" ")}`);

  process.stdout.on("error", (err) => {
    log(`stdout error: ${err.message}`, "error");
  });

  const eventName = (yield* io.getArg(2)) ?? "unknown";

  // Read and parse stdin
  const raw = yield* io.readStdin().pipe(Effect.catch(() => Effect.succeed("")));
  const inputData = yield* parseStdin(raw);

  yield* Effect.logDebug(`Payload: ${JSON.stringify(inputData)}`);

  // Early exit: if socket doesn't exist, pass through immediately
  const socketPath = config.socketPath;
  if (!existsSync(socketPath)) {
    yield* Effect.logDebug(`Socket not found at ${socketPath}, passing through`);
    yield* io.writeStdout(JSON.stringify({}) + "\n");
    return;
  }

  // Snapshot raw hook input before enrichment mutations
  const rawHookInput = JSON.parse(JSON.stringify(inputData));

  // Extract session identifiers
  const sessionId = inputData.session_id || "unknown";
  const cwd = inputData.cwd || "";
  const pid = config.claudePid;

  // Inject config-derived fields into hook data
  if (config.tempId) inputData.temp_id = config.tempId;
  if (config.tmuxSession) inputData.tmux_session = config.tmuxSession;
  if (config.effortLevel) inputData.effort_level = config.effortLevel;

  // Dump raw input if dump mode enabled
  let dumpSeq: number | undefined;
  if (config.dumpDir) {
    dumpSeq = nextDumpSeq(config.dumpDir);
    writeDumpFile(config.dumpDir, dumpSeq, eventName, "raw", inputData);
  }

  // --- Enrichment (still imperative — converted in Phase 3) ---
  const transcriptPath = inputData.transcript_path;

  enrichSessionMetadata(inputData, transcriptPath);

  if (eventName === "SubagentStart") {
    enrichSubagentStart(inputData, sessionId, cwd, transcriptPath);
  }
  if (eventName === "SubagentStop") {
    enrichSubagentStop(inputData, sessionId, cwd, transcriptPath);
  }
  if (eventName === "SessionEnd") {
    enrichSessionEnd(sessionId, cwd);
  }
  if (eventName === "PreToolUse" || eventName === "PostToolUse" || eventName === "PostToolUseFailure") {
    enrichToolAttribution(inputData, sessionId, cwd, transcriptPath);
  }
  if (eventName === "PreToolUse") {
    enrichPreToolUse(inputData, sessionId, transcriptPath);
  }
  if (eventName === "PostToolUse" || eventName === "PostToolUseFailure") {
    enrichPostToolUse(inputData, sessionId, transcriptPath);
  }
  if (eventName === "Stop") {
    yield* Effect.promise(() => enrichStop(inputData, transcriptPath));
  }

  // Dump enriched output
  if (config.dumpDir && dumpSeq !== undefined) {
    writeDumpFile(config.dumpDir, dumpSeq, eventName, "output", {
      event: eventName, session_id: sessionId, cwd, pid, data: inputData,
    });
  }

  // --- Routing ---

  // Auto-approve safe read-only Bash commands (skip Emacs round-trip)
  if (eventName === "PermissionRequest" && !config.noAutoApprove && isSafeBashCommand(inputData)) {
    yield* Effect.promise(() =>
      sendToEmacs("PermissionAutoApproved", sessionId, cwd, pid, {
        tool_name: inputData.tool_name,
        tool_use_id: inputData.tool_use_id,
        command: inputData.tool_input?.command,
      })
    );
    yield* io.writeStdout(JSON.stringify({
      hookSpecificOutput: {
        hookEventName: "PermissionRequest",
        decision: { behavior: "allow" },
      },
    }) + "\n");
    return;
  }

  // Check if session is ignored — pass bidirectional hooks through to TUI
  if (eventName === "PermissionRequest" || eventName === "AskUserQuestionIntercept") {
    if (isIgnoredSession(sessionId, cwd)) {
      yield* Effect.logDebug(`Session ${sessionId} is ignored, passing ${eventName} through to TUI`);
      yield* Effect.promise(() => sendToEmacs(eventName, sessionId, cwd, pid, inputData, rawHookInput));
      yield* io.writeStdout(JSON.stringify({}) + "\n");
      return;
    }
  }

  // Send to Emacs — PermissionRequest and AskUserQuestionIntercept use bidirectional wait
  if (eventName === "PermissionRequest") {
    const toolName = inputData.tool_name || "unknown";
    yield* Effect.logWarning(`PermissionRequest: waiting for Emacs response [tool=${toolName}, session=${sessionId}]`);
    const response = yield* Effect.promise(() =>
      sendToEmacsAndWait(eventName, sessionId, cwd, pid, inputData, undefined, rawHookInput)
    );

    // Guard against empty response
    if (!response || Object.keys(response).length === 0) {
      yield* Effect.logError(`PermissionRequest: empty response from Emacs [tool=${toolName}, session=${sessionId}] — writing {} to stdout`);
    }

    // ── DENY-AS-APPROVE WORKAROUND ──────────────────────────────────────
    // Bug: Claude Code silently ignores ExitPlanMode "allow" responses from
    // PermissionRequest hooks. This is a regression of #15755.
    // See ARCHITECTURE.md for full explanation.
    //
    // TO REMOVE: When Claude Code fixes #15755, delete this if-block.
    // ─────────────────────────────────────────────────────────────────────
    if (toolName === "ExitPlanMode" && response?.decision?.behavior === "allow") {
      yield* Effect.logWarning(`PermissionRequest: converting ExitPlanMode allow → deny-as-approve [session=${sessionId}]`);
      response.decision = {
        behavior: "deny",
        message: "User approved the plan. Proceed with implementation.",
      };
    }

    const responseStr = JSON.stringify(response) + "\n";
    // Hard timeout: ensure process exits even if stdout write callback never fires
    const hardExit = setTimeout(() => {
      log(`PermissionRequest: hard exit timeout [tool=${toolName}]`, "error");
      process.exit(1);
    }, 5000);
    hardExit.unref();
    process.stdout.write(responseStr, () => {
      log(`PermissionRequest: stdout write callback OK [tool=${toolName}]`, "warn");
      setTimeout(() => process.exit(0), 50);
    });

  } else if (eventName === "AskUserQuestionIntercept") {
    yield* Effect.logWarning("AskUserQuestionIntercept: waiting for Emacs response");
    const response = yield* Effect.promise(() =>
      sendToEmacsAndWait("PreToolUse", sessionId, cwd, pid, inputData, undefined, rawHookInput)
    );
    const output = (response && response.hookSpecificOutput)
      ? JSON.stringify(response)
      : JSON.stringify({});
    process.stdout.write(output + "\n", () => {
      process.exit(0);
    });

  } else {
    // Fire-and-forget: send to Emacs and output {}
    if (eventName === "SubagentStop") {
      yield* Effect.logWarning(
        `[FINAL_CHECK_BEFORE_SEND] agent_stop_text=${typeof inputData.agent_stop_text === "string" ? `"${(inputData.agent_stop_text as string).substring(0, 40)}"` : inputData.agent_stop_text}`
      );
    }
    yield* Effect.promise(() => sendToEmacs(eventName, sessionId, cwd, pid, inputData, rawHookInput));
    yield* io.writeStdout(JSON.stringify({}) + "\n");
  }
});

// Top-level safety: ALWAYS output valid JSON, even on uncaught errors
const safe = pipe(
  program,
  Effect.catch((error) =>
    Effect.gen(function* () {
      yield* Effect.logError("Bridge error: " + String(error));
      const io = yield* Effect.service(ProcessIO);
      yield* io.writeStdout(JSON.stringify({}) + "\n");
    })
  ),
);

// --- Layer composition ---
const MainLive = Layer.mergeAll(
  ProcessIOLive,
  FsLive,
  LoggerLive,
);

// BridgeConfigLive depends on ProcessIO + Fs
const ConfigLayer = Layer.provide(BridgeConfigLive, MainLive);
const FullLayer = Layer.mergeAll(MainLive, ConfigLayer);

// --- Entry point ---
const args = process.argv.slice(2);
if (args.includes("--mode") && args[args.indexOf("--mode") + 1] === "opencode") {
  import("./opencode-bridge.js").then(() => {
    log("OpenCode bridge started");
  }).catch((e) => {
    console.error("Failed to load opencode-bridge:", e);
    process.exit(1);
  });
} else {
  Effect.runPromise(Effect.provide(safe, FullLayer)).catch(() => {
    // Last-resort safety: output valid JSON even if Effect runtime fails
    try { process.stdout.write(JSON.stringify({}) + "\n"); } catch {}
  });
}
