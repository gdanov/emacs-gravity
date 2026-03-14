import { Effect, Layer, pipe } from "effect";
import { join } from "path";
import { log, initLogForSession } from "./log.js";
import { isSafeBashCommand } from "./safe-bash.js";
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
import { Fs, FsLive } from "./services/fs.js";
import { LoggerLive } from "./services/logger.js";
import { EmacsSocket, EmacsSocketLive } from "./services/emacs-socket.js";

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

// --- Main program ---
const program = Effect.gen(function* () {
  const io = yield* Effect.service(ProcessIO);
  const fs = yield* Effect.service(Fs);
  const socket = yield* Effect.service(EmacsSocket);
  const config = yield* Effect.service(BridgeConfig);

  yield* Effect.logDebug(`Process started: ${process.argv.join(" ")}`);

  process.stdout.on("error", (err) => {
    log(`stdout error: ${err.message}`, "error");
  });

  const eventName = (yield* io.getArg(2)) ?? "unknown";

  // Read and parse stdin
  const raw = yield* io.readStdin().pipe(Effect.catch(() => Effect.succeed("")));
  let inputData = yield* parseStdin(raw);

  yield* Effect.logDebug(`Payload: ${JSON.stringify(inputData)}`);

  // Early exit: if socket doesn't exist, pass through immediately
  const socketPath = config.socketPath;
  const socketExists = yield* socket.socketExists();
  if (!socketExists) {
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

  // Dump raw input if transcript path available
  const transcriptPath = inputData.transcript_path;
  initLogForSession(transcriptPath);
  
  let dumpSeq: number | undefined;
  if (transcriptPath) {
    dumpSeq = nextDumpSeq(transcriptPath);
    writeDumpFile(transcriptPath, dumpSeq, eventName, "raw", inputData);
  }

  // --- Enrichment (pure functions returning new enriched objects) ---
  let enrichedData = enrichSessionMetadata(inputData, transcriptPath);

  if (eventName === "SubagentStart") {
    enrichedData = enrichSubagentStart(enrichedData, sessionId, cwd, transcriptPath);
  }
  if (eventName === "SubagentStop") {
    enrichedData = enrichSubagentStop(enrichedData, sessionId, cwd, transcriptPath);
  }
  if (eventName === "SessionEnd") {
    enrichedData = enrichSessionEnd(enrichedData, sessionId, cwd, transcriptPath);
  }
  if (eventName === "PreToolUse" || eventName === "PostToolUse" || eventName === "PostToolUseFailure") {
    enrichedData = enrichToolAttribution(enrichedData, sessionId, cwd, transcriptPath);
  }
  if (eventName === "PreToolUse") {
    enrichedData = enrichPreToolUse(enrichedData, sessionId, transcriptPath);
  }
  if (eventName === "PostToolUse" || eventName === "PostToolUseFailure") {
    enrichedData = enrichPostToolUse(enrichedData, sessionId, transcriptPath);
  }
  if (eventName === "Stop") {
    enrichedData = yield* Effect.promise(() => enrichStop(enrichedData, transcriptPath));
  }

  // Use enriched data for the rest of the pipeline
  inputData = enrichedData;

  // Dump enriched output
  if (transcriptPath && dumpSeq !== undefined) {
    writeDumpFile(transcriptPath, dumpSeq, eventName, "output", {
      event: eventName, session_id: sessionId, cwd, pid, data: inputData,
    });
  }

  // --- Routing ---

  // Auto-approve safe read-only Bash commands (skip Emacs round-trip)
  if (eventName === "PermissionRequest" && !config.noAutoApprove && isSafeBashCommand(inputData)) {
    yield* socket.send({
      event: "PermissionAutoApproved",
      session_id: sessionId,
      cwd,
      pid,
      data: {
        tool_name: inputData.tool_name,
        tool_use_id: inputData.tool_use_id,
        command: inputData.tool_input?.command,
      },
    });
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
    const ignored = yield* Effect.gen(function* () {
      const ignoreFile = join(cwd, ".claude", "gravity-ignored-sessions.json");
      const fileExists = yield* fs.exists(ignoreFile);
      if (!fileExists) return false;
      const content = yield* fs.readFile(ignoreFile).pipe(
        Effect.catch(() => Effect.succeed("[]"))
      );
      const ignoredArr = JSON.parse(content);
      return Array.isArray(ignoredArr) && ignoredArr.includes(sessionId);
    });
    if (ignored) {
      yield* Effect.logDebug(`Session ${sessionId} is ignored, passing ${eventName} through to TUI`);
      yield* socket.send({
        event: eventName,
        session_id: sessionId,
        cwd,
        pid,
        data: inputData,
        hook_input: rawHookInput,
      });
      yield* io.writeStdout(JSON.stringify({}) + "\n");
      return;
    }
  }

  // Send to Emacs — PermissionRequest and AskUserQuestionIntercept use bidirectional wait
  if (eventName === "PermissionRequest") {
    const toolName = inputData.tool_name || "unknown";
    yield* Effect.logWarning(`PermissionRequest: waiting for Emacs response [tool=${toolName}, session=${sessionId}]`);
    const response = yield* socket.sendAndWait({
      event: eventName,
      session_id: sessionId,
      cwd,
      pid,
      data: inputData,
      hook_input: rawHookInput,
    });

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
    const response = yield* socket.sendAndWait({
      event: "PreToolUse",
      session_id: sessionId,
      cwd,
      pid,
      data: inputData,
      hook_input: rawHookInput,
    });
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
        `[FINAL_CHECK_BEFORE_SEND] agent_stop_text=${typeof enrichedData.agent_stop_text === "string" ? `"${(enrichedData.agent_stop_text as string).substring(0, 40)}"` : enrichedData.agent_stop_text}`
      );
    }
    yield* socket.send({
      event: eventName,
      session_id: sessionId,
      cwd,
      pid,
      data: enrichedData,
      hook_input: rawHookInput,
    });
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

// EmacsSocket created with socket path from environment (same as legacy socket.ts)
const socketPathFromEnv = (() => {
  const gravitySock = process.env.CLAUDE_GRAVITY_SOCK;
  if (gravitySock) return gravitySock;
  const sockDir = process.env.CLAUDE_GRAVITY_SOCK_DIR;
  if (sockDir) return join(sockDir, "claude-gravity.sock");
  const home = process.env.HOME || "/tmp";
  return join(home, ".local", "state", "claude-gravity.sock");
})();

const FullLayer = Layer.mergeAll(MainLive, ConfigLayer, EmacsSocketLive(socketPathFromEnv));

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
