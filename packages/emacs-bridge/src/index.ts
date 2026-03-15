import { Effect, Layer, pipe } from "effect";
import { join } from "path";
import { log, initLogForSession } from "./log.js";
import type { HookData } from "@gravity/shared";
import { nextDumpSeq, writeDumpFile } from "./dump.js";
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
import { EmacsSocket, EmacsSocketLive } from "./services/emacs-socket.js";
import { HookSocketClient, HookSocketClientLive } from "./services/hook-socket.js";
import type { HookSocketMessage, HookEventName } from "@gravity/shared";

// Re-export types and transcript functions
export type { HookData, TokenUsage } from "@gravity/shared";
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
  const socket = yield* Effect.service(EmacsSocket);
  const hookSocket = yield* Effect.service(HookSocketClient);
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

  const transcriptPath = inputData.transcript_path;
  initLogForSession(transcriptPath);

  // Dump raw input (opt-in via CLAUDE_GRAVITY_DUMP=1 or CLAUDE_GRAVITY_DUMP_DIR)
  let dumpSeq: number | undefined;
  if (config.dumpEnabled && transcriptPath) {
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

  // Dump enriched output
  if (config.dumpEnabled && transcriptPath && dumpSeq !== undefined) {
    writeDumpFile(transcriptPath, dumpSeq, eventName, "output", {
      event: eventName, session_id: sessionId, cwd, pid, data: enrichedData,
    });
  }

  // --- Routing: all events go through gravity-server ---
  const isBidirectional = eventName === "PermissionRequest" || eventName === "AskUserQuestionIntercept";

  const hookMsg: HookSocketMessage = {
    event: eventName as HookEventName,
    session_id: sessionId,
    cwd,
    pid,
    source: "bridge",
    data: enrichedData,
    needs_response: isBidirectional,
  };

  if (isBidirectional) {
    // Bidirectional events routed through gravity-server.
    // The server manages inbox, terminal UI, and DENY-AS-APPROVE workaround.
    // Bridge just forwards and writes the response to stdout.
    const toolName = enrichedData.tool_name || "unknown";
    yield* Effect.logWarning(
      `${eventName}: waiting for gravity-server response [tool=${toolName}, session=${sessionId}]`
    );

    // Also notify Emacs socket (fire-and-forget, for UI updates)
    yield* socket.send({
      event: eventName,
      session_id: sessionId,
      cwd,
      pid,
      data: enrichedData,
      hook_input: rawHookInput,
    }).pipe(Effect.catch(() => Effect.void));

    const response = yield* hookSocket.sendAndWait(hookMsg).pipe(
      Effect.catch(() => {
        log(`${eventName}: hook socket sendAndWait failed`, "error");
        return Effect.succeed({});
      })
    );

    // Guard against empty response
    if (!response || Object.keys(response).length === 0) {
      yield* Effect.logError(
        `${eventName}: empty response from gravity-server [tool=${toolName}, session=${sessionId}] — writing {} to stdout`
      );
    }

    // Write response directly to stdout — server provides the full hookSpecificOutput format
    const responseStr = JSON.stringify(response) + "\n";
    const hardExit = setTimeout(() => {
      log(`${eventName}: hard exit timeout [tool=${toolName}]`, "error");
      process.exit(1);
    }, 5000);
    hardExit.unref();
    process.stdout.write(responseStr, () => {
      log(`${eventName}: stdout write callback OK [tool=${toolName}]`, "warn");
      setTimeout(() => process.exit(0), 50);
    });

  } else {
    // Fire-and-forget: send to hook socket and Emacs socket
    yield* hookSocket.send(hookMsg).pipe(
      Effect.catch(() => Effect.logDebug("Hook socket send failed (server unavailable)"))
    );

    yield* socket.send({
      event: eventName,
      session_id: sessionId,
      cwd,
      pid,
      data: enrichedData,
      hook_input: rawHookInput,
    });
    let hookResponse: Record<string, unknown> = {};
    if (eventName === "SessionStart") {
      const shortId = sessionId.slice(0, 8);
      hookResponse.systemMessage = `emacs-gravity: connected (session ${shortId}, pid ${pid})`;
    }
    yield* io.writeStdout(JSON.stringify(hookResponse) + "\n");
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

const FullLayer = Layer.mergeAll(MainLive, ConfigLayer, EmacsSocketLive(socketPathFromEnv), HookSocketClientLive);

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
