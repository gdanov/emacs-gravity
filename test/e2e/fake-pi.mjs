#!/usr/bin/env node
// Fake `pi` for the e2e harness. gravity-server's pi-driver spawns this
// (PI_BINARY_PATH) instead of the real pi binary and talks the pi RPC
// wire protocol over stdin/stdout:
//   stdout: newline-delimited JSON pi events / RPC responses
//   stdin : newline-delimited JSON commands (prompt, extension_ui_response…)
//
// Deterministic + out-of-band triggered (no LLM): on PI_E2E_TRIGGER file
// appearing it emits ONE `extension_ui_request` (method=input). When the
// driver writes the matching `extension_ui_response` back on stdin, the
// received value is written to PI_E2E_RESULT so the runner can assert the
// full round-trip (Emacs *Claude Pi Input* → action.question → pi).
//
// Args from the driver (`--mode rpc --session-dir … --thinking …`) are
// ignored. The process stays alive so the pi session stays healthy.

import { existsSync, writeFileSync, readFileSync } from "fs";

const TRIGGER = process.env.PI_E2E_TRIGGER;
const RESULT = process.env.PI_E2E_RESULT;
const UI_ID = "fp-ui-1";

const emit = (obj) => process.stdout.write(JSON.stringify(obj) + "\n");

// Handshake: a model_select makes the translator emit SessionStart so the
// pi session is fully real (startPiSession already created it; this keeps
// the sequence faithful to real pi).
emit({ type: "model_select", model: "fake-model", provider: "fake" });

let dialogSent = false;
let stdinBuf = "";

process.stdin.on("data", (chunk) => {
  stdinBuf += chunk.toString("utf8");
  let i;
  while ((i = stdinBuf.indexOf("\n")) !== -1) {
    const line = stdinBuf.slice(0, i).trim();
    stdinBuf = stdinBuf.slice(i + 1);
    if (!line) continue;
    let cmd;
    try { cmd = JSON.parse(line); } catch { continue; }
    // RPC commands the driver may probe with at startup — answer success
    // so the driver/session stays healthy.
    if (cmd.type === "get_state" || cmd.type === "get_session_stats"
        || cmd.type === "get_commands" || cmd.type === "get_available_models") {
      emit({ type: "response", command: cmd.type, id: cmd.id, success: true, data: {} });
      continue;
    }
    if (cmd.type === "extension_ui_response" && cmd.id === UI_ID) {
      // The full round-trip closed: record what the user typed in Emacs.
      const payload = cmd.cancelled
        ? { cancelled: true }
        : { value: cmd.value ?? null };
      if (RESULT) writeFileSync(RESULT, JSON.stringify(payload));
    }
    if (cmd.type === "prompt" && typeof cmd.message === "string") {
      // Drive a realistic transcript so the UI-flow scenario can observe
      // (a) immediate "thinking" (agent_start → server sets claude-status
      // "responding"), then (b) a populated session transcript (user
      // prompt + a tool call + assistant text + Stop). Two phases with a
      // short pause between so the test can deterministically assert the
      // mid-flight responding state before agent_end resolves to idle.
      const turnId = `t-${Date.now()}`;
      const toolCallId = `tc-${Date.now()}`;
      const promptText = cmd.message;
      emit({ type: "agent_start" });
      emit({ type: "message_start",
        message: { role: "user", content: [{ type: "text", text: promptText }] } });
      emit({ type: "turn_start", turn_id: turnId });
      emit({ type: "text_delta", delta: "Reading the file…" });
      emit({ type: "tool_execution_start",
        toolCallId, toolName: "Read", args: { file_path: "/work/proj/foo.el" } });
      setTimeout(() => {
        emit({ type: "tool_execution_end",
          toolCallId, toolName: "Read",
          result: "(defun foo () :ok)", isError: false });
        emit({ type: "text_delta", delta: " Done." });
        emit({ type: "turn_end", turn_id: turnId });
        emit({ type: "agent_end",
          messages: [{ role: "assistant",
                       usage: { input: 5, output: 7 } }] });
      }, 250);
    }
  }
});

// Watch for the out-of-band trigger; emit the input dialog exactly once.
const tick = setInterval(() => {
  if (!dialogSent && TRIGGER && existsSync(TRIGGER)) {
    dialogSent = true;
    emit({
      type: "extension_ui_request",
      id: UI_ID,
      method: "input",
      title: "Pi needs input",
      message: "What should the commit message be?",
      placeholder: "type a message",
      prefill: "chore: ",
    });
  }
}, 100);

// Stay alive (exiting marks the pi session dead). Clean exit on signals.
for (const s of ["SIGINT", "SIGTERM"]) {
  process.on(s, () => { clearInterval(tick); process.exit(0); });
}
