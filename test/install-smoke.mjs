#!/usr/bin/env node
// Standalone socket smoke tests for gravity-server.
// Expects server already running. Reads socket paths from env vars.
// Usage: node test/install-smoke.mjs [level3|level4|level6]

import { createConnection } from "node:net";
import { existsSync, readFileSync } from "node:fs";
import assert from "node:assert/strict";

const HOOK_SOCK = process.env.GRAVITY_HOOK_SOCK;
const TERMINAL_SOCK = process.env.GRAVITY_TERMINAL_SOCK;
const PID_FILE = process.env.GRAVITY_PID_FILE;

if (!HOOK_SOCK || !TERMINAL_SOCK) {
  console.error("GRAVITY_HOOK_SOCK and GRAVITY_TERMINAL_SOCK must be set");
  process.exit(1);
}

const level = process.argv[2] || "all";
let passed = 0;
let failed = 0;

// ── Helpers ──────────────────────────────────────────────────────────

function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

/** Connect to a Unix socket, return the socket */
function connect(sockPath) {
  return new Promise((resolve, reject) => {
    const sock = createConnection(sockPath, () => resolve(sock));
    sock.on("error", reject);
  });
}

/** Read NDJSON messages from socket until it closes or timeout */
function readMessages(socket, timeoutMs = 500) {
  return new Promise((resolve) => {
    const messages = [];
    let buffer = "";
    const onData = (chunk) => {
      buffer += chunk.toString();
      let idx;
      while ((idx = buffer.indexOf("\n")) !== -1) {
        const line = buffer.substring(0, idx).trim();
        buffer = buffer.substring(idx + 1);
        if (line.length > 0) messages.push(JSON.parse(line));
      }
    };
    socket.on("data", onData);
    setTimeout(() => {
      socket.removeListener("data", onData);
      resolve(messages);
    }, timeoutMs);
  });
}

/** Send NDJSON to hook socket (fire-and-forget) */
function sendHookEvent(msg) {
  return new Promise((resolve, reject) => {
    const client = createConnection(HOOK_SOCK, () => {
      client.write(JSON.stringify(msg) + "\n");
      client.end();
    });
    client.on("close", () => resolve());
    client.on("error", reject);
  });
}

/** Send NDJSON message to a terminal socket */
function sendTerminalMessage(socket, msg) {
  socket.write(JSON.stringify(msg) + "\n");
}

async function test(name, fn) {
  try {
    await fn();
    console.log(`  PASS  ${name}`);
    passed++;
  } catch (err) {
    console.log(`  FAIL  ${name}`);
    console.log(`        ${err.message}`);
    failed++;
  }
}

// ── Level 3: Server Lifecycle ────────────────────────────────────────

async function runLevel3() {
  console.log("\n=== Level 3: Server Lifecycle ===");

  await test("Both sockets exist", () => {
    assert.ok(existsSync(HOOK_SOCK), `Hook socket not found: ${HOOK_SOCK}`);
    assert.ok(existsSync(TERMINAL_SOCK), `Terminal socket not found: ${TERMINAL_SOCK}`);
  });

  if (PID_FILE) {
    await test("PID file has valid PID", () => {
      assert.ok(existsSync(PID_FILE), `PID file not found: ${PID_FILE}`);
      const pid = parseInt(readFileSync(PID_FILE, "utf8").trim(), 10);
      assert.ok(pid > 0, `Invalid PID: ${pid}`);
    });
  }

  await test("Terminal receives overview.snapshot on connect", async () => {
    const sock = await connect(TERMINAL_SOCK);
    const msgs = await readMessages(sock, 1000);
    sock.end();
    assert.ok(msgs.length > 0, "No messages received");
    const overview = msgs.find((m) => m.type === "overview.snapshot");
    assert.ok(overview, `Expected overview.snapshot, got: ${msgs.map((m) => m.type).join(", ")}`);
  });
}

// ── Level 4: Hook event creates session ──────────────────────────────

async function runLevel4() {
  console.log("\n=== Level 4: Hook Event Creates Session ===");

  await test("SessionStart creates session visible to terminal", async () => {
    // Send SessionStart
    await sendHookEvent({
      event: "SessionStart",
      session_id: "smoke-s1",
      cwd: "/tmp/test-project",
      pid: process.pid,
      source: "bridge",
      data: {},
      needs_response: false,
    });
    await sleep(200);

    // Connect terminal, check overview
    const sock = await connect(TERMINAL_SOCK);
    sendTerminalMessage(sock, { type: "request.overview" });
    const msgs = await readMessages(sock, 1000);
    sock.end();

    const overview = msgs.find((m) => m.type === "overview.snapshot");
    assert.ok(overview, "No overview.snapshot received");
    assert.ok(overview.projects.length > 0, "No projects in overview");
    const allSessions = overview.projects.flatMap((p) => p.sessions);
    const found = allSessions.find((s) => s.sessionId === "smoke-s1");
    assert.ok(found, "Session smoke-s1 not found in overview");
  });
}

// ── Level 6: Full turn lifecycle ─────────────────────────────────────

async function runLevel6() {
  console.log("\n=== Level 6: End-to-End Full Turn ===");

  await test("Full turn lifecycle (hook→server→terminal)", async () => {
    const sid = "smoke-e2e";

    // SessionStart
    await sendHookEvent({
      event: "SessionStart",
      session_id: sid,
      cwd: "/tmp/test-project",
      pid: process.pid,
      source: "bridge",
      data: {},
      needs_response: false,
    });
    await sleep(100);

    // Connect terminal + subscribe
    const sock = await connect(TERMINAL_SOCK);
    sendTerminalMessage(sock, { type: "request.session", sessionId: sid });
    await sleep(200);

    // Start collecting patches
    const allPatches = [];
    let buf = "";
    sock.on("data", (chunk) => {
      buf += chunk.toString();
      let idx;
      while ((idx = buf.indexOf("\n")) !== -1) {
        const line = buf.substring(0, idx).trim();
        buf = buf.substring(idx + 1);
        if (line.length > 0) {
          try {
            const msg = JSON.parse(line);
            if (msg.type === "session.update" && msg.patches) {
              for (const p of msg.patches) allPatches.push(p.op);
            }
          } catch {}
        }
      }
    });

    // Drain initial messages
    await sleep(200);
    allPatches.length = 0;

    // UserPromptSubmit
    await sendHookEvent({
      event: "UserPromptSubmit",
      session_id: sid,
      cwd: "/tmp/test-project",
      pid: process.pid,
      source: "bridge",
      data: { prompt: "Fix the bug" },
      needs_response: false,
    });
    await sleep(100);

    // PreToolUse
    await sendHookEvent({
      event: "PreToolUse",
      session_id: sid,
      cwd: "/tmp/test-project",
      pid: process.pid,
      source: "bridge",
      data: { tool_name: "Read", tool_use_id: "smoke-t1", tool_input: { file_path: "/src/app.ts" } },
      needs_response: false,
    });
    await sleep(100);

    // PostToolUse
    await sendHookEvent({
      event: "PostToolUse",
      session_id: sid,
      cwd: "/tmp/test-project",
      pid: process.pid,
      source: "bridge",
      data: { tool_name: "Read", tool_use_id: "smoke-t1", tool_response: "file contents here" },
      needs_response: false,
    });
    await sleep(100);

    // Stop
    await sendHookEvent({
      event: "Stop",
      session_id: sid,
      cwd: "/tmp/test-project",
      pid: process.pid,
      source: "bridge",
      data: {
        stop_text: "Fixed it",
        token_usage: { input_tokens: 100, output_tokens: 50, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 },
      },
      needs_response: false,
    });
    await sleep(300);

    sock.end();

    // Verify patch sequence
    const expected = ["add_turn", "add_prompt", "set_claude_status", "add_tool", "complete_tool", "set_turn_stop", "set_token_usage"];
    for (const op of expected) {
      assert.ok(allPatches.includes(op), `Missing patch op: ${op}. Got: ${[...new Set(allPatches)].join(", ")}`);
    }
  });
}

// ── Main ─────────────────────────────────────────────────────────────

async function main() {
  try {
    if (level === "all" || level === "level3") await runLevel3();
    if (level === "all" || level === "level4") await runLevel4();
    if (level === "all" || level === "level6") await runLevel6();
  } catch (err) {
    console.error("Unexpected error:", err);
    failed++;
  }

  console.log(`\n  Node.js smoke: ${passed} passed, ${failed} failed`);
  process.exit(failed > 0 ? 1 : 0);
}

main();
