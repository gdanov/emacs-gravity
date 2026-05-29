// Hook scenario injector for the e2e harness.
//
// Replays hook events onto the gravity-server hook socket exactly as the
// bridge shim would. Two modes:
//  - fire-and-forget: write one NDJSON message, end the socket.
//  - bidirectional (PermissionRequest / AskUserQuestionIntercept): keep
//    the socket open; the server writes the decision back once a terminal
//    responds. Returns a promise for that response.
//
// Hook wire shape (matches bridge → hook socket):
//   { event, session_id, cwd, pid, data, needs_response }

import { createConnection } from "net";

/** Fire-and-forget hook event. Resolves when the socket closes. */
export function sendHook(sockPath, msg) {
  return new Promise((resolve, reject) => {
    const c = createConnection(sockPath, () => {
      c.write(JSON.stringify(msg) + "\n");
      c.end();
    });
    c.on("close", () => resolve());
    c.on("error", reject);
  });
}

/**
 * Bidirectional hook event. Keeps the hook socket open and resolves
 * `response` with the parsed JSON the server writes back (the decision a
 * terminal produced), or rejects on timeout.
 *
 * Returns { response: Promise<object>, close: () => void }.
 */
export function sendBidirectionalHook(sockPath, msg, timeoutMs = 15000) {
  let resolveFn, rejectFn, sentFn;
  const response = new Promise((res, rej) => { resolveFn = res; rejectFn = rej; });
  // Resolves once the hook message is actually on the wire. The runner
  // makes BLOCKING (spawnSync) emacsclient calls that starve Node's event
  // loop; callers must `await sent` before those so the server has the
  // event before Emacs is driven.
  const sent = new Promise((res) => { sentFn = res; });
  let buf = "";
  const c = createConnection(sockPath, () => {
    c.write(JSON.stringify({ ...msg, needs_response: true }) + "\n", () => sentFn());
  });
  const timer = setTimeout(() => {
    rejectFn(new Error(`bidirectional hook timed out after ${timeoutMs}ms: ${msg.event}/${msg?.data?.tool_name}`));
    c.destroy();
  }, timeoutMs);
  c.on("data", (chunk) => {
    buf += chunk.toString("utf8");
    const i = buf.indexOf("\n");
    if (i !== -1) {
      const line = buf.slice(0, i).trim();
      clearTimeout(timer);
      try { resolveFn(JSON.parse(line)); }
      catch { resolveFn({ _unparsed: line }); }
      c.end();
    }
  });
  c.on("error", (e) => { clearTimeout(timer); rejectFn(e); });
  return { response, sent, close: () => { clearTimeout(timer); c.destroy(); } };
}

/**
 * Replay an ordered hook sequence. Each step is either:
 *   { event, data?, session_id?, cwd?, pid? }                  (fire-forget)
 *   { event, bidirectional: true, ... }   → returns a handle in `bidi`
 *
 * Fire-and-forget steps await socket close before the next (preserves the
 * real ordering the bridge produces). Bidirectional steps are launched and
 * their handles returned so the caller can answer via Emacs then await.
 */
export async function replay(hookSock, steps, defaults = {}) {
  const bidi = [];
  for (const step of steps) {
    const msg = {
      event: step.event,
      session_id: step.session_id ?? defaults.session_id ?? "e2e-sess",
      cwd: step.cwd ?? defaults.cwd ?? "/workspace",
      pid: step.pid ?? defaults.pid ?? 4242,
      data: step.data ?? {},
      needs_response: !!step.bidirectional,
    };
    if (step.bidirectional) {
      const handle = sendBidirectionalHook(hookSock, msg, step.timeoutMs);
      // Guarantee the event is on the wire before the caller starts
      // making blocking emacsclient calls (which freeze this event loop).
      await handle.sent;
      bidi.push({ step, handle });
    } else {
      await sendHook(hookSock, msg);
    }
  }
  return bidi;
}
