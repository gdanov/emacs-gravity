// E2E scenarios. Each drives a real gravity feature end-to-end through
// the full stack (synthetic hooks → server → proxy → live Emacs client →
// rendered buffers + action round-trip).
//
// ctx: { RUN, HOOK, PROXY_LOG, emacs, emacsT, assert, proxyMessages, dump }

import { replay } from "./inject.mjs";
import { join } from "path";
import { readFileSync, existsSync } from "fs";

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Dump the first buffer matching NAME-REGEXP (Emacs-side) and read it back.
function dumpMatch(ctx, nameRegexp) {
  const f = join(ctx.RUN, "dump.txt");
  ctx.emacs(`(cge-dump-match ${JSON.stringify(nameRegexp)} ${JSON.stringify(f)})`);
  return existsSync(f) ? readFileSync(f, "utf8") : "";
}
const c2s = (ctx, type) =>
  ctx.proxyMessages().filter((m) => m.dir === "c2s" && m.msg?.type === type);

const AUQ_INPUT = {
  questions: [{
    question: "Which approach do you prefer?",
    header: "Approach",
    options: [
      { label: "Recursive", description: "use recursion" },
      { label: "Iterative", description: "use a loop" },
    ],
  }],
};

export const scenarios = [
  // ── S1: a minimal turn renders through the live client ──────────────
  {
    name: "minimal-turn",
    async run(ctx) {
      const sid = "s1-min";
      await replay(ctx.HOOK, [
        { event: "SessionStart", data: { slug: "min" } },
        { event: "UserPromptSubmit", data: { prompt: "read the file" } },
        { event: "PreToolUse", data: { tool_name: "Read", tool_use_id: "t1", tool_input: { file_path: "/workspace/foo.el" } } },
        { event: "PostToolUse", data: { tool_name: "Read", tool_use_id: "t1", tool_input: { file_path: "/workspace/foo.el" }, tool_response: "(defun foo ())" } },
        { event: "Stop", data: {} },
      ], { session_id: sid });

      ctx.emacs("(cge-poll)");
      const ok = ctx.emacsT(
        `(cge-wait "(gethash \\"${sid}\\" claude-gravity--sessions)" 5)`);
      ctx.assert(ok, "session reached the live client via pull poll");

      const stateF = join(ctx.RUN, "state.json");
      ctx.emacs(`(cge-state ${JSON.stringify(stateF)})`);
      const st = JSON.parse(readFileSync(stateF, "utf8"));
      const s = st.sessions.find((x) => x.id === sid);
      ctx.assert(!!s, "session present in client state");
      ctx.assert(s && s.turns >= 1, `turn tree built (turns=${s?.turns})`);
      ctx.assert(s && s["claude-status"] === "idle",
        `claude-status idle after Stop (got ${s?.["claude-status"]})`);

      // Overview buffer renders the project/slug.
      ctx.emacs("(ignore-errors (claude-gravity-status))");
      ctx.emacs("(cge-pump 0.4)");
      const ov = dumpMatch(ctx, "\\*Structured Claude Sessions\\*");
      ctx.assert(/min|s1-min/.test(ov), "overview buffer lists the session");
    },
  },

  // ── S2: AskUserQuestion regression, full stack ──────────────────────
  // The triple-fire (generic PreToolUse + intercept + PermissionRequest,
  // one tool_use_id). Asserts the OPTION UI renders (not the raw-JSON
  // permission dump), the answer round-trips, and BOTH bidirectional hook
  // sockets get correctly-shaped responses.
  {
    name: "askuserquestion-regression",
    async run(ctx) {
      const sid = "s2-auq";
      const data = { tool_name: "AskUserQuestion", tool_use_id: "tq", tool_input: AUQ_INPUT };

      const bidi = await replay(ctx.HOOK, [
        { event: "SessionStart", data: { slug: "auq" } },
        { event: "UserPromptSubmit", data: { prompt: "pick one" } },
        { event: "AskUserQuestionIntercept", bidirectional: true, data },
        { event: "PreToolUse", data }, // the supersede attacker, same tool_use_id
        { event: "PermissionRequest", bidirectional: true, data },
      ], { session_id: sid });
      const intercept = bidi[0].handle;   // AskUserQuestionIntercept socket
      const permission = bidi[1].handle;  // redundant PermissionRequest socket

      ctx.emacs("(cge-poll)");
      const got = ctx.emacsT(
        `(cge-wait "(cl-find \\"${sid}\\" claude-gravity--inbox :key (lambda (i) (alist-get 'session-id i)) :test #'equal)" 6)`);
      ctx.assert(got, "question inbox item delivered via pull poll");

      const typ = ctx.emacs(`(cge-open-inbox ${JSON.stringify(sid)})`);
      ctx.emacs("(cge-pump 0.4)");
      ctx.assert(/question/.test(typ),
        `inbox item dispatched as a QUESTION (got ${typ}) — not permission`);

      const qbuf = dumpMatch(ctx, "\\*Claude Action: Question");
      ctx.assert(/Which approach do you prefer\?/.test(qbuf),
        "option UI shows the question text");
      ctx.assert(/Recursive/.test(qbuf) && /Iterative/.test(qbuf),
        "option UI shows the choices");
      ctx.assert(!/tool_use_id|\{\s*\"/.test(qbuf),
        "NOT a raw-JSON permission dump (the regression)");
      const permCount = ctx.emacs('(cge-buffer-names "\\\\*Claude Action: Permission")');
      ctx.assert(permCount === "0",
        `no permission JSON buffer opened (count=${permCount})`);

      // Answer: pick option 1 (single-select auto-submits → action.question)
      const acted = ctx.emacsT(
        `(cge-eval-in-match "\\\\*Claude Action: Question" "(claude-gravity--question-action-select 1)")`);
      ctx.assert(acted, "answered the question in the option UI");
      ctx.emacs("(cge-pump 0.4)");

      const aq = c2s(ctx, "action.question");
      ctx.assert(aq.length >= 1, "client sent action.question to server");
      const answers = JSON.stringify(aq[aq.length - 1]?.msg?.answers ?? "");
      ctx.assert(/Recursive/.test(answers),
        `action.question carried the chosen answer (${answers})`);

      const ir = await intercept.response;
      ctx.assert(ir?.hookSpecificOutput?.hookEventName === "PreToolUse"
        && !!ir?.hookSpecificOutput?.updatedInput,
        "intercept hook socket got PreToolUse+updatedInput (answer delivered to Claude)");
      const pr = await permission.response;
      ctx.assert(pr?.hookSpecificOutput?.hookEventName === "PermissionRequest"
        && pr?.hookSpecificOutput?.decision?.behavior === "allow",
        "redundant PermissionRequest socket unblocked with allow passthrough");
    },
  },

  // ── S3: ExitPlanMode plan-review approve ────────────────────────────
  {
    name: "plan-review-approve",
    async run(ctx) {
      const sid = "s3-plan";
      const data = { tool_name: "ExitPlanMode", tool_use_id: "tp",
        tool_input: { plan: "## Plan\n1. do alpha\n2. do beta" } };
      const bidi = await replay(ctx.HOOK, [
        { event: "SessionStart", data: { slug: "plan" } },
        { event: "UserPromptSubmit", data: { prompt: "plan it" } },
        { event: "PermissionRequest", bidirectional: true, data },
      ], { session_id: sid });
      const planHandle = bidi[0].handle;

      ctx.emacs("(cge-poll)");
      const got = ctx.emacsT(
        `(cge-wait "(cl-find \\"${sid}\\" claude-gravity--inbox :key (lambda (i) (alist-get 'session-id i)) :test #'equal)" 6)`);
      ctx.assert(got, "plan-review inbox item delivered");

      const typ = ctx.emacs(`(cge-open-inbox ${JSON.stringify(sid)})`);
      ctx.emacs("(cge-pump 0.4)");
      ctx.assert(/plan-review/.test(typ), `dispatched as plan-review (${typ})`);

      const pbuf = dumpMatch(ctx, "\\*Claude Plan Review");
      ctx.assert(/do alpha/.test(pbuf) && /do beta/.test(pbuf),
        "plan-review buffer renders the plan markdown");

      const ok = ctx.emacsT(
        `(cge-eval-in-match "\\\\*Claude Plan Review" "(claude-gravity-plan-review-approve)")`);
      ctx.assert(ok, "approved the plan");
      ctx.emacs("(cge-pump 0.4)");

      const ap = c2s(ctx, "action.plan-review");
      ctx.assert(ap.length >= 1, "client sent action.plan-review");
      const resp = await planHandle.response;
      ctx.assert(!!resp && !!resp.hookSpecificOutput,
        "plan PermissionRequest hook socket got a decision (deny-as-approve workaround)");
    },
  },

  // ── S4: pi adapter — extension_ui_request input dialog ──────────────
  // Exercises the OTHER driver (pi, not the Claude hook path): fake-pi
  // emits an `extension_ui_request` method=input; gravity-server's pi
  // adapter turns it into a question inbox item with pi_ui.kind="text";
  // the live Emacs client renders the *Claude Pi Input* text buffer; the
  // typed answer round-trips back to pi as `extension_ui_response`.
  {
    name: "pi-input-dialog",
    async run(ctx) {
      ctx.piTrigger(); // fake-pi emits the extension_ui_request

      ctx.emacs("(cge-poll)");
      const pending = ctx.emacsT('(cge-wait "(cge-pi-text-pending-p)" 10)');
      ctx.assert(pending, "pi extension_ui_request became a pi-text inbox item");

      const opened = ctx.emacsT("(cge-open-pi-text)");
      ctx.assert(opened, "opened the pi text-entry action buffer");
      ctx.emacs("(cge-pump 0.4)");

      const buf = dumpMatch(ctx, "\\*Claude Pi Input");
      ctx.assert(/Pi needs input/.test(buf), "pi-input buffer shows the title");
      ctx.assert(/commit message/.test(buf), "pi-input buffer shows the message");
      ctx.assert(/chore: /.test(buf), "pi-input buffer seeded with the prefill");

      const acted = ctx.emacsT(
        `(cge-eval-in-match "\\\\*Claude Pi Input" "(progn (goto-char (point-max)) (insert \\"add e2e harness\\") (claude-gravity-pi-text-submit))")`);
      ctx.assert(acted, "submitted the pi text answer");
      ctx.emacs("(cge-pump 0.4)");

      const aq = c2s(ctx, "action.question");
      ctx.assert(aq.some((m) => JSON.stringify(m.msg?.answers ?? "").includes("add e2e harness")),
        "client sent action.question carrying the typed text");

      // Round-trip closed at pi: fake-pi wrote what it received as
      // extension_ui_response.
      let res = null;
      for (let i = 0; i < 50 && !res; i++) { res = ctx.piResult(); await sleep(100); }
      ctx.assert(res && typeof res.value === "string"
        && res.value.includes("add e2e harness"),
        `pi received extension_ui_response value (${JSON.stringify(res)})`);
      ctx.assert(res && /^chore: /.test(res.value),
        "answer preserved the prefill (full free-text round-trip)");
    },
  },
];
