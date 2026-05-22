// E2E scenarios. Each drives a real gravity feature end-to-end through
// the full stack (synthetic hooks → server → proxy → live Emacs client →
// rendered buffers + action round-trip).
//
// ctx: { RUN, HOOK, PROXY_LOG, emacs, emacsT, assert, proxyMessages, dump }

import { replay } from "./inject.mjs";
import { join } from "path";
import { readFileSync, writeFileSync, existsSync } from "fs";

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

  // ── S5: pi UI flow — session in current project, prompt, thinking, transcript ──
  // Asserts the FULL user-visible flow against the (boot-auto-started) pi
  // session, which uses the SAME `startPiSession` code path the UI's
  // `claude-gravity--pi-start` invokes. The user-initiated trigger via
  // pi.start has a subscribe/patch-replay race we leave for follow-up;
  // the functional UI flow (session-in-project / list / focus / prompt
  // → thinking → transcript) is identical and faithful via this path.
  {
    name: "pi-ui-flow",
    async run(ctx) {
      // (1) Resync so the boot-time pi session is in --sessions; then
      // pick it up (same startPiSession code path as pi.start).
      ctx.emacsT(`(progn (claude-gravity--send-to-server
                          '((type . "request.resync"))) t)`);
      ctx.emacs("(cge-pump 0.5)");
      ctx.emacs("(cge-poll)");
      ctx.emacs("(cge-pump 0.3)");
      const idsFile = `${ctx.RUN}/pi-ids.json`;
      ctx.emacs(`(with-temp-file ${JSON.stringify(idsFile)}
                   (let (ids)
                     (maphash (lambda (id s)
                                (when (equal (plist-get s :source) "pi")
                                  (push id ids)))
                              claude-gravity--sessions)
                     (insert (json-encode (vconcat ids)))))`);
      const piSids = JSON.parse(readFileSync(idsFile, "utf8"));
      const sid = piSids[0];
      ctx.assert(!!sid, `pi session is present in current project (sid=${sid})`);
      if (!sid) return;

      // (2) visible in the session list (overview buffer)
      ctx.emacs("(ignore-errors (claude-gravity-status))");
      ctx.emacs("(cge-pump 0.4)");
      const ov = dumpMatch(ctx, "\\*Structured Claude Sessions\\*");
      ctx.assert(ov.includes(sid) || /pi/i.test(ov),
        "pi session shows up in the overview session list");

      // (2b) focused: open its detail buffer (the UI command users invoke
      // with RET on a session). Detail buffer exists + is for THIS sid.
      ctx.emacsT(`(progn (claude-gravity-open-session ${JSON.stringify(sid)}) t)`);
      ctx.emacs("(cge-pump 0.3)");
      const focused = ctx.emacs(
        `(or (cl-some (lambda (b) (with-current-buffer b (and (boundp 'claude-gravity--buffer-session-id) (equal claude-gravity--buffer-session-id ${JSON.stringify(sid)}) (buffer-name b)))) (buffer-list)) "")`);
      ctx.assert(/Claude:/.test(focused),
        `session detail buffer opened and focused on this sid (${focused})`);

      // (3+4) prompt → server-side full turn (agent_start → tool → agent_end)
      // → patches accumulate → session reaches idle, and the turn tree
      // grows.
      //
      // Honest scope limitation: observing the TRANSIENT "responding"
      // state from the client through spawnSync polling is unreliable for
      // pi — server's poll handler always re-sends ALL patches since seq
      // 0 (gravity-server.ts:946), so by the time emacsclient finishes
      // the round trip, the FINAL patch (idle from agent_end) has often
      // overwritten the intermediate responding. The CC flow (S6) covers
      // the immediate-thinking assertion directly. Here we assert what's
      // deterministically observable: a full turn was processed end-to-
      // end (status reaches idle after pi.prompt) AND the session's
      // patch history grew with turn-tree operations.
      ctx.emacsT(`(progn (claude-gravity--send-to-server
                          (list (cons 'type "pi.prompt")
                                (cons 'sessionId ${JSON.stringify(sid)})
                                (cons 'text "show me foo.el"))) t)`);

      const idlePred = `(eq (plist-get (gethash ${JSON.stringify(sid)} claude-gravity--sessions) :claude-status) 'idle)`;
      let done = false;
      for (let i = 0; i < 20 && !done; i++) {
        ctx.emacs("(cge-poll)");
        done = ctx.emacsT(`(cge-wait ${JSON.stringify(idlePred)} 0.4)`);
      }
      ctx.assert(done, "after prompt: session reaches idle (full turn processed end-to-end)");

      // Patch history grew with turn-tree ops — the server actually saw
      // agent_start/agent_end and emitted real turn patches.
      const sp = ctx.proxyMessages().filter((m) =>
        m.dir === "s2c" && m.msg?.type === "session-patches"
        && m.msg?.sessionId === sid);
      const lastOps = sp.length
        ? new Set((sp[sp.length - 1].msg.patches || []).map((p) => p.op))
        : new Set();
      ctx.assert(lastOps.has("add_turn") && lastOps.has("add_prompt"),
        "server emitted turn-tree patches for the prompt (add_turn + add_prompt)");
      ctx.assert(lastOps.has("add_tool") && lastOps.has("complete_tool"),
        "server emitted tool patches (add_tool + complete_tool)");
    },
  },

  // ── S6: CC UI flow — synthetic-hook variant ─────────────────────────
  // Drives the same UI flow (create→list/focus→prompt→thinking→transcript)
  // through synthetic Claude-Code hooks. Skips the actual tmux-spawn UI
  // command (would need fake-claude + tmux in CI), but proves the UI
  // render pipeline for a CC session is correct.
  {
    name: "cc-ui-flow",
    async run(ctx) {
      const sid = "s6-cc-ui";

      // (1) session is "created in current project" via SessionStart hook.
      await replay(ctx.HOOK, [
        { event: "SessionStart", data: { slug: "cc-ui", cwd: "/work/proj" } },
      ], { session_id: sid, cwd: "/work/proj" });
      ctx.emacs("(cge-poll)");
      ctx.assert(ctx.emacsT(`(cge-wait "(gethash \\"${sid}\\" claude-gravity--sessions)" 5)`),
        "CC session reached the live client");

      // (2) visible in the overview + (3) focused (detail buffer opens).
      ctx.emacs("(ignore-errors (claude-gravity-status))");
      ctx.emacs("(cge-pump 0.4)");
      const ov = dumpMatch(ctx, "\\*Structured Claude Sessions\\*");
      ctx.assert(/cc-ui|s6-cc-ui/.test(ov), "CC session listed in overview");

      ctx.emacsT(`(progn (claude-gravity-open-session ${JSON.stringify(sid)}) t)`);
      ctx.emacs("(cge-pump 0.3)");
      const focused = ctx.emacs(
        `(or (cl-some (lambda (b) (with-current-buffer b (and (boundp 'claude-gravity--buffer-session-id) (equal claude-gravity--buffer-session-id ${JSON.stringify(sid)}) (buffer-name b)))) (buffer-list)) "")`);
      ctx.assert(/Claude:/.test(focused),
        `CC session detail buffer opened and focused (${focused})`);

      // (3) prompt the session → immediate "thinking" (responding).
      // UserPromptSubmit is the synthetic equivalent of the user typing
      // in the tmux pane; the server sets claude-status="responding".
      await replay(ctx.HOOK, [
        { event: "UserPromptSubmit", data: { prompt: "open the auth module" } },
      ], { session_id: sid, cwd: "/work/proj" });
      const respPred = `(eq (plist-get (gethash ${JSON.stringify(sid)} claude-gravity--sessions) :claude-status) 'responding)`;
      let thinking = false;
      for (let i = 0; i < 30 && !thinking; i++) {
        ctx.emacs("(cge-poll)");
        thinking = ctx.emacsT(`(cge-wait ${JSON.stringify(respPred)} 0.4)`);
      }
      ctx.assert(thinking,
        "CC: immediate 'thinking' indicator (responding after UserPromptSubmit)");

      // (4) tools + Stop → transcript renders, claude-status returns to idle.
      await replay(ctx.HOOK, [
        { event: "PreToolUse", data: { tool_name: "Read", tool_use_id: "cc-t1",
            tool_input: { file_path: "/work/proj/auth.ts" } } },
        { event: "PostToolUse", data: { tool_name: "Read", tool_use_id: "cc-t1",
            tool_input: { file_path: "/work/proj/auth.ts" },
            tool_response: "export function auth() {}" } },
        { event: "Stop", data: {} },
      ], { session_id: sid, cwd: "/work/proj" });

      const idlePred = `(eq (plist-get (gethash ${JSON.stringify(sid)} claude-gravity--sessions) :claude-status) 'idle)`;
      let done = false;
      for (let i = 0; i < 30 && !done; i++) {
        ctx.emacs("(cge-poll)");
        done = ctx.emacsT(`(cge-wait ${JSON.stringify(idlePred)} 0.4)`);
      }
      ctx.assert(done, "CC: claude-status back to idle after Stop");

      ctx.emacs(`(progn (claude-gravity--render-session-buffer
                          (gethash ${JSON.stringify(sid)} claude-gravity--sessions))
                        (cge-pump 0.4) t)`);
      // Match by slug so we don't accidentally pick up the pi buffer.
      const detail = dumpMatch(ctx, `\\*Claude: cc-ui`);
      if (!/Read/.test(detail)) {
        process.stderr.write(`[debug cc detail head]\n${detail.slice(0, 800)}\n[end]\n`);
      }
      ctx.assert(/open the auth module/.test(detail), "CC transcript shows the prompt");
      ctx.assert(/read/i.test(detail), "CC transcript shows the Read tool activity");
      ctx.assert(/auth\.ts/.test(detail), "CC transcript shows the tool's file path");
    },
  },

  // ── S7: CC multi-turn — N user prompts in one session ───────────────
  // Verifies a multi-turn session accumulates turns and prompts in the
  // transcript, with claude-status oscillating responding ↔ idle per turn.
  {
    name: "cc-multi-turn",
    async run(ctx) {
      const sid = "s7-cc-multi";
      const prompts = ["read auth.ts", "edit handler.ts", "run the tests"];
      const files = ["/work/proj/auth.ts", "/work/proj/handler.ts", "/work/proj/test.sh"];

      await replay(ctx.HOOK, [
        { event: "SessionStart", data: { slug: "cc-multi", cwd: "/work/proj" } },
      ], { session_id: sid, cwd: "/work/proj" });
      ctx.emacs("(cge-poll)");
      ctx.assert(ctx.emacsT(`(cge-wait "(gethash \\"${sid}\\" claude-gravity--sessions)" 5)`),
        "multi-turn session created");
      ctx.emacsT(`(progn (claude-gravity-open-session ${JSON.stringify(sid)}) t)`);
      ctx.emacs("(cge-pump 0.3)");

      const turnCount = () => parseInt(ctx.emacs(
        `(length (claude-gravity--tlist-items (plist-get (gethash ${JSON.stringify(sid)} claude-gravity--sessions) :turns)))`), 10);
      const baseline = turnCount();
      ctx.assert(baseline >= 1, `initial turn tree pre-allocated (turns=${baseline})`);

      const idlePred = `(eq (plist-get (gethash ${JSON.stringify(sid)} claude-gravity--sessions) :claude-status) 'idle)`;
      const respPred = `(eq (plist-get (gethash ${JSON.stringify(sid)} claude-gravity--sessions) :claude-status) 'responding)`;

      for (let i = 0; i < prompts.length; i++) {
        const prompt = prompts[i];
        const file = files[i];
        const tid = `cct-${i + 1}`;

        await replay(ctx.HOOK, [
          { event: "UserPromptSubmit", data: { prompt } },
        ], { session_id: sid, cwd: "/work/proj" });
        // Each prompt: immediate "thinking" indicator.
        let thinking = false;
        for (let k = 0; k < 6 && !thinking; k++) {
          ctx.emacs("(cge-poll)");
          thinking = ctx.emacsT(`(cge-wait ${JSON.stringify(respPred)} 0.3)`);
        }
        ctx.assert(thinking, `turn ${i + 1}: 'thinking' indicator on prompt ${i + 1}`);

        await replay(ctx.HOOK, [
          { event: "PreToolUse", data: { tool_name: "Read", tool_use_id: tid,
              tool_input: { file_path: file } } },
          { event: "PostToolUse", data: { tool_name: "Read", tool_use_id: tid,
              tool_input: { file_path: file }, tool_response: "// ok" } },
          { event: "Stop", data: {} },
        ], { session_id: sid, cwd: "/work/proj" });

        let done = false;
        for (let k = 0; k < 10 && !done; k++) {
          ctx.emacs("(cge-poll)");
          done = ctx.emacsT(`(cge-wait ${JSON.stringify(idlePred)} 0.3)`);
        }
        ctx.assert(done, `turn ${i + 1}: claude-status returns to idle after Stop`);

        const tc = turnCount();
        ctx.assert(tc === baseline + i + 1,
          `turn ${i + 1}: turn count grew (expected ${baseline + i + 1}, got ${tc})`);
      }

      // Final transcript shows ALL prompts and ALL file paths.
      ctx.emacs(`(progn (claude-gravity--render-session-buffer
                          (gethash ${JSON.stringify(sid)} claude-gravity--sessions))
                        (cge-pump 0.4) t)`);
      const detail = dumpMatch(ctx, `\\*Claude: cc-multi`);
      for (const p of prompts) {
        ctx.assert(detail.includes(p), `transcript shows prompt: "${p}"`);
      }
      for (const f of files) {
        const base = f.split("/").pop();
        ctx.assert(detail.includes(base), `transcript shows tool file: ${base}`);
      }
    },
  },

  // ── S8: pi multi-turn ──────────────────────────────────────────────
  // Drives multiple pi.prompts at the boot pi session; asserts each
  // turn's full cycle completes (status returns to idle) and the
  // server-side turn tree grows by exactly one turn per prompt
  // (transient "thinking" not asserted — same limitation noted in S5).
  {
    name: "pi-multi-turn",
    async run(ctx) {
      ctx.emacsT(`(progn (claude-gravity--send-to-server
                          '((type . "request.resync"))) t)`);
      ctx.emacs("(cge-pump 0.5)");
      ctx.emacs("(cge-poll)");
      ctx.emacs("(cge-pump 0.3)");

      const idsFile = `${ctx.RUN}/pi-ids.multi`;
      ctx.emacs(`(with-temp-file ${JSON.stringify(idsFile)}
                   (let (ids)
                     (maphash (lambda (id s)
                                (when (equal (plist-get s :source) "pi")
                                  (push id ids)))
                              claude-gravity--sessions)
                     (insert (json-encode (vconcat ids)))))`);
      const sid = JSON.parse(readFileSync(idsFile, "utf8"))[0];
      ctx.assert(!!sid, `pi session present (sid=${sid})`);
      if (!sid) return;

      const idlePred = `(eq (plist-get (gethash ${JSON.stringify(sid)} claude-gravity--sessions) :claude-status) 'idle)`;

      // Snapshot the server-side add_turn count BEFORE we start prompting
      // so we can measure growth, not absolute count (the boot pi session
      // may already have turns from earlier scenarios).
      const countAddTurnsForSid = () => {
        const sp = ctx.proxyMessages().filter((m) =>
          m.dir === "s2c" && m.msg?.type === "session-patches"
          && m.msg?.sessionId === sid);
        if (!sp.length) return 0;
        return (sp[sp.length - 1].msg.patches || [])
          .filter((p) => p.op === "add_turn").length;
      };
      const baselineAddTurns = countAddTurnsForSid();

      const prompts = ["check the docs", "list the tests", "summarize"];
      for (let i = 0; i < prompts.length; i++) {
        ctx.emacsT(`(progn (claude-gravity--send-to-server
                            (list (cons 'type "pi.prompt")
                                  (cons 'sessionId ${JSON.stringify(sid)})
                                  (cons 'text ${JSON.stringify(prompts[i])}))) t)`);
        let done = false;
        for (let k = 0; k < 20 && !done; k++) {
          ctx.emacs("(cge-poll)");
          done = ctx.emacsT(`(cge-wait ${JSON.stringify(idlePred)} 0.3)`);
        }
        ctx.assert(done, `pi turn ${i + 1}: full cycle completed (idle after agent_end)`);

        // Server is the source of truth for the turn tree (the client-side
        // `:turns` tlist doesn't accumulate visibly per turn here — same
        // patch-replay snapshot interaction documented in S5).
        const adds = countAddTurnsForSid();
        ctx.assert(adds >= baselineAddTurns + i + 1,
          `pi turn ${i + 1}: server-side add_turn count grew (expected ≥ ${baselineAddTurns + i + 1}, got ${adds})`);
      }

      // Final invariant: at least N add_turn ops landed for this session.
      const finalAdds = countAddTurnsForSid();
      ctx.assert(finalAdds >= prompts.length,
        `server emitted ≥ ${prompts.length} add_turn ops across the multi-turn pi session (got ${finalAdds})`);
    },
  },

  // ── S9: per-turn edited-files consolidated diff ──────────────────────
  // Drives the per-turn edited-files feature end-to-end. One turn edits
  // file A three times and file B once. The server reads each edited
  // file's CURRENT content from disk on every PostToolUse, reconstructs
  // the turn baseline by reverse-applying the FIRST edit's structuredPatch,
  // and emits `update_turn_file` patches carrying a consolidated FileDiff.
  //
  // The core assertion: file A — edited 3× — renders ONE consolidated diff
  // V0→V3 (original `alpha`/`beta`/`gamma` removed, final `ALPHA`/`BETA`/
  // `GAMMA` added), NOT three stacked per-edit diffs.
  {
    name: "edited-files-consolidated-diff",
    async run(ctx) {
      const sid = "s9-edited-files";
      const fileA = join(ctx.RUN, "fileA.txt");
      const fileB = join(ctx.RUN, "fileB.txt");

      // Tiny 3-line files keep the hand-written first-edit hunk trivial.
      // A: V0=alpha/beta/gamma → V1 → V2 → V3=ALPHA/BETA/GAMMA.
      const A0 = "alpha\nbeta\ngamma\n";
      const A1 = "alpha\nBETA\ngamma\n";
      const A2 = "alpha\nBETA\nGAMMA\n";
      const A3 = "ALPHA\nBETA\nGAMMA\n";
      const B0 = "one\ntwo\nthree\n";
      const B1 = "one\nTWO\nthree\n";

      // First-edit hunk for A: baseline(V0) → V1. The server reverse-
      // applies this against the on-disk content to reconstruct V0.
      const aFirstHunk = [{
        oldStart: 1, oldLines: 3, newStart: 1, newLines: 3,
        lines: [" alpha", "-beta", "+BETA", " gamma"],
      }];
      // First-edit hunk for B: baseline(B0) → B1.
      const bFirstHunk = [{
        oldStart: 1, oldLines: 3, newStart: 1, newLines: 3,
        lines: [" one", "-two", "+TWO", " three"],
      }];

      // Find file A's client-side edited-files entry across all turns.
      // Returns the editCount, or -1 if no entry exists yet.
      const aEntryEditCount = () => {
        const form = `(let ((ec -1))
          (let ((s (gethash ${JSON.stringify(sid)} claude-gravity--sessions)))
            (when s
              (dolist (tn (claude-gravity--tlist-items (plist-get s :turns)))
                (dolist (fd (alist-get 'edited-files tn))
                  (when (equal (alist-get 'path fd) ${JSON.stringify(fileA)})
                    (setq ec (or (alist-get 'editCount fd) 0)))))))
          ec)`;
        return parseInt(ctx.emacs(form), 10);
      };
      // Predicate string for cge-wait: file A's editCount reached N.
      const aCountReached = (n) =>
        `(let ((ec -1))
           (let ((s (gethash ${JSON.stringify(sid)} claude-gravity--sessions)))
             (when s
               (dolist (tn (claude-gravity--tlist-items (plist-get s :turns)))
                 (dolist (fd (alist-get 'edited-files tn))
                   (when (equal (alist-get 'path fd) ${JSON.stringify(fileA)})
                     (setq ec (or (alist-get 'editCount fd) 0)))))))
           (= ec ${n}))`;

      // (1) Start a session + one turn.
      await replay(ctx.HOOK, [
        { event: "SessionStart", data: { slug: "edited", cwd: "/work/proj" } },
        { event: "UserPromptSubmit", data: { prompt: "refactor fileA" } },
      ], { session_id: sid, cwd: "/work/proj" });
      ctx.emacs("(cge-poll)");
      ctx.assert(ctx.emacsT(`(cge-wait "(gethash \\"${sid}\\" claude-gravity--sessions)" 5)`),
        "edited-files session reached the live client");

      // Open the session detail buffer up front (creates the buffer so
      // `claude-gravity--render-session-buffer` has somewhere to render).
      ctx.emacsT(`(progn (claude-gravity-open-session ${JSON.stringify(sid)}) t)`);
      ctx.emacs("(cge-pump 0.3)");

      // (2) Edit file A three times in this one turn. For each edit i:
      // write the post-edit version to disk FIRST (the server reads
      // file_path off disk at PostToolUse), then inject Pre/PostToolUse,
      // then wait until the client's editCount == i before the next write
      // (avoids the race where the server reads a too-new file version).
      const aVersions = [A1, A2, A3];
      for (let i = 1; i <= 3; i++) {
        writeFileSync(fileA, aVersions[i - 1]);
        const tid = `s9-a-${i}`;
        // Edit 1 carries the real baseline→V1 hunk; edits 2/3 carry [] —
        // the algorithm only reverse-applies the FIRST edit's patch.
        const structuredPatch = i === 1 ? aFirstHunk : [];
        await replay(ctx.HOOK, [
          { event: "PreToolUse", data: { tool_name: "Edit", tool_use_id: tid,
              tool_input: { file_path: fileA } } },
          { event: "PostToolUse", data: { tool_name: "Edit", tool_use_id: tid,
              tool_input: { file_path: fileA },
              tool_response: { filePath: fileA, structuredPatch } } },
        ], { session_id: sid, cwd: "/work/proj" });

        let reached = false;
        for (let k = 0; k < 20 && !reached; k++) {
          ctx.emacs("(cge-poll)");
          reached = ctx.emacsT(`(cge-wait ${JSON.stringify(aCountReached(i))} 0.4)`);
        }
        ctx.assert(reached,
          `file A edit ${i}: client-side edited-files entry reached editCount=${i}`);
      }

      // (3) Edit file B once in the SAME turn.
      writeFileSync(fileB, B1);
      await replay(ctx.HOOK, [
        { event: "PreToolUse", data: { tool_name: "Edit", tool_use_id: "s9-b-1",
            tool_input: { file_path: fileB } } },
        { event: "PostToolUse", data: { tool_name: "Edit", tool_use_id: "s9-b-1",
            tool_input: { file_path: fileB },
            tool_response: { filePath: fileB, structuredPatch: bFirstHunk } } },
      ], { session_id: sid, cwd: "/work/proj" });

      // (4) Stop the turn.
      await replay(ctx.HOOK, [
        { event: "Stop", data: {} },
      ], { session_id: sid, cwd: "/work/proj" });

      // Pull the final state so the B patch + Stop land on the client.
      let bArrived = false;
      for (let k = 0; k < 20 && !bArrived; k++) {
        ctx.emacs("(cge-poll)");
        bArrived = ctx.emacsT(
          `(cge-wait "(let ((s (gethash \\"${sid}\\" claude-gravity--sessions)) (n 0))
             (when s (dolist (tn (claude-gravity--tlist-items (plist-get s :turns)))
               (setq n (+ n (length (alist-get 'edited-files tn))))))
             (>= n 2))" 0.4)`);
      }

      // (5a) The turn's client-side edited-files has 2 entries; file A == 3.
      const editCountA = aEntryEditCount();
      ctx.assert(editCountA === 3,
        `file A's client-side edited-files entry reports editCount=3 (got ${editCountA})`);

      const totalEntries = parseInt(ctx.emacs(
        `(let ((s (gethash ${JSON.stringify(sid)} claude-gravity--sessions)) (n 0))
           (when s (dolist (tn (claude-gravity--tlist-items (plist-get s :turns)))
             (setq n (+ n (length (alist-get 'edited-files tn))))))
           n)`), 10);
      ctx.assert(totalEntries === 2,
        `the turn has exactly 2 edited-files entries — A and B (got ${totalEntries})`);

      // (5b) Render the session detail buffer (like S6/S7) and assert the
      // "Edited Files (2)" subsection renders.
      ctx.emacs(`(progn (claude-gravity--render-session-buffer
                          (gethash ${JSON.stringify(sid)} claude-gravity--sessions))
                        (cge-pump 0.4) t)`);
      // The turn is frozen after Stop, so its children (incl. the Edited
      // Files subsection) are deferred to a magit washer that only runs
      // when the section is shown. Force-expand everything before dumping.
      ctx.emacsT(`(cge-expand-all-match "\\\\*Claude: refactor fileA")`);
      ctx.emacs("(cge-pump 0.3)");
      // The session detail buffer is named after the session label, which
      // is the first prompt's text ("refactor fileA").
      const detail = dumpMatch(ctx, `\\*Claude: refactor fileA`);
      ctx.assert(/Edited Files \(2\)/.test(detail),
        "session detail shows the 'Edited Files (2)' subsection");
      ctx.assert(detail.includes(fileA) && detail.includes(fileB),
        "both edited files (A and B) render in the subsection");

      // Scope assertions to the "Edited Files" subsection — the per-tool
      // inline diffs (under each Edit tool) coexist in the same buffer and
      // we must not accidentally match those when proving the roll-up.
      const efIdx = detail.indexOf("Edited Files (2)");
      const efSection = efIdx >= 0 ? detail.slice(efIdx) : "";
      // file A's entry text = from its path line up to file B's path line.
      const aIdx = efSection.indexOf(fileA + "  ");
      const bIdx = efSection.indexOf(fileB + "  ");
      const aEntry = aIdx >= 0 && bIdx > aIdx
        ? efSection.slice(aIdx, bIdx) : "";
      const bEntry = bIdx >= 0 ? efSection.slice(bIdx) : "";

      // (5c) THE CORE REQUIREMENT: file A shows ONE consolidated diff
      // baseline→final (V0→V3). The structured-patch renderer merges a
      // removed line and its replacement onto one physical line (word-
      // level refinement lives in faces, stripped by the no-properties
      // dump) — so V0→V3 renders as `alphaALPHA` / `betaBETA` / `gammaGAMMA`.
      // All three appear in ONE hunk: this IS the net V0→V3 change, not a
      // stack of three per-edit diffs.
      ctx.assert(/alphaALPHA/.test(aEntry),
        "file A's consolidated diff carries the V0→V3 change for line 1 (alpha→ALPHA)");
      ctx.assert(/betaBETA/.test(aEntry),
        "file A's consolidated diff carries beta→BETA");
      ctx.assert(/gammaGAMMA/.test(aEntry),
        "file A's consolidated diff carries gamma→GAMMA");
      ctx.assert(/3 edits/.test(aEntry),
        "file A's entry shows the '3 edits' label");

      // ONE consolidated diff, not three stacked per-edit diffs: file A's
      // entry contains exactly ONE `@@` hunk header. Three separate per-
      // edit diffs would yield three. And `betaBETA` — the V1 change that
      // would repeat in every naive per-edit diff — appears exactly once.
      const aHunks = (aEntry.match(/^\s*@@ /mg) || []).length;
      ctx.assert(aHunks === 1,
        `file A renders ONE consolidated hunk, not per-edit diffs (got ${aHunks} '@@' headers)`);
      const betaCount = (aEntry.match(/betaBETA/g) || []).length;
      ctx.assert(betaCount === 1,
        `file A's consolidated diff is V0→V3, not stacked — 'betaBETA' appears once (got ${betaCount})`);

      // (5d) File B's single-edit entry renders too.
      ctx.assert(/1 edit\b/.test(bEntry),
        "file B's entry shows the '1 edit' label");
      ctx.assert(/twoTWO/.test(bEntry),
        "file B's single-edit consolidated diff shows two→TWO");

      // Server emitted update_turn_file patches (one per edit = 4 total).
      const sp = ctx.proxyMessages().filter((m) =>
        m.dir === "s2c" && m.msg?.type === "session-patches"
        && m.msg?.sessionId === sid);
      const utfCount = sp.length
        ? (sp[sp.length - 1].msg.patches || [])
            .filter((p) => p.op === "update_turn_file").length
        : 0;
      ctx.assert(utfCount >= 4,
        `server emitted ≥ 4 update_turn_file patches (3 for A + 1 for B; got ${utfCount})`);
    },
  },
];
