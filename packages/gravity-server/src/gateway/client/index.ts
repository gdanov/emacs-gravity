// index.ts — Phase-B browser client.
//
// Connects to the gateway, exchanges the protocol-2 hello + initial
// resync, then renders the live state into static DOM nodes from
// `index.html`. Three panels:
//
//   1. Fleet table — `ProjectSummary[]` grouped by `repoKey` (grouping
//      happens server-side via `git rev-parse --git-common-dir`; the
//      client only iterates). One row per `SessionSummary` showing
//      role / branch / worktree / turnCount / currentTool / cost.
//   2. Inbox — every `InboxItem` rendered regardless of `actionable`,
//      with `classifyInboxItem` distinguishing actionable / view-only /
//      attention items via CSS classes. No buttons are ever wired —
//      `hello` stays `capabilities: []` for the whole ticket, this file
//      included.
//   3. Drill-down — clicking a session row sends `request.session`,
//      which subscribes AND delivers a `session.snapshot`. Subsequent
//      state for that session arrives via `state-changed` → synchronous
//      `poll` → `session-patches{sessionId, seq, patches}` (we apply
//      the patches to a local copy via `applyPatch` from logic.ts). A
//      "back" button sends `request.unsubscribe` and pops the panel.
//
// Reconnects on close / `visibilitychange`, always sends the hello +
// resync on a fresh socket, and re-subscribes to a previously drilled
// session after reconnect (the server's subscription set lives on the
// conn, which is gone after reconnect).

import {
  applyPatch,
  classifyInboxItem,
  firstLine,
  formatDuration,
  groupedFleet,
  resultText,
  shouldSyncPoll,
  toolArgSummary,
} from "./logic.js";
import type {
  Agent,
  InboxItem,
  ProjectSummary,
  ServerMessage,
  Session,
  SessionSummary,
  StepNode,
  Tool,
  TurnNode,
} from "@gravity/shared";

const RECONNECT_DELAY_MS = 1000;
const TOKEN = (globalThis as { __GRAVITY_TOKEN__?: string }).__GRAVITY_TOKEN__ ?? "";

interface ClientState {
  projects: ProjectSummary[];
  inbox: InboxItem[];
  status: string;
  notice: string | null;
  /** The session we are currently drilled into, if any. The id is
   * remembered across reconnects so the drill-down can be re-restored
   * via a fresh `request.session` after a dropped socket. */
  currentSessionId: string | null;
  /** Local mutable copy of the drilled session, advanced by snapshots
   * and patches. Null while waiting for the initial `session.snapshot`
   * after a (re-)subscribe. */
  currentSession: Session | null;
}

const state: ClientState = {
  projects: [],
  inbox: [],
  status: "connecting…",
  notice: null,
  currentSessionId: null,
  currentSession: null,
};

let ws: WebSocket | null = null;
let reconnectTimer: ReturnType<typeof setTimeout> | null = null;
let closedByUs: boolean = false;

function el<T extends HTMLElement>(id: string): T | null {
  return document.getElementById(id) as T | null;
}

function textNode(text: string): Text {
  return document.createTextNode(text);
}

function elFromHtml(html: string): HTMLElement {
  const tpl = document.createElement("template");
  tpl.innerHTML = html.trim();
  return tpl.content.firstElementChild as HTMLElement;
}

function renderFleet(parent: HTMLElement): void {
  const projects = groupedFleet(state.projects);
  // Drop any stale click handlers by recreating the container each
  // render. The click wiring reads `data-session-id` off the row, so it
  // naturally handles re-renders; recreating the wrapper is the
  // simplest way to drop any per-row listeners without keeping a
  // bookkeeping structure.
  const container = elFromHtml(`<div class="fleet"></div>`);

  if (projects.length === 0) {
    container.appendChild(textNode("(no projects)"));
  } else {
    for (const project of projects) {
      const groupKey = project.repoKey || project.project;
      const group = elFromHtml(
        `<section class="fleet-group"><h3 class="fleet-group__key"></h3></section>`,
      );
      const keyEl = group.querySelector(".fleet-group__key");
      if (keyEl) keyEl.textContent = groupKey;

      const summaryText = ` — ${project.sessions.length} session(s)`;
      const meta = elFromHtml(`<span class="fleet-group__meta"></span>`);
      meta.textContent = summaryText;
      group.appendChild(meta);

      if (project.sessions.length === 0) {
        const empty = document.createElement("p");
        empty.className = "fleet-empty";
        empty.textContent = "(no sessions)";
        group.appendChild(empty);
      } else {
        const table = elFromHtml(
          `<table class="fleet-table">
            <thead>
              <tr>
                <th>session</th>
                <th>role</th>
                <th>branch</th>
                <th>worktree</th>
                <th>turns</th>
                <th>current tool</th>
                <th>cost</th>
              </tr>
            </thead>
            <tbody></tbody>
          </table>`,
        );
        const tbody = table.querySelector("tbody");
        if (!tbody) {
          group.appendChild(table);
        } else {
          for (const session of project.sessions) {
            tbody.appendChild(renderFleetRow(project.project, session));
          }
          group.appendChild(table);
        }
      }
      container.appendChild(group);
    }
  }

  replaceChildById(parent, "fleet-root", container);
}

function renderFleetRow(project: string, session: SessionSummary): HTMLTableRowElement {
  const tr = document.createElement("tr");
  tr.className = "fleet-row";
  tr.dataset.sessionId = session.sessionId;
  tr.dataset.project = project;
  tr.setAttribute("tabindex", "0");
  tr.setAttribute("role", "button");
  tr.setAttribute("aria-label", `Drill into session ${session.sessionId}`);

  const idCell = elFromHtml(`<td class="fleet-row__id"></td>`);
  idCell.textContent = session.displayName ?? session.slug ?? session.sessionId;
  if (!session.displayName && !session.slug) {
    idCell.title = session.sessionId;
  }
  tr.appendChild(idCell);

  tr.appendChild(td(session.role ?? "—"));
  tr.appendChild(td(session.branch ?? "—"));
  tr.appendChild(td(session.worktree ?? "—"));
  tr.appendChild(td(String(session.turnCount)));
  tr.appendChild(td(session.currentTool ?? "—"));
  tr.appendChild(td(formatCost(session.cost)));

  tr.addEventListener("click", () => onSessionRowClick(session.sessionId));
  tr.addEventListener("keydown", (ev) => {
    if (ev.key === "Enter" || ev.key === " ") {
      ev.preventDefault();
      onSessionRowClick(session.sessionId);
    }
  });
  return tr;
}

function td(text: string): HTMLTableCellElement {
  const cell = document.createElement("td");
  cell.textContent = text;
  return cell;
}

function formatCost(cost: number | null): string {
  if (cost == null) return "—";
  if (cost < 0.01) return `$${cost.toFixed(4)}`;
  return `$${cost.toFixed(2)}`;
}

function onSessionRowClick(sessionId: string): void {
  if (state.currentSessionId === sessionId) return;
  if (state.currentSessionId && state.currentSessionId !== sessionId) {
    // Drop the previous subscription before re-subscribing to a
    // different one — the server-side per-conn subscription set is
    // single-shot, so we have to release the old one explicitly.
    send({ type: "request.unsubscribe", sessionId: state.currentSessionId });
  }
  state.currentSessionId = sessionId;
  state.currentSession = null;
  openState.clear();
  render();
  send({ type: "request.session", sessionId });
}

function closeDrillDown(): void {
  if (!state.currentSessionId) return;
  send({ type: "request.unsubscribe", sessionId: state.currentSessionId });
  state.currentSessionId = null;
  state.currentSession = null;
  openState.clear();
  render();
}

function renderInbox(parent: HTMLElement): void {
  const container = elFromHtml(`<ul class="inbox" id="inbox-list"></ul>`);
  if (state.inbox.length === 0) {
    const li = document.createElement("li");
    li.className = "inbox-empty";
    li.textContent = "(empty)";
    container.appendChild(li);
  } else {
    for (const item of state.inbox) {
      container.appendChild(renderInboxItem(item));
    }
  }
  replaceChildById(parent, "inbox-root", container);
}

function renderInboxItem(item: InboxItem): HTMLLIElement {
  const cls = classifyInboxItem(item);
  const li = document.createElement("li");
  li.className = `inbox-item inbox-item--${cls}`;
  const badge = document.createElement("span");
  badge.className = `inbox-item__badge inbox-item__badge--${cls}`;
  badge.textContent = cls === "attention" ? "!" : cls === "actionable" ? "→" : "view";
  li.appendChild(badge);
  const type = document.createElement("span");
  type.className = "inbox-item__type";
  type.textContent = item.type;
  li.appendChild(type);
  const label = document.createElement("span");
  label.className = "inbox-item__label";
  label.textContent = item.label;
  li.appendChild(label);
  if (item.type === "attention") {
    li.title = item.summary || item.label;
  }
  return li;
}

function renderDrillDown(parent: HTMLElement): void {
  // Always (re)render so that updates from `session-patches` flow
  // through. When no session is selected, render an empty stub that
  // collapses the panel.
  const container = elFromHtml(`<section class="drill" id="drill-root"></section>`);
  if (!state.currentSessionId) {
    const empty = document.createElement("p");
    empty.className = "drill-empty";
    empty.textContent = "(click a session row to drill in)";
    container.appendChild(empty);
    replaceChildById(parent, "drill-root", container);
    return;
  }

  const header = elFromHtml(
    `<header class="drill__header">
       <button type="button" class="drill__back" aria-label="Back to fleet">← fleet</button>
       <h3 class="drill__title"></h3>
     </header>`,
  );
  header.querySelector(".drill__back")?.addEventListener("click", closeDrillDown);
  const titleEl = header.querySelector(".drill__title");
  if (titleEl) {
    titleEl.textContent = state.currentSession
      ? `session ${state.currentSession.sessionId}`
      : `session ${state.currentSessionId} — loading…`;
  }
  container.appendChild(header);

  if (!state.currentSession) {
    const loading = document.createElement("p");
    loading.className = "drill-loading";
    loading.textContent = "loading…";
    container.appendChild(loading);
  } else {
    container.appendChild(renderSessionTree(state.currentSession));
  }
  replaceChildById(parent, "drill-root", container);
}

// ── Transcript rendering ─────────────────────────────────────────────
//
// The drill-down renders the session as a chat-style transcript: one
// collapsible `details.event` card per prompt / assistant message /
// tool call, subagents as indented `details.subagent` branches. The
// visual language (kind tinting, badges, monospace summaries) mirrors
// the render-transcript HTML reports so both read the same.
//
// Because the view re-renders on every patch, each card carries a
// stable `data-key`; `openState` remembers the user's manual toggles
// and re-applies them after a rebuild. Defaults: user prompts open,
// everything else collapsed.

const openState = new Map<string, boolean>();

function isOpen(key: string, dflt: boolean): boolean {
  return openState.get(key) ?? dflt;
}

interface EventCardOpts {
  key: string;
  className: string;
  defaultOpen?: boolean;
  summary: (HTMLElement | Text)[];
  body: HTMLElement[];
}

function eventCard(opts: EventCardOpts): HTMLDetailsElement {
  const details = document.createElement("details");
  details.className = opts.className;
  details.dataset.key = opts.key;
  details.open = isOpen(opts.key, opts.defaultOpen ?? false);
  const summary = document.createElement("summary");
  for (const node of opts.summary) summary.appendChild(node);
  details.appendChild(summary);
  if (opts.body.length > 0) {
    const body = document.createElement("div");
    body.className = "body";
    for (const node of opts.body) body.appendChild(node);
    details.appendChild(body);
  }
  return details;
}

function badge(text: string, extraClass = ""): HTMLSpanElement {
  const span = document.createElement("span");
  span.className = extraClass ? `badge ${extraClass}` : "badge";
  span.textContent = text;
  return span;
}

function span(className: string, text: string): HTMLSpanElement {
  const s = document.createElement("span");
  s.className = className;
  s.textContent = text;
  return s;
}

function pre(className: string, text: string): HTMLPreElement {
  const p = document.createElement("pre");
  p.className = className;
  p.textContent = text;
  return p;
}

function thinkingBlock(key: string, text: string): HTMLDetailsElement {
  const details = document.createElement("details");
  details.className = "thinking-block";
  details.dataset.key = key;
  details.open = isOpen(key, false);
  const summary = document.createElement("summary");
  summary.textContent = "thinking";
  details.appendChild(summary);
  details.appendChild(pre("text-block", text));
  return details;
}

function renderSessionTree(session: Session): HTMLElement {
  const root = elFromHtml(`<div class="transcript"></div>`);

  const meta = elFromHtml(`<div class="transcript-meta"></div>`);
  meta.appendChild(badge(session.source ?? "claude-code", "harness"));
  if (session.role) meta.appendChild(badge(session.role, "role"));
  meta.appendChild(badge(
    session.status === "ended" ? "ended" : session.claudeStatus,
    session.status === "ended" ? "status-ended"
      : session.claudeStatus === "responding" ? "status-responding" : "",
  ));
  const metaLines: string[] = [];
  metaLines.push(`session id: ${session.sessionId}`);
  if (session.modelName) metaLines.push(`model: ${session.modelName}`);
  if (session.branch) metaLines.push(`branch: ${session.branch}`);
  metaLines.push(`cwd: ${session.cwd}`);
  metaLines.push(`started: ${new Date(session.startTime).toISOString()}`);
  if (session.tokenUsage) {
    metaLines.push(`tokens: in=${session.tokenUsage.input_tokens ?? 0} out=${session.tokenUsage.output_tokens ?? 0}`);
  }
  if (session.cost != null) metaLines.push(`cost: ${formatCost(session.cost)}`);
  for (const line of metaLines) {
    meta.appendChild(span("", line));
    meta.appendChild(document.createElement("br"));
  }
  root.appendChild(meta);

  const controls = elFromHtml(
    `<div class="controls">
       <button type="button" class="c-expand">expand all</button>
       <button type="button" class="c-collapse">collapse all</button>
       <button type="button" class="c-prompts">back to prompts only</button>
     </div>`,
  );
  root.appendChild(controls);

  const events = elFromHtml(`<section class="events"></section>`);
  for (const turn of session.turns) {
    renderTurnEvents(events, session, turn);
  }
  root.appendChild(events);

  // Record every manual toggle so the state survives re-renders.
  // `toggle` doesn't bubble, so listen in the capture phase.
  events.addEventListener(
    "toggle",
    (ev) => {
      const d = ev.target as HTMLDetailsElement;
      if (d.dataset.key) openState.set(d.dataset.key, d.open);
    },
    true,
  );

  const allCards = (): HTMLDetailsElement[] =>
    Array.from(events.querySelectorAll<HTMLDetailsElement>("details[data-key]"));
  controls.querySelector(".c-expand")?.addEventListener("click", () => {
    for (const d of allCards()) d.open = true;
  });
  controls.querySelector(".c-collapse")?.addEventListener("click", () => {
    for (const d of allCards()) d.open = false;
  });
  controls.querySelector(".c-prompts")?.addEventListener("click", () => {
    for (const d of allCards()) d.open = d.classList.contains("prompt-event");
  });

  return root;
}

function renderTurnEvents(events: HTMLElement, session: Session, turn: TurnNode): void {
  const sep = elFromHtml(`<div class="turn-sep"></div>`);
  const parts = [`turn ${turn.turnNumber}`];
  if (turn.toolCount > 0) parts.push(`${turn.toolCount} tools`);
  if (turn.agentCount > 0) parts.push(`${turn.agentCount} agents`);
  if (turn.stopReason && turn.stopReason !== "stop") parts.push(`stop: ${turn.stopReason}`);
  sep.textContent = parts.join(" · ");
  events.appendChild(sep);

  for (const marker of session.compactions) {
    if (marker.turnNumber !== turn.turnNumber) continue;
    events.appendChild(eventCard({
      key: `t${turn.turnNumber}-compact-${marker.timestamp}`,
      className: "event kind-control",
      summary: [badge("control"), textNode(` compaction (${marker.reason})`)],
      body: marker.summary ? [pre("text-block", marker.summary)] : [],
    }));
  }

  if (turn.prompt) {
    const roleLabel = turn.prompt.type === "user" ? "user" : turn.prompt.type;
    const body: HTMLElement[] = [pre("text-block", turn.prompt.text)];
    if (turn.prompt.answer) {
      body.push(pre("text-block", `answer: ${turn.prompt.answer}`));
    }
    events.appendChild(eventCard({
      key: `t${turn.turnNumber}-prompt`,
      className: "event kind-message prompt-event",
      defaultOpen: true,
      summary: [badge(roleLabel, "role"), textNode(` ${firstLine(turn.prompt.text)}`)],
      body,
    }));
  }

  for (let si = 0; si < turn.steps.length; si++) {
    renderStepEvents(events, turn.steps[si], `t${turn.turnNumber}-s${si}`);
  }

  for (const agent of turn.agents) {
    events.appendChild(renderAgentBranch(agent));
  }

  if (turn.stopText || turn.stopThinking) {
    const body: HTMLElement[] = [];
    if (turn.stopThinking) body.push(thinkingBlock(`t${turn.turnNumber}-stop-think`, turn.stopThinking));
    if (turn.stopText) body.push(pre("text-block", turn.stopText));
    events.appendChild(eventCard({
      key: `t${turn.turnNumber}-stop`,
      className: "event kind-message",
      summary: [badge("assistant", "role"), textNode(` ${firstLine(turn.stopText ?? "(thinking)")}`)],
      body,
    }));
  }
}

function renderStepEvents(parent: HTMLElement, step: StepNode, keyPrefix: string): void {
  if (step.text || step.thinking) {
    const body: HTMLElement[] = [];
    if (step.thinking) body.push(thinkingBlock(`${keyPrefix}-think`, step.thinking));
    if (step.text) body.push(pre("text-block", step.text));
    parent.appendChild(eventCard({
      key: `${keyPrefix}-msg`,
      className: "event kind-message",
      summary: [badge("assistant", "role"), textNode(` ${firstLine(step.text ?? "(thinking)")}`)],
      body,
    }));
  }
  for (const tool of step.tools) {
    parent.appendChild(renderToolCard(tool, keyPrefix));
  }
}

function renderToolCard(tool: Tool, keyPrefix: string): HTMLDetailsElement {
  const key = `${keyPrefix}-tool-${tool.toolUseId || tool.name}`;
  const statusClass = tool.status === "running" ? " tool--running"
    : tool.status === "error" ? " tool--error" : "";

  const summary: (HTMLElement | Text)[] = [
    badge(tool.status === "running" ? "running" : "tool"),
    textNode(" "),
    span("tool-name", tool.name),
    textNode(" "),
  ];
  const args = toolArgSummary(tool);
  if (args) {
    summary.push(span("tool-args", args));
    summary.push(textNode(" "));
  }
  const preview = tool.status === "running"
    ? resultText(tool.partial)
    : resultText(tool.result);
  if (preview) {
    summary.push(span("result-arrow", "→"));
    summary.push(textNode(` ${firstLine(preview, 80)}`));
  }
  if (tool.duration != null) {
    summary.push(textNode(" "));
    summary.push(span("dur", formatDuration(tool.duration)));
  }

  const body: HTMLElement[] = [];
  if (Object.keys(tool.input ?? {}).length > 0) {
    body.push(pre("json tool-call-input", JSON.stringify(tool.input, null, 2)));
  }
  if (preview) body.push(pre("tool-result", preview));

  return eventCard({
    key,
    className: `event kind-tool-result grouped${statusClass}`,
    summary,
    body,
  });
}

function renderAgentBranch(agent: Agent): HTMLDetailsElement {
  const key = `agent-${agent.agentId}`;
  const details = document.createElement("details");
  details.className = "subagent";
  details.dataset.key = key;
  details.open = isOpen(key, agent.status === "running");
  const summary = document.createElement("summary");
  summary.appendChild(badge(agent.status === "running" ? "running" : "agent"));
  summary.appendChild(textNode(` ${agent.type}`));
  if (agent.duration != null) {
    summary.appendChild(textNode(" "));
    summary.appendChild(span("dur", formatDuration(agent.duration)));
  }
  details.appendChild(summary);

  const body = document.createElement("div");
  body.className = "subagent-body";
  for (let si = 0; si < agent.steps.length; si++) {
    renderStepEvents(body, agent.steps[si], `${key}-s${si}`);
  }
  if (agent.stopText || agent.stopThinking) {
    const stopBody: HTMLElement[] = [];
    if (agent.stopThinking) stopBody.push(thinkingBlock(`${key}-stop-think`, agent.stopThinking));
    if (agent.stopText) stopBody.push(pre("text-block", agent.stopText));
    body.appendChild(eventCard({
      key: `${key}-stop`,
      className: "event kind-message",
      summary: [badge("assistant", "role"), textNode(` ${firstLine(agent.stopText ?? "(thinking)")}`)],
      body: stopBody,
    }));
  }
  details.appendChild(body);
  return details;
}

function replaceChildById(parent: HTMLElement, id: string, next: HTMLElement): void {
  next.id = id;
  const existing = parent.querySelector(`:scope > #${CSS.escape(id)}`);
  if (existing && existing !== next) {
    parent.replaceChild(next, existing);
    return;
  }
  parent.appendChild(next);
}

function render(): void {
  const status = el<HTMLElement>("status");
  if (status) {
    const txt = state.notice ? `${state.status} — ${state.notice}` : state.status;
    if (status.textContent !== txt) status.textContent = txt;
  }
  renderFleet(document.body);
  renderInbox(document.body);
  renderDrillDown(document.body);
}

function isServerMessage(value: unknown): value is ServerMessage {
  if (typeof value !== "object" || value === null) return false;
  const t = (value as { type?: unknown }).type;
  return typeof t === "string";
}

function applySnapshot(msg: ServerMessage): void {
  switch (msg.type) {
    case "overview.snapshot":
    case "overview-data":
      state.projects = msg.projects;
      break;
    case "inbox.snapshot":
    case "inbox-items":
      state.inbox = msg.items;
      break;
    case "session.snapshot":
      if (state.currentSessionId === msg.sessionId) {
        state.currentSession = msg.session;
      }
      break;
    case "session-patches": {
      if (state.currentSessionId !== msg.sessionId) break;
      if (!state.currentSession) break;
      let working: Session | null = state.currentSession;
      for (const patch of msg.patches) {
        if (!working) break;
        const next = applyPatch(working, patch);
        if (next === working) {
          // Unknown op (or no-op): drop the local copy and re-request
          // a full snapshot rather than rendering a stale view.
          send({ type: "request.session", sessionId: msg.sessionId });
          state.currentSession = null;
          working = null;
          break;
        }
        working = next;
      }
      if (working) state.currentSession = working;
      break;
    }
    case "session.removed":
      if (state.currentSessionId === msg.sessionId) {
        state.currentSessionId = null;
        state.currentSession = null;
      }
      break;
    case "notice":
      state.notice = msg.text;
      break;
    case "protocol.mismatch":
      state.notice = `protocol mismatch — ${msg.text}`;
      break;
    case "state-changed":
      // Signal-only — `shouldSyncPoll` decides whether to follow up
      // with a synchronous `poll` (see `onmessage` below). No state
      // mutation here.
      break;
    case "pi.session":
      // The browser is read-only — nothing to render for pi lifecycle.
      break;
  }
}

function connect(): void {
  closedByUs = false;
  state.status = "connecting…";
  state.notice = null;
  render();

  const url = new URL("/ws", location.href);
  url.protocol = url.protocol === "https:" ? "wss:" : "ws:";
  url.searchParams.set("token", TOKEN);

  ws = new WebSocket(url.toString());

  ws.addEventListener("open", () => {
    state.status = "connected";
    render();
    send({ type: "hello", capabilities: [], protocolVersion: 2 });
    send({ type: "request.resync" });
    if (state.currentSessionId) {
      // The server's per-conn subscription set was wiped on disconnect;
      // re-establish it so the next session.snapshot re-populates
      // state.currentSession.
      send({ type: "request.session", sessionId: state.currentSessionId });
    }
  });

  ws.addEventListener("message", (ev: MessageEvent) => {
    let parsed: unknown;
    try {
      parsed = JSON.parse(typeof ev.data === "string" ? ev.data : "");
    } catch {
      return;
    }
    if (!isServerMessage(parsed)) return;
    applySnapshot(parsed);
    render();
    if (shouldSyncPoll(parsed)) {
      // Must be synchronous with the message dispatch — background tabs
      // throttle setTimeout / queueMicrotask, and pulling the next
      // snapshot must not depend on the event loop being free.
      send({ type: "poll" });
    }
  });

  ws.addEventListener("close", () => {
    ws = null;
    // Drop the in-memory session copy so we re-render "(loading…)"
    // until a fresh `session.snapshot` arrives after reconnect. The
    // session id stays — that drives the resubscribe above.
    state.currentSession = null;
    state.status = "disconnected";
    render();
    if (!closedByUs) scheduleReconnect();
  });

  ws.addEventListener("error", () => {
    // The close handler runs after error and owns the reconnect path.
  });
}

function send(payload: unknown): void {
  if (!ws || ws.readyState !== WebSocket.OPEN) return;
  try {
    ws.send(JSON.stringify(payload));
  } catch {
    // socket will close on its own
  }
}

function scheduleReconnect(): void {
  state.status = "reconnecting…";
  render();
  if (reconnectTimer) clearTimeout(reconnectTimer);
  reconnectTimer = setTimeout(() => {
    reconnectTimer = null;
    connect();
  }, RECONNECT_DELAY_MS);
}

document.addEventListener("visibilitychange", () => {
  if (document.visibilityState !== "visible") return;
  if (!ws || ws.readyState === WebSocket.CLOSED) {
    if (reconnectTimer) clearTimeout(reconnectTimer);
    reconnectTimer = null;
    connect();
  }
});

window.addEventListener("beforeunload", () => {
  closedByUs = true;
  if (ws) {
    try { ws.close(); } catch { /* ignore */ }
    ws = null;
  }
});

connect();
