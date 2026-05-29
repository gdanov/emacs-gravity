// session.ts — Session factory and mutation methods
//
// Each mutation modifies Session in place AND returns the Patch[] it generated.
// This allows callers to batch patches and send them to terminals.

import type {
  Session,
  TurnNode,
  StepNode,
  Tool,
  Agent,
  Patch,
  TokenUsage,
  Plan,
  PromptEntry,
  Task,
  FileEntry,
  ToolLocation,
  AgentLocation,
  CompactionMarker,
  PiCommandDescriptor,
  PiModel,
} from "@gravity/shared";

// ── Helpers ──────────────────────────────────────────────────────────

/** Fields on tool results that are large and unused by terminal clients. */
const BLOATED_RESULT_FIELDS = ["structured_patch"];

/** Strip large unused fields from tool results to reduce patch size. */
function stripBloatedFields(result: unknown): unknown {
  if (result == null || typeof result !== "object" || Array.isArray(result)) return result;
  const rec = result as Record<string, unknown>;
  const hasBloat = BLOATED_RESULT_FIELDS.some((f) => f in rec);
  if (!hasBloat) return result;
  const cleaned: Record<string, unknown> = {};
  for (const [k, v] of Object.entries(rec)) {
    if (!BLOATED_RESULT_FIELDS.includes(k)) cleaned[k] = v;
  }
  return cleaned;
}

// ── Factory ──────────────────────────────────────────────────────────

/** Create a new empty session. */
export function createSession(sessionId: string, cwd: string, source?: string): Session {
  // Strip trailing slash before extracting basename. read-directory-name on the
  // Emacs side returns paths with a trailing "/", which makes split("/").pop()
  // return "" and then fall back to the full cwd as the project label.
  const trimmed = cwd.replace(/\/+$/, "");
  const project = trimmed.split("/").pop() || trimmed || cwd;
  return {
    sessionId,
    cwd,
    project,
    status: "active",
    claudeStatus: "idle",
    slug: null,
    displayName: null,
    branch: null,
    pid: null,
    modelName: null,
    tmuxSession: null,
    source: source ?? null,
    startTime: Date.now(),
    lastEventTime: Date.now(),
    tokenUsage: null,
    cost: null,
    contextUsage: null,
    piSessionFile: null,
    plan: null,
    streamingText: null,
    permissionMode: null,
    turns: [createTurnNode(0)], // turn 0 = pre-prompt activity
    currentTurn: 0,
    toolIndex: {},
    agentIndex: {},
    tasks: {},
    files: {},
    compactions: [],
    totalToolCount: 0,
    piCommands: null,
    piModels: null,
  };
}

export function createTurnNode(turnNumber: number): TurnNode {
  return {
    turnNumber,
    prompt: null,
    steps: [],
    agents: [],
    tasks: [],
    toolCount: 0,
    agentCount: 0,
    frozen: false,
    stopText: null,
    stopThinking: null,
    stopReason: null,
    tokenIn: null,
    tokenOut: null,
    editedFiles: [],
  };
}

export function createStepNode(thinking?: string | null, text?: string | null): StepNode {
  return {
    thinking: thinking ?? null,
    text: text ?? null,
    tools: [],
  };
}

// ── Mutations (return Patch[] for each state change) ─────────────────

export function sessionEnd(s: Session): Patch[] {
  s.status = "ended";
  s.claudeStatus = "idle";
  return [{ op: "set_status", status: "ended" }, { op: "set_claude_status", claudeStatus: "idle" }];
}

export function resetSession(s: Session): Patch[] {
  s.status = "active";
  s.claudeStatus = "idle";
  s.turns = [createTurnNode(0)];
  s.currentTurn = 0;
  s.toolIndex = {};
  s.agentIndex = {};
  s.tasks = {};
  s.files = {};
  s.compactions = [];
  s.totalToolCount = 0;
  s.plan = null;
  s.streamingText = null;
  s.tokenUsage = null;
  s.cost = null;
  s.contextUsage = null;
  s.lastEventTime = Date.now();
  // Emit patches so terminals clear stale data
  return [
    { op: "set_status", status: "active" },
    { op: "set_claude_status", claudeStatus: "idle" },
    { op: "set_plan", plan: null },
    { op: "set_streaming_text", text: null },
    { op: "set_token_usage", usage: { input_tokens: 0, output_tokens: 0, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 } },
    { op: "set_cost", cost: null },
    { op: "set_context_usage", contextUsage: null },
  ];
}

export function setClaudeStatus(s: Session, status: "idle" | "responding"): Patch[] {
  if (s.claudeStatus === status) return [];
  s.claudeStatus = status;
  return [{ op: "set_claude_status", claudeStatus: status }];
}

export function setPermissionMode(s: Session, mode: string | null): Patch[] {
  s.permissionMode = mode;
  return [{ op: "set_permission_mode", mode }];
}

export function setTokenUsage(s: Session, usage: TokenUsage): Patch[] {
  s.tokenUsage = usage;
  return [{ op: "set_token_usage", usage }];
}

export function setCost(s: Session, cost: number | null): Patch[] {
  s.cost = cost;
  return [{ op: "set_cost", cost }];
}

export function setContextUsage(
  s: Session,
  contextUsage: Session["contextUsage"],
): Patch[] {
  s.contextUsage = contextUsage;
  return [{ op: "set_context_usage", contextUsage }];
}

export function setPlan(s: Session, plan: Plan | null): Patch[] {
  s.plan = plan;
  return [{ op: "set_plan", plan }];
}

/**
 * Replace pi's command inventory snapshot. Project-scoped — survives turn
 * resets but is cleared on session purge. Callers should fetch via
 * `driver.getCommands()` and feed the result here.
 */
export function setPiCommands(
  s: Session,
  commands: PiCommandDescriptor[],
): Patch[] {
  s.piCommands = commands;
  return [{ op: "set_pi_commands", commands }];
}

/**
 * Replace pi's available-model snapshot. Pi-process-scoped — survives turn
 * resets but is cleared on session purge. Callers should fetch via
 * `driver.getAvailableModels()` and feed the result here.
 */
export function setPiModels(s: Session, models: PiModel[]): Patch[] {
  s.piModels = models;
  return [{ op: "set_pi_models", models }];
}

export function updateMeta(
  s: Session,
  opts: { pid?: number; slug?: string; displayName?: string; branch?: string; modelName?: string; tmuxSession?: string; piSessionFile?: string },
): Patch[] {
  s.lastEventTime = Date.now();
  if (opts.pid && opts.pid > 0) s.pid = opts.pid;
  if (opts.slug && !s.slug) s.slug = opts.slug;
  if (opts.displayName) s.displayName = opts.displayName;
  if (opts.branch) s.branch = opts.branch;
  if (opts.modelName) s.modelName = opts.modelName;
  if (opts.tmuxSession && !s.tmuxSession) s.tmuxSession = opts.tmuxSession;
  if (opts.piSessionFile) s.piSessionFile = opts.piSessionFile;
  return [{ op: "set_meta", ...opts }];
}

// ── Turn management ──────────────────────────────────────────────────

function currentTurnNode(s: Session): TurnNode {
  return s.turns[s.turns.length - 1];
}

function getTurnNode(s: Session, turnNumber: number): TurnNode | undefined {
  return s.turns.find((t) => t.turnNumber === turnNumber);
}

/**
 * Open a new empty turn.
 *
 * Defensively freezes the previous turn if it is still open — this guards
 * against the restart hazard where a pi subprocess dies mid-turn and the
 * next spawn would otherwise stack a new turn on top of an unfrozen one.
 *
 * Emits `freeze_turn` (prev, if applicable) + `add_turn` (new).
 */
export function openTurn(s: Session): Patch[] {
  const patches: Patch[] = [];

  const prev = currentTurnNode(s);
  if (prev && !prev.frozen) {
    prev.frozen = true;
    patches.push({ op: "freeze_turn", turnNumber: prev.turnNumber });
  }

  s.currentTurn++;
  const turn = createTurnNode(s.currentTurn);
  s.turns.push(turn);
  patches.push({ op: "add_turn", turn });

  return patches;
}

/**
 * Attach a prompt to the current (last) turn. Does NOT create a new turn —
 * the caller must have already called `openTurn` if needed. Idempotent: if
 * the current turn already has a prompt, this is a no-op (the boundary
 * survives, just no relabel).
 */
export function attachPrompt(s: Session, entry: PromptEntry): Patch[] {
  const turn = currentTurnNode(s);
  if (!turn) return [];
  if (turn.prompt) return [];
  turn.prompt = entry;
  return [{ op: "add_prompt", turnNumber: turn.turnNumber, prompt: entry }];
}

/**
 * Close the current turn: stamp stop text/thinking, mark frozen, record
 * elapsed time on the prompt. Optionally records token usage on the turn.
 *
 * Emits `set_turn_stop` (if any stop text/thinking) + `set_turn_tokens`
 * (if usage given) + `freeze_turn`. Idempotent under repeated calls — a
 * second `closeTurn` on the same already-frozen turn is a no-op except for
 * updating fields that are still null.
 */
export function closeTurn(
  s: Session,
  opts: {
    stopText?: string;
    stopThinking?: string;
    stopReason?: string;
    tokenIn?: number;
    tokenOut?: number;
  } = {},
): Patch[] {
  const turn = currentTurnNode(s);
  if (!turn) return [];

  const patches: Patch[] = [];

  if (turn.prompt && !turn.prompt.elapsed && turn.prompt.submitted) {
    turn.prompt.elapsed = (Date.now() - turn.prompt.submitted) / 1000;
  }

  if (opts.stopText && !turn.stopText) turn.stopText = opts.stopText;
  if (opts.stopThinking && !turn.stopThinking) turn.stopThinking = opts.stopThinking;
  if (opts.stopReason && !turn.stopReason) turn.stopReason = opts.stopReason;

  if (opts.stopText || opts.stopThinking || opts.stopReason) {
    patches.push({
      op: "set_turn_stop",
      turnNumber: turn.turnNumber,
      stopText: opts.stopText,
      stopThinking: opts.stopThinking,
      stopReason: opts.stopReason,
    });
  }

  if (opts.tokenIn != null || opts.tokenOut != null) {
    const tokenIn = opts.tokenIn ?? 0;
    const tokenOut = opts.tokenOut ?? 0;
    turn.tokenIn = tokenIn;
    turn.tokenOut = tokenOut;
    patches.push({ op: "set_turn_tokens", turnNumber: turn.turnNumber, tokenIn, tokenOut });
  }

  if (!turn.frozen) {
    turn.frozen = true;
    patches.push({ op: "freeze_turn", turnNumber: turn.turnNumber });
  }

  return patches;
}

/** Open a new turn and attach a prompt to it. Used by sources where the
 * boundary and the prompt arrive atomically (Claude Code's
 * UserPromptSubmit). */
export function addPrompt(s: Session, entry: PromptEntry): Patch[] {
  return [...openTurn(s), ...attachPrompt(s, entry)];
}

/**
 * Finalize the last prompt: set stop text/thinking AND freeze the turn.
 *
 * Historically this only set stop text and left the turn unfrozen — the
 * next `addPrompt` did the freeze lazily. That left turns open if the
 * source stopped and never sent another prompt (the pi case, and the
 * latent bug for Claude Code masked by tight prompt cadence). Now backed
 * by `closeTurn` so the turn is always frozen on stop.
 */
export function finalizeLastPrompt(
  s: Session,
  stopText?: string,
  stopThinking?: string,
): Patch[] {
  return closeTurn(s, { stopText, stopThinking });
}

export function setTurnTokens(s: Session, tokenIn: number, tokenOut: number): Patch[] {
  const turn = currentTurnNode(s);
  if (!turn) return [];
  turn.tokenIn = tokenIn;
  turn.tokenOut = tokenOut;
  return [{ op: "set_turn_tokens", turnNumber: turn.turnNumber, tokenIn, tokenOut }];
}

export function updatePromptAnswer(s: Session, toolUseId: string, answer: string): Patch[] {
  for (const turn of s.turns) {
    if (
      turn.prompt &&
      turn.prompt.type === "question" &&
      turn.prompt.toolUseId === toolUseId
    ) {
      turn.prompt.answer = answer;
      if (turn.prompt.submitted) {
        turn.prompt.elapsed = (Date.now() - turn.prompt.submitted) / 1000;
      }
      return [{ op: "set_prompt_answer", turnNumber: turn.turnNumber, toolUseId, answer }];
    }
  }
  return [];
}

// ── Tool management ──────────────────────────────────────────────────

/**
 * Ensure a step exists for the given context.
 * Creates a new step if assistant text/thinking has changed.
 */
function ensureStep(steps: StepNode[], thinking?: string | null, text?: string | null): { step: StepNode; isNew: boolean } {
  const current = steps.length > 0 ? steps[steps.length - 1] : null;

  if (current) {
    // Check if we need a new step (new assistant text/thinking boundary)
    const needsNew =
      (text && text.length > 0 && current.text && current.text.length > 0 && !text.startsWith(current.text)) ||
      (thinking && thinking.length > 0 && current.thinking && current.thinking.length > 0 && thinking !== current.thinking);

    if (!needsNew) {
      // Update existing step's text/thinking if not yet set
      if (text && text.length > 0 && (!current.text || current.text.length === 0)) {
        current.text = text;
      }
      if (thinking && thinking.length > 0 && (!current.thinking || current.thinking.length === 0)) {
        current.thinking = thinking;
      }
      return { step: current, isNew: false };
    }
  }

  const newStep = createStepNode(thinking, text);
  steps.push(newStep);
  return { step: newStep, isNew: true };
}

export function addTool(
  s: Session,
  tool: Tool,
  agentId?: string | null,
  candidateAgentIds?: string[] | null,
): Patch[] {
  // Dedup by tool_use_id
  if (tool.toolUseId && s.toolIndex[tool.toolUseId]) return [];

  const patches: Patch[] = [];

  // Set ambiguous flag
  if (agentId === "ambiguous") {
    tool.ambiguous = true;
    tool.candidateAgentIds = candidateAgentIds ?? null;
  }

  // Register in tool index
  const turnNode = getTurnNode(s, tool.turn) || currentTurnNode(s);
  if (!turnNode) return [];

  if (agentId && agentId !== "ambiguous") {
    // Route to agent's steps
    const agent = s.agentIndex[agentId];
    if (agent) {
      const agentObj = findAgent(s, agentId);
      if (agentObj) {
        const { step } = ensureStep(agentObj.steps, tool.assistantThinking, tool.assistantText);
        step.tools.push(tool);
        agentObj.toolCount++;
        tool.parentAgentId = agentId;

        const stepIdx = agentObj.steps.indexOf(step);
        if (tool.toolUseId) {
          s.toolIndex[tool.toolUseId] = {
            turnNumber: turnNode.turnNumber,
            stepIndex: stepIdx,
            toolIndex: step.tools.length - 1,
            agentId,
          };
        }
        patches.push({
          op: "add_tool",
          turnNumber: turnNode.turnNumber,
          stepIndex: stepIdx,
          agentId,
          tool,
        });
      }
    }
  } else {
    // Route to turn's root steps
    const { step, isNew } = ensureStep(turnNode.steps, tool.assistantThinking, tool.assistantText);

    // Dedup assistant text with current step (parallel tool calls share text)
    if (!isNew && tool.assistantText === step.text) {
      tool.assistantText = null;
    }
    if (!isNew && tool.assistantThinking === step.thinking) {
      tool.assistantThinking = null;
    }

    step.tools.push(tool);
    turnNode.toolCount++;

    const stepIdx = turnNode.steps.indexOf(step);
    if (tool.toolUseId) {
      s.toolIndex[tool.toolUseId] = {
        turnNumber: turnNode.turnNumber,
        stepIndex: stepIdx,
        toolIndex: step.tools.length - 1,
        agentId: null,
      };
    }

    patches.push({
      op: "add_tool",
      turnNumber: turnNode.turnNumber,
      stepIndex: stepIdx,
      tool,
    });
  }

  // Update session total tool count
  s.totalToolCount++;

  return patches;
}

export function completeTool(
  s: Session,
  toolUseId: string,
  result: unknown,
  status: "done" | "error" = "done",
  postText?: string,
  postThinking?: string,
): Patch[] {
  const loc = s.toolIndex[toolUseId];
  if (!loc) return [];

  const tool = findToolByLocation(s, loc);
  if (!tool) return [];

  const cleanResult = stripBloatedFields(result);

  tool.status = status;
  tool.result = cleanResult;
  if (tool.timestamp) {
    tool.duration = (Date.now() - tool.timestamp) / 1000;
  }
  if (postText) tool.postText = postText;
  if (postThinking) tool.postThinking = postThinking;

  return [{
    op: "complete_tool",
    toolUseId,
    result: cleanResult,
    status,
    duration: tool.duration ?? undefined,
    postText,
    postThinking,
  }];
}

/**
 * Update a running tool's partial result (pi's `tool_execution_update`).
 *
 * Replaces `tool.partial` with the new value (cumulative-snapshot model,
 * not append). Returns empty patches if the tool is unknown or already
 * completed — late `_update`s arriving after `_end` are silently dropped.
 *
 * Terminals choose how to render `partial`: ignore for terminals that
 * only show the final result, or surface live progress for long-running
 * tools (bash builds, etc.).
 */
export function updateToolPartial(
  s: Session,
  toolUseId: string,
  partial: unknown,
): Patch[] {
  const loc = s.toolIndex[toolUseId];
  if (!loc) return [];
  const tool = findToolByLocation(s, loc);
  if (!tool) return [];
  if (tool.status !== "running") return [];
  tool.partial = partial;
  return [{ op: "update_tool_partial", toolUseId, partial }];
}

export function findTool(s: Session, toolUseId: string): Tool | undefined {
  const loc = s.toolIndex[toolUseId];
  if (!loc) return undefined;
  return findToolByLocation(s, loc);
}

function findToolByLocation(s: Session, loc: ToolLocation): Tool | undefined {
  if (loc.agentId) {
    const agent = findAgent(s, loc.agentId);
    if (!agent) return undefined;
    const step = agent.steps[loc.stepIndex];
    if (!step) return undefined;
    return step.tools[loc.toolIndex];
  }
  const turn = getTurnNode(s, loc.turnNumber);
  if (!turn) return undefined;
  const step = turn.steps[loc.stepIndex];
  if (!step) return undefined;
  return step.tools[loc.toolIndex];
}

// ── Agent management ─────────────────────────────────────────────────

export function addAgent(s: Session, agent: Agent): Patch[] {
  if (s.agentIndex[agent.agentId]) return [];

  const turn = currentTurnNode(s);
  if (!turn) return [];

  // Register in index
  s.agentIndex[agent.agentId] = {
    turnNumber: turn.turnNumber,
    agentIndex: turn.agents.length,
  };

  turn.agents.push(agent);
  turn.agentCount++;

  // Link to Task tool
  linkAgentToTaskTool(s, turn, agent);

  return [{ op: "add_agent", agent }];
}

export function completeAgent(
  s: Session,
  agentId: string,
  opts?: { stopText?: string; stopThinking?: string; transcriptPath?: string },
): Patch[] {
  const agent = findAgent(s, agentId);
  if (!agent) return [];

  agent.status = "done";
  if (agent.timestamp) {
    agent.duration = (Date.now() - agent.timestamp) / 1000;
  }
  if (opts?.stopText) agent.stopText = opts.stopText;
  if (opts?.stopThinking) agent.stopThinking = opts.stopThinking;
  if (opts?.transcriptPath) agent.transcriptPath = opts.transcriptPath;

  return [{
    op: "complete_agent",
    agentId,
    stopText: opts?.stopText,
    stopThinking: opts?.stopThinking,
    duration: agent.duration ?? undefined,
    transcriptPath: opts?.transcriptPath,
  }];
}

function findAgent(s: Session, agentId: string): Agent | undefined {
  const loc = s.agentIndex[agentId];
  if (!loc) return undefined;
  const turn = getTurnNode(s, loc.turnNumber);
  if (!turn) return undefined;
  return turn.agents[loc.agentIndex];
}

function linkAgentToTaskTool(s: Session, turn: TurnNode, agent: Agent): void {
  // Scan steps in reverse for matching unlinked Task tool
  for (let si = turn.steps.length - 1; si >= 0; si--) {
    const step = turn.steps[si];
    for (let ti = step.tools.length - 1; ti >= 0; ti--) {
      const tool = step.tools[ti];
      if (
        tool.name === "Task" &&
        (tool.input as Record<string, unknown>)?.subagent_type === agent.type &&
        !tool.agentId
      ) {
        tool.agentId = agent.agentId;
        agent.taskToolUseId = tool.toolUseId;
        return;
      }
    }
  }
}

// ── File tracking ────────────────────────────────────────────────────

export function trackFile(s: Session, toolName: string, toolInput: Record<string, unknown> | undefined): Patch[] {
  if (!toolInput) return [];

  let path: string | undefined;
  let op: string | undefined;

  switch (toolName) {
    case "Read": path = toolInput.file_path as string; op = "read"; break;
    case "Edit": path = toolInput.file_path as string; op = "edit"; break;
    case "MultiEdit": path = toolInput.file_path as string; op = "edit"; break;
    case "NotebookEdit":
      path = (toolInput.notebook_path ?? toolInput.file_path) as string;
      op = "edit";
      break;
    case "Write": path = toolInput.file_path as string; op = "write"; break;
    default: return [];
  }

  if (!path || !op) return [];

  const entry = s.files[path];
  if (entry) {
    if (!entry.ops.includes(op)) entry.ops.push(op);
    entry.lastTouched = Date.now();
  } else {
    s.files[path] = { ops: [op], lastTouched: Date.now() };
  }

  return [{ op: "track_file", path, fileOp: op }];
}

// ── Task tracking ────────────────────────────────────────────────────

export function trackTask(
  s: Session,
  eventType: string,
  toolName: string,
  toolInput: Record<string, unknown> | undefined,
  toolUseId: string,
  toolResponse?: unknown,
): Patch[] {
  if (eventType === "PreToolUse" && toolName === "TaskCreate") {
    const task: Task = {
      taskId: `_pending_${toolUseId}`,
      subject: (toolInput?.subject as string) ?? null,
      description: (toolInput?.description as string) ?? null,
      activeForm: (toolInput?.activeForm as string) ?? null,
      status: "pending",
      turn: s.currentTurn,
    };
    s.tasks[task.taskId] = task;

    // Add to current turn's tasks
    const turn = currentTurnNode(s);
    if (turn) turn.tasks.push(task);

    return [{ op: "update_task", taskId: task.taskId, task }];
  }

  if (eventType === "PreToolUse" && toolName === "TaskUpdate") {
    const taskId = toolInput?.taskId as string;
    if (!taskId) return [];
    const task = s.tasks[taskId];
    if (!task) return [];

    if (toolInput?.status) task.status = toolInput.status as Task["status"];
    if (toolInput?.subject) task.subject = toolInput.subject as string;
    if (toolInput?.description) task.description = toolInput.description as string;
    if (toolInput?.activeForm) task.activeForm = toolInput.activeForm as string;

    return [{ op: "update_task", taskId, task }];
  }

  if (eventType === "PostToolUse" && toolName === "TaskCreate") {
    const tempKey = `_pending_${toolUseId}`;
    const task = s.tasks[tempKey];
    const resp = toolResponse as Record<string, unknown> | undefined;
    const taskData = resp?.task as Record<string, unknown> | undefined;
    const realId = (resp?.taskId as string) || (taskData?.id as string);

    if (task && realId) {
      task.taskId = realId;
      s.tasks[realId] = task;
      delete s.tasks[tempKey];
      return [{ op: "update_task", taskId: realId, task }];
    }
  }

  if (eventType === "PostToolUse" && toolName === "TaskList") {
    const resp = toolResponse as Record<string, unknown> | undefined;
    const taskList = resp?.tasks as Array<Record<string, unknown>> | undefined;
    if (!taskList || !Array.isArray(taskList)) return [];

    const patches: Patch[] = [];
    for (const t of taskList) {
      const id = t.id as string;
      if (!id) continue;
      const existing = s.tasks[id];
      const task: Task = {
        taskId: id,
        subject: (t.subject as string) ?? null,
        description: (t.description as string) ?? null,
        activeForm: (t.activeForm as string) ?? existing?.activeForm ?? null,
        status: (t.status as Task["status"]) ?? "pending",
        turn: existing?.turn ?? 0,
      };
      s.tasks[id] = task;
      patches.push({ op: "update_task", taskId: id, task });
    }
    return patches;
  }

  return [];
}

// ── Compaction ───────────────────────────────────────────────────────

/**
 * Record a pi compaction marker on the session. Append-only; never
 * mutated thereafter. The marker captures the turn that was current at
 * the time, so terminals can group markers by turn for inline rendering.
 *
 * Idempotent under repeated calls only insofar as duplicate markers are
 * acceptable — the translator should emit one marker per `compaction_end`.
 * Aborted compactions are still recorded (`aborted: true`) so the user
 * sees the attempt.
 */
export function addCompaction(s: Session, marker: CompactionMarker): Patch[] {
  s.compactions.push(marker);
  return [{ op: "add_compaction", marker }];
}

// ── Token tracking helpers ───────────────────────────────────────────

/** Previous token usage baseline for computing per-turn deltas. */
const prevTokenUsage = new WeakMap<Session, { totalIn: number; totalOut: number }>();

export function updateTurnTokens(s: Session, usage: TokenUsage): Patch[] {
  const prev = prevTokenUsage.get(s) ?? { totalIn: 0, totalOut: 0 };
  const newIn = (usage.input_tokens ?? 0) + (usage.cache_read_input_tokens ?? 0) + (usage.cache_creation_input_tokens ?? 0);
  const newOut = usage.output_tokens ?? 0;
  const deltaIn = Math.max(0, newIn - prev.totalIn);
  const deltaOut = Math.max(0, newOut - prev.totalOut);
  return setTurnTokens(s, deltaIn, deltaOut);
}

export function finalizeTurnTokens(s: Session, usage: TokenUsage): Patch[] {
  const patches = updateTurnTokens(s, usage);
  const newIn = (usage.input_tokens ?? 0) + (usage.cache_read_input_tokens ?? 0) + (usage.cache_creation_input_tokens ?? 0);
  const newOut = usage.output_tokens ?? 0;
  prevTokenUsage.set(s, { totalIn: newIn, totalOut: newOut });
  return patches;
}
