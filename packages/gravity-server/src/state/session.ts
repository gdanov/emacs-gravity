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
} from "@gravity/shared";

// ── Factory ──────────────────────────────────────────────────────────

/** Create a new empty session. */
export function createSession(sessionId: string, cwd: string): Session {
  const project = cwd.split("/").pop() || cwd;
  return {
    sessionId,
    cwd,
    project,
    status: "active",
    claudeStatus: "idle",
    slug: null,
    branch: null,
    pid: null,
    modelName: null,
    startTime: Date.now(),
    lastEventTime: Date.now(),
    tokenUsage: null,
    plan: null,
    streamingText: null,
    permissionMode: null,
    turns: [createTurnNode(0)], // turn 0 = pre-prompt activity
    currentTurn: 0,
    toolIndex: {},
    agentIndex: {},
    tasks: {},
    files: {},
    totalToolCount: 0,
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
    tokenIn: null,
    tokenOut: null,
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

export function resetSession(s: Session): void {
  s.status = "active";
  s.claudeStatus = "idle";
  s.turns = [createTurnNode(0)];
  s.currentTurn = 0;
  s.toolIndex = {};
  s.agentIndex = {};
  s.tasks = {};
  s.files = {};
  s.totalToolCount = 0;
  s.plan = null;
  s.streamingText = null;
  s.tokenUsage = null;
  s.lastEventTime = Date.now();
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

export function setPlan(s: Session, plan: Plan | null): Patch[] {
  s.plan = plan;
  return [{ op: "set_plan", plan }];
}

export function updateMeta(
  s: Session,
  opts: { pid?: number; slug?: string; branch?: string; modelName?: string },
): Patch[] {
  s.lastEventTime = Date.now();
  if (opts.pid && opts.pid > 0) s.pid = opts.pid;
  if (opts.slug && !s.slug) s.slug = opts.slug;
  if (opts.branch) s.branch = opts.branch;
  if (opts.modelName) s.modelName = opts.modelName;
  return [{ op: "set_meta", ...opts }];
}

// ── Turn management ──────────────────────────────────────────────────

function currentTurnNode(s: Session): TurnNode {
  return s.turns[s.turns.length - 1];
}

function getTurnNode(s: Session, turnNumber: number): TurnNode | undefined {
  return s.turns.find((t) => t.turnNumber === turnNumber);
}

export function addPrompt(s: Session, entry: PromptEntry): Patch[] {
  const patches: Patch[] = [];

  // Freeze previous turn
  const prev = currentTurnNode(s);
  if (prev && !prev.frozen) {
    prev.frozen = true;
    patches.push({ op: "freeze_turn", turnNumber: prev.turnNumber });
  }

  // Create new turn
  s.currentTurn++;
  const turn = createTurnNode(s.currentTurn);
  turn.prompt = entry;
  s.turns.push(turn);

  patches.push({ op: "add_turn", turn });
  patches.push({ op: "add_prompt", turnNumber: s.currentTurn, prompt: entry });
  return patches;
}

export function finalizeLastPrompt(
  s: Session,
  stopText?: string,
  stopThinking?: string,
): Patch[] {
  const turn = currentTurnNode(s);
  if (!turn) return [];

  const patches: Patch[] = [];

  // Compute elapsed on prompt
  if (turn.prompt && !turn.prompt.elapsed && turn.prompt.submitted) {
    turn.prompt.elapsed = (Date.now() - turn.prompt.submitted) / 1000;
  }

  // Store stop text/thinking on turn node
  if (stopText) turn.stopText = stopText;
  if (stopThinking) turn.stopThinking = stopThinking;

  if (stopText || stopThinking) {
    patches.push({
      op: "set_turn_stop",
      turnNumber: turn.turnNumber,
      stopText: stopText,
      stopThinking: stopThinking,
    });
  }

  return patches;
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

  tool.status = status;
  tool.result = result;
  if (tool.timestamp) {
    tool.duration = (Date.now() - tool.timestamp) / 1000;
  }
  if (postText) tool.postText = postText;
  if (postThinking) tool.postThinking = postThinking;

  return [{
    op: "complete_tool",
    toolUseId,
    result,
    status,
    duration: tool.duration ?? undefined,
    postText,
    postThinking,
  }];
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
