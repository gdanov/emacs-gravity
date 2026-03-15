// types.ts — Shared types for the gravity ecosystem
//
// Three layers:
// 1. HookData — raw hook events flowing from Claude Code through the bridge
// 2. View Model — stateful session tree (port of claude-gravity-state.el)
// 3. Protocol — messages between server and terminals

// ── Hook Data (bridge layer) ─────────────────────────────────────────

/** Token usage counters extracted from transcript. */
export interface TokenUsage {
  input_tokens: number;
  output_tokens: number;
  cache_read_input_tokens: number;
  cache_creation_input_tokens: number;
}

/**
 * Hook event data flowing through the bridge.
 *
 * Fields are grouped into two categories:
 * - **Hook input**: populated by Claude Code before the bridge runs
 * - **Enrichment output**: populated by the bridge before sending to Emacs
 *
 * All fields are optional because different event types carry different subsets.
 */
export interface HookData {
  // --- Hook input (from Claude Code) ---
  session_id?: string;
  cwd?: string;
  transcript_path?: string;
  agent_id?: string;
  tool_name?: string;
  tool_use_id?: string;
  tool_input?: {
    command?: string;
    model?: string;
    [key: string]: unknown;
  };

  // --- Enrichment output (added by bridge) ---

  // Session metadata
  temp_id?: string | null;
  tmux_session?: string;
  effort_level?: string;
  slug?: string | null;
  branch?: string | null;

  // Agent tracking
  agent_transcript_path?: string;
  agent_tool_ids?: string[];
  agent_stop_text?: string;
  agent_stop_thinking?: string;

  // Tool attribution
  parent_agent_id?: string;
  candidate_agent_ids?: string[];

  // Content extraction (PreToolUse)
  assistant_text?: string;
  assistant_thinking?: string;
  model?: string;
  requested_model?: string;

  // Content extraction (PostToolUse)
  post_tool_text?: string;
  post_tool_thinking?: string;

  // Content extraction (Stop)
  stop_text?: string;
  stop_thinking?: string;
  token_usage?: TokenUsage;

  // Pass-through for unknown fields from Claude Code
  [key: string]: unknown;
}

/** All hook event types supported by the bridge. */
export type HookEventName =
  | "SessionStart"
  | "SessionEnd"
  | "PreToolUse"
  | "PostToolUse"
  | "PostToolUseFailure"
  | "SubagentStart"
  | "SubagentStop"
  | "UserPromptSubmit"
  | "Stop"
  | "Notification"
  | "PermissionRequest"
  | "AskUserQuestionIntercept";

// ── View Model (session state tree) ──────────────────────────────────
//
// Direct port of claude-gravity-state.el / claude-gravity-session.el.
// Bidirectional links (Tool↔Agent) use ID references instead of
// object pointers.

export interface Session {
  sessionId: string;
  cwd: string;
  project: string;
  status: "active" | "ended";
  claudeStatus: "idle" | "responding";
  slug: string | null;
  branch: string | null;
  pid: number | null;
  modelName: string | null;
  tmuxSession: string | null;
  startTime: number;
  lastEventTime: number;
  tokenUsage: TokenUsage | null;
  plan: Plan | null;
  streamingText: string | null;
  permissionMode: string | null;

  // Turn tree
  turns: TurnNode[];
  currentTurn: number;

  // Indexes for O(1) lookup
  toolIndex: Record<string, ToolLocation>;
  agentIndex: Record<string, AgentLocation>;

  // Collections
  tasks: Record<string, Task>;
  files: Record<string, FileEntry>;

  totalToolCount: number;
}

/** Pointer to a tool's location in the turn tree. */
export interface ToolLocation {
  turnNumber: number;
  stepIndex: number;
  toolIndex: number;
  agentId: string | null;
}

/** Pointer to an agent's location in the turn tree. */
export interface AgentLocation {
  turnNumber: number;
  agentIndex: number;
}

export interface TurnNode {
  turnNumber: number;
  prompt: PromptEntry | null;
  steps: StepNode[];
  agents: Agent[];
  tasks: Task[];
  toolCount: number;
  agentCount: number;
  frozen: boolean;
  stopText: string | null;
  stopThinking: string | null;
  tokenIn: number | null;
  tokenOut: number | null;
}

export interface StepNode {
  thinking: string | null;
  text: string | null;
  tools: Tool[];
}

export interface Tool {
  toolUseId: string;
  name: string;
  input: Record<string, unknown>;
  status: "running" | "done" | "error";
  result: unknown;
  timestamp: number;
  duration: number | null;
  turn: number;

  // Assistant context around this tool
  assistantText: string | null;
  assistantThinking: string | null;
  postText: string | null;
  postThinking: string | null;

  // Agent attribution
  parentAgentId: string | null;
  ambiguous: boolean;
  candidateAgentIds: string[] | null;

  // Linked agent (for Task tools that spawn agents)
  agentId: string | null;
}

export interface Agent {
  agentId: string;
  type: string;
  status: "running" | "done";
  steps: StepNode[];
  toolCount: number;
  stopText: string | null;
  stopThinking: string | null;
  duration: number | null;
  timestamp: number;
  transcriptPath: string | null;

  // Linked Task tool that spawned this agent
  taskToolUseId: string | null;
}

export interface Task {
  taskId: string;
  subject: string | null;
  description: string | null;
  activeForm: string | null;
  status: "pending" | "in_progress" | "completed";
  turn: number;
}

export interface FileEntry {
  ops: string[];
  lastTouched: number;
}

export interface Plan {
  content: string;
  filePath: string | null;
  allowedPrompts: string[] | null;
}

export interface PromptEntry {
  type: "user" | "question" | "phase-boundary";
  text: string;
  submitted: number;
  elapsed: number | null;
  // For question prompts
  toolUseId: string | null;
  answer: string | null;
}

// ── Inbox ────────────────────────────────────────────────────────────

export type InboxItemType = "permission" | "question" | "plan-review" | "idle";

export interface InboxItem {
  id: number;
  type: InboxItemType;
  sessionId: string;
  project: string | null;
  label: string;
  timestamp: number;
  summary: string;
  data: Record<string, unknown>;
}

// ── Semantic Patches ─────────────────────────────────────────────────
//
// Typed operations that map 1:1 to model mutations.
// Terminals apply patches incrementally; unknown ops trigger full refresh.

export type Patch =
  | { op: "set_status"; status: "active" | "ended" }
  | { op: "set_claude_status"; claudeStatus: "idle" | "responding" }
  | { op: "set_token_usage"; usage: TokenUsage }
  | { op: "set_plan"; plan: Plan | null }
  | { op: "set_streaming_text"; text: string | null }
  | { op: "set_permission_mode"; mode: string | null }
  | { op: "set_meta"; slug?: string; branch?: string; pid?: number; modelName?: string; tmuxSession?: string }
  | { op: "add_turn"; turn: TurnNode }
  | { op: "freeze_turn"; turnNumber: number }
  | { op: "set_turn_stop"; turnNumber: number; stopText?: string; stopThinking?: string }
  | { op: "set_turn_tokens"; turnNumber: number; tokenIn: number; tokenOut: number }
  | { op: "add_step"; turnNumber: number; agentId?: string; step: StepNode }
  | { op: "add_tool"; turnNumber: number; stepIndex: number; agentId?: string; tool: Tool }
  | { op: "complete_tool"; toolUseId: string; result: unknown; status: "done" | "error"; duration?: number; postText?: string; postThinking?: string }
  | { op: "add_agent"; agent: Agent }
  | { op: "complete_agent"; agentId: string; stopText?: string; stopThinking?: string; duration?: number }
  | { op: "update_task"; taskId: string; task: Task }
  | { op: "track_file"; path: string; fileOp: string }
  | { op: "add_prompt"; turnNumber: number; prompt: PromptEntry }
  | { op: "set_prompt_answer"; turnNumber: number; toolUseId: string; answer: string };

// ── Protocol Messages ────────────────────────────────────────────────
//
// Newline-delimited JSON over Unix domain socket.
// WebSocket upgrade path for web terminals later.

/** Messages from server to terminal. */
export type ServerMessage =
  | { type: "session.snapshot"; sessionId: string; session: Session }
  | { type: "session.update"; sessionId: string; patches: Patch[] }
  | { type: "session.removed"; sessionId: string }
  | { type: "inbox.added"; item: InboxItem }
  | { type: "inbox.removed"; itemId: number }
  | { type: "overview.snapshot"; projects: ProjectSummary[] };

/** Messages from terminal to server. */
export type TerminalMessage =
  | { type: "action.permission"; itemId: number; decision: "allow" | "deny"; message?: string }
  | { type: "action.question"; itemId: number; answers: string[] }
  | { type: "action.plan-review"; itemId: number; decision: "allow" | "deny"; feedback?: PlanFeedback }
  | { type: "action.turn-auto-approve"; sessionId: string }
  | { type: "request.session"; sessionId: string }
  | { type: "request.overview" };

export interface ProjectSummary {
  project: string;
  sessions: SessionSummary[];
}

export interface SessionSummary {
  sessionId: string;
  slug: string | null;
  status: "active" | "ended";
  claudeStatus: "idle" | "responding";
  toolCount: number;
  lastEventTime: number;
}

export interface PlanFeedback {
  inlineComments: Array<{ line: number; nearText: string; comment: string }>;
  claudeMarkers: Array<{ line: number; nearText: string; text: string }>;
  diff: string | null;
  generalComment: string | null;
}

// ── Hook Socket Messages ─────────────────────────────────────────────
//
// Messages between the bridge shim and the gravity server.
// One-shot: shim connects, sends event, optionally waits for response.

export interface HookSocketMessage {
  event: HookEventName;
  session_id: string;
  cwd: string;
  pid: number | null;
  source: string;
  data: HookData;
  needs_response: boolean;
}

/**
 * Response written back to the hook socket by the gravity-server.
 *
 * In server mode, this is the full stdout-ready payload that the bridge
 * writes directly to stdout for Claude Code to consume.
 *
 * The hookSpecificOutput wrapper is required by Claude Code's hook protocol.
 * Additional top-level fields (answer, answers) are included for
 * AskUserQuestionIntercept responses.
 */
export interface HookSocketResponse {
  hookSpecificOutput?: {
    hookEventName: string;
    decision?: {
      behavior: "allow" | "deny";
      message?: string;
    };
    permissionDecision?: string;
    permissionDecisionReason?: string;
  };
  /** Legacy format (used by Emacs socket direct mode) */
  decision?: {
    behavior: "allow" | "deny";
    message?: string;
  };
  answer?: string;
  answers?: string[];
}
