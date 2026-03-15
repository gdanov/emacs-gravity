// Socket message contract between bridges and Emacs.
// Both Claude Code and OpenCode bridges must produce messages conforming
// to this envelope structure and per-event data schemas.

/** Valid top-level keys in the socket message envelope. */
export const VALID_ENVELOPE_KEYS = new Set([
  'event', 'session_id', 'cwd', 'pid', 'data',
  'source', 'needs_response', 'hook_input',
]);

/** Per-event required and optional fields inside `data`. */
export const EVENT_DATA_SCHEMAS: Record<string, { required: string[]; optional: string[] }> = {
  // -- Claude Code events --
  SessionStart:        { required: [], optional: ['slug', 'branch', 'temp_id', 'tmux_session'] },
  SessionEnd:          { required: [], optional: ['slug'] },
  UserPromptSubmit:    { required: [], optional: ['prompt', 'slug', 'message_id', 'agent', 'model', 'tools', 'system'] },
  PreToolUse:          { required: ['tool_use_id', 'tool_name', 'tool_input'], optional: ['assistant_text', 'assistant_thinking', 'parent_agent_id', 'model', 'requested_model', 'permission_mode', 'candidate_agent_ids', 'slug'] },
  PostToolUse:         { required: ['tool_use_id', 'tool_name'], optional: ['tool_input', 'tool_response', 'post_tool_text', 'post_tool_thinking', 'parent_agent_id', 'slug'] },
  PostToolUseFailure:  { required: ['tool_use_id', 'tool_name'], optional: ['tool_input', 'error', 'post_tool_text', 'post_tool_thinking', 'parent_agent_id', 'slug'] },
  SubagentStart:       { required: ['agent_id'], optional: ['agent_type', 'agent_transcript_path', 'slug'] },
  SubagentStop:        { required: ['agent_id'], optional: ['agent_stop_text', 'agent_stop_thinking', 'agent_tool_ids', 'agent_transcript_path', 'slug'] },
  Stop:                { required: [], optional: ['stop_text', 'stop_thinking', 'token_usage', 'slug'] },
  Notification:        { required: [], optional: ['title', 'message', 'slug'] },
  PermissionRequest:   { required: [], optional: ['slug', 'permission_id', 'permission', 'patterns', 'metadata', 'always', 'tool'] },
  PermissionAutoApproved: { required: ['tool_name', 'tool_use_id'], optional: ['command'] },

  // -- OpenCode-specific events --
  SessionStatus:       { required: ['status'], optional: ['attempt', 'message'] },
  SessionIdle:         { required: [], optional: [] },
  AssistantMessage:    { required: ['message_id'], optional: ['parent_id', 'model_id', 'provider_id', 'cost', 'tokens', 'finish', 'error'] },
  MessagePart:         { required: ['message_id', 'part_id', 'part_type'], optional: ['text', 'tool', 'delta'] },
  AskUserQuestion:     { required: ['question_id', 'questions'], optional: ['tool'] },
  VcsBranchUpdate:     { required: ['branch'], optional: [] },
  SessionUpdate:       { required: [], optional: ['title', 'slug', 'summary'] },
};

/** Events emitted by both bridges. */
export const SHARED_EVENTS = ['SessionStart', 'SessionEnd', 'UserPromptSubmit'] as const;

export interface ValidationResult {
  valid: boolean;
  errors: string[];
}

/** Validate the top-level envelope structure of a socket message. */
export function validateEnvelope(msg: unknown): ValidationResult {
  const errors: string[] = [];

  if (typeof msg !== 'object' || msg === null || Array.isArray(msg)) {
    return { valid: false, errors: ['Message must be a non-null object'] };
  }

  const obj = msg as Record<string, unknown>;

  // Required fields
  if (typeof obj.event !== 'string' || obj.event.length === 0) {
    errors.push('event must be a non-empty string');
  }
  if (typeof obj.session_id !== 'string' || (obj.session_id as string).length === 0) {
    errors.push('session_id must be a non-empty string');
  }
  if (typeof obj.cwd !== 'string') {
    errors.push('cwd must be a string');
  }
  if (typeof obj.data !== 'object' || obj.data === null || Array.isArray(obj.data)) {
    errors.push('data must be a non-null, non-array object');
  }

  // Optional fields type check
  if ('pid' in obj && obj.pid !== null && typeof obj.pid !== 'number') {
    errors.push('pid must be a number or null');
  }
  if ('source' in obj && typeof obj.source !== 'string') {
    errors.push('source must be a string');
  }
  if ('needs_response' in obj && typeof obj.needs_response !== 'boolean') {
    errors.push('needs_response must be a boolean');
  }

  // No unknown top-level keys
  for (const key of Object.keys(obj)) {
    if (!VALID_ENVELOPE_KEYS.has(key)) {
      errors.push(`Unknown top-level key: ${key}`);
    }
  }

  return { valid: errors.length === 0, errors };
}

/** Validate the `data` object for a specific event type. */
export function validateEventData(eventName: string, data: Record<string, unknown>): ValidationResult {
  const errors: string[] = [];
  const schema = EVENT_DATA_SCHEMAS[eventName];

  if (!schema) {
    // Unknown event — no schema to validate against
    return { valid: true, errors: [] };
  }

  for (const field of schema.required) {
    if (!(field in data) || data[field] === undefined) {
      errors.push(`Missing required data field: ${field}`);
    }
  }

  return { valid: errors.length === 0, errors };
}

/** Build a Claude Code bridge envelope (mirrors sendToEmacs in index.ts). */
export function buildClaudeCodeEnvelope(
  eventName: string,
  sessionId: string,
  cwd: string,
  pid: number | null,
  data: Record<string, unknown>,
  hookInput?: unknown,
): Record<string, unknown> {
  const msg: Record<string, unknown> = {
    event: eventName,
    session_id: sessionId,
    cwd,
    pid,
    data,
  };
  if (hookInput !== undefined) msg.hook_input = hookInput;
  return msg;
}

/** Build an OpenCode bridge envelope (mirrors sendToEmacs in opencode-bridge.ts). */
export function buildOpenCodeEnvelope(
  eventName: string,
  sessionId: string,
  cwd: string,
  data: Record<string, unknown>,
  instancePort?: number,
  instanceDir?: string,
): Record<string, unknown> {
  return {
    event: eventName,
    session_id: sessionId,
    cwd,
    source: 'opencode',
    data: {
      ...data,
      instance_port: instancePort,
      instance_dir: instanceDir,
    },
  };
}
