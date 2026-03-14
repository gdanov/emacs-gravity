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
