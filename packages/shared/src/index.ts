// @gravity/shared — Shared types and utilities for the gravity ecosystem

export type {
  // Hook data
  HookData,
  HookEventName,
  TokenUsage,

  // View model
  Session,
  TurnNode,
  StepNode,
  Tool,
  ToolLocation,
  Agent,
  AgentLocation,
  Task,
  FileEntry,
  Plan,
  PromptEntry,

  // Inbox
  InboxItem,
  InboxItemType,

  // Patches
  Patch,

  // Protocol messages
  ServerMessage,
  TerminalMessage,
  ProjectSummary,
  SessionSummary,
  PlanFeedback,

  // Hook socket
  HookSocketMessage,
  HookSocketResponse,
} from "./types.js";

export { isSafeBashCommand } from "./safe-bash.js";
