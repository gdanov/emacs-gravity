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
  FileDiff,
  StructuredPatchHunk,
  Plan,
  PromptEntry,

  // Inbox
  InboxItem,
  InboxItemType,

  // Pi
  CompactionMarker,
  PiCommandDescriptor,
  PiModel,

  // Patches
  Patch,

  // Protocol messages
  ServerMessage,
  ServerPushMessage,
  ServerSignalMessage,
  TerminalMessage,
  ChangedArea,
  ProjectSummary,
  SessionSummary,
  PlanFeedback,

  // Hook socket
  HookSocketMessage,
  HookSocketResponse,
} from "./types.js";

// Protocol version (runtime value — must be a value export, not `export type`)
export { PROTOCOL_VERSION } from "./types.js";

export { isSafeBashCommand } from "./safe-bash.js";

// Services
export { FileReadError, FileWriteError, Fs, FsLive, FsTest } from "./services/index.js";
export type { FsService } from "./services/index.js";
