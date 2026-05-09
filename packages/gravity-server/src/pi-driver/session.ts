// session.ts — Effort level mapping and session metadata sync
//
// Manages effort level synchronization between pi thinking levels
// and gravity-server's effort_level field in HookData.

import type { ThinkingLevel } from "./types.js";
import { EFFORT_FROM_THINKING } from "./types.js";

/**
 * Map pi thinking level to gravity effort level.
 *
 * pi levels: off, minimal, low, medium, high, xhigh
 * gravity levels: low, medium, high (capped at "high")
 */
export function thinkingToEffort(level: ThinkingLevel): string {
  return EFFORT_FROM_THINKING[level] ?? "medium";
}

/**
 * Validate and normalize a thinking level string.
 * Returns the normalized level or "medium" if invalid.
 */
export function normalizeThinkingLevel(level: string): ThinkingLevel {
  const valid: ThinkingLevel[] = ["off", "minimal", "low", "medium", "high", "xhigh"];
  if (valid.includes(level as ThinkingLevel)) {
    return level as ThinkingLevel;
  }
  return "medium";
}

/**
 * Session metadata tracked by the driver.
 */
export interface SessionMetadata {
  sessionId: string;
  cwd: string;
  modelName: string | null;
  effortLevel: string;
  thinkingLevel: ThinkingLevel;
  startedAt: number;
}

/**
 * Create initial session metadata.
 */
export function createSessionMetadata(
  sessionId: string,
  cwd: string,
  thinkingLevel: ThinkingLevel = "medium",
): SessionMetadata {
  return {
    sessionId,
    cwd,
    modelName: null,
    effortLevel: thinkingToEffort(thinkingLevel),
    thinkingLevel,
    startedAt: Date.now(),
  };
}

/**
 * Update session metadata when model is selected.
 */
export function updateModel(metadata: SessionMetadata, model: string, provider: string): SessionMetadata {
  return {
    ...metadata,
    modelName: model,
  };
}

/**
 * Update session metadata when thinking level changes.
 */
export function updateThinkingLevel(metadata: SessionMetadata, level: ThinkingLevel): SessionMetadata {
  return {
    ...metadata,
    thinkingLevel: level,
    effortLevel: thinkingToEffort(level),
  };
}
