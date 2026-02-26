// safe-bash.ts — Auto-approve safe read-only Bash commands in PermissionRequest
//
// Classifies Bash commands as safe (read-only) or unsafe (potentially destructive).
// Safe commands are auto-approved at the bridge level, skipping the Emacs permission UI.

import { basename } from "path";
import { log } from "./log.js";

/** Commands that are inherently read-only regardless of arguments. */
const SAFE_BINARIES = new Set([
  // Filesystem read
  "ls", "cat", "head", "tail", "wc", "file", "stat", "du", "df", "tree",
  "find", "fd", "locate", "which", "whereis", "type", "readlink", "realpath",
  "basename", "dirname", "bat", "less", "more",

  // Text search/processing (stdout-only when no redirection)
  "grep", "egrep", "fgrep", "rg", "ag", "ack",
  "sort", "uniq", "cut", "tr", "awk", "gawk",
  "diff", "comm", "join", "paste", "jq", "yq",
  "xxd", "hexdump", "od",

  // Info
  "echo", "printf", "pwd", "date", "cal",
  "uname", "hostname", "whoami", "id", "groups",
  "env", "printenv", "uptime", "man", "help",

  // Process info
  "ps", "pgrep",

  // Network (read-only by default — write flags checked separately)
  "curl", "wget",

  // Trivial
  "true", "false", "test", "[", "seq", "sleep",
]);

/** Flags on curl/wget that indicate non-GET / data-writing operations. */
const CURL_WGET_DANGEROUS = /(?:^|\s)(?:-X\b|--request\b|-d\b|--data\b|--data-\w+\b|-F\b|--form\b|--upload-file\b|-T\b)/;

/**
 * Check if the raw command string contains dangerous shell constructs.
 * These make any command unsafe regardless of the binary.
 */
function hasDangerousConstructs(cmd: string): boolean {
  // Command substitution: backtick or $(
  if (/`/.test(cmd)) return true;
  if (/\$\(/.test(cmd)) return true;

  // Output redirection: any > character (conservative — blocks even quoted >)
  if (/>/.test(cmd)) return true;

  // Process substitution: <( or >(
  if (/<\(/.test(cmd)) return true;

  // Here-document: <<
  if (/<</.test(cmd)) return true;

  // Subshells and brace groups
  if (/[(){}]/.test(cmd)) return true;

  // eval, exec, source at word boundary
  if (/\beval\b/.test(cmd)) return true;
  if (/\bexec\b/.test(cmd)) return true;
  if (/\bsource\b/.test(cmd)) return true;

  // Background: trailing & not part of &&
  if (/[^&]&\s*$/.test(cmd)) return true;
  if (/^&\s*$/.test(cmd)) return true;

  return false;
}

/**
 * Split command into segments on pipe, &&, ||, ; boundaries.
 */
function splitSegments(cmd: string): string[] {
  return cmd
    .split(/\s*(?:\|\||&&|[|;])\s*/)
    .map((s) => s.trim())
    .filter((s) => s.length > 0);
}

/**
 * Extract the binary name from a command segment.
 * Strips leading env-var assignments (FOO=bar cmd) and paths (/usr/bin/cmd).
 */
function extractBinary(segment: string): string | null {
  let s = segment;
  // Skip env var prefix assignments: VAR=val cmd
  while (/^\w+=\S*\s/.test(s)) {
    s = s.replace(/^\w+=\S*\s+/, "");
  }
  const firstToken = s.split(/\s+/)[0];
  if (!firstToken) return null;
  return basename(firstToken);
}

/**
 * Check per-binary dangerous flags for binaries that are safe by default
 * but have dangerous subcommands/flags.
 */
function hasPerBinaryDangerousFlags(binary: string, segment: string): boolean {
  if (binary === "curl" || binary === "wget") {
    return CURL_WGET_DANGEROUS.test(segment);
  }
  return false;
}

/**
 * Determine if a PermissionRequest payload is for a safe, read-only Bash command.
 * Returns true if the command can be auto-approved.
 */
export function isSafeBashCommand(inputData: any): boolean {
  if (process.env.CLAUDE_GRAVITY_NO_AUTO_APPROVE === "1") return false;

  const toolName = inputData.tool_name;
  if (toolName !== "Bash") return false;

  const command = inputData.tool_input?.command;
  if (!command || typeof command !== "string" || command.trim().length === 0) return false;

  const trimmed = command.trim();

  // Phase 1: Reject dangerous shell constructs
  if (hasDangerousConstructs(trimmed)) {
    log(`[auto-approve] REJECT (dangerous construct) — ${trimmed.substring(0, 100)}`, "debug");
    return false;
  }

  // Phase 2: Every segment's binary must be in the safe list
  const segments = splitSegments(trimmed);
  if (segments.length === 0) return false;

  for (const seg of segments) {
    const binary = extractBinary(seg);
    if (!binary || !SAFE_BINARIES.has(binary)) {
      log(`[auto-approve] REJECT (unsafe binary: ${binary}) — ${trimmed.substring(0, 100)}`, "debug");
      return false;
    }

    // Phase 3: Per-binary dangerous flag checks
    if (hasPerBinaryDangerousFlags(binary, seg)) {
      log(`[auto-approve] REJECT (dangerous flags for ${binary}) — ${trimmed.substring(0, 100)}`, "debug");
      return false;
    }
  }

  log(`[auto-approve] ALLOW — ${trimmed.substring(0, 100)}`, "info");
  return true;
}
