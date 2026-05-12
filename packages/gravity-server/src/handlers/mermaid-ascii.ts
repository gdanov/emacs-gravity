// mermaid-ascii.ts — Render mermaid diagrams to ASCII art
// ⚠️ VIBECODED EXPERIMENT — This code was generated in a single AI-assisted session.
// It may contain bugs, design issues, or unexpected behavior. Use with caution.
// Uses beautiful-mermaid library for parsing and ASCII rendering.
// Provides both sync and async APIs for different contexts.

import { renderMermaidASCII } from "beautiful-mermaid";

export interface RenderMermaidOptions {
  /** Use ASCII chars (+-|) instead of Unicode box-drawing. Default: false */
  useAscii?: boolean;
  /** Theme colors for ASCII output */
  theme?: {
    fg?: string;
    border?: string;
    line?: string;
    arrow?: string;
  };
}

export interface RenderMermaidParams {
  source: string;
  options?: RenderMermaidOptions;
}

/**
 * Render a mermaid diagram source text to an ASCII art string.
 * Uses Unicode box-drawing characters by default for better visual quality.
 *
 * @param source - Mermaid diagram source (flowchart, sequence, class, etc.)
 * @param options - Rendering options
 * @returns ASCII art string, or error message if rendering fails
 */
export function renderMermaidToASCII(
  source: string,
  options: RenderMermaidOptions = {},
): string {
  try {
    const result = renderMermaidASCII(source, {
      colorMode: "none", // Plain text, no ANSI codes
      useAscii: options.useAscii ?? false,
      theme: options.theme,
    });
    return result;
  } catch (err) {
    const error = err as Error;
    return `[Mermaid render error: ${error.message}]`;
  }
}

/**
 * Render mermaid diagram with ANSI colors for terminal display.
 *
 * @param source - Mermaid diagram source
 * @param options - Rendering options
 * @returns ASCII art with ANSI color codes
 */
export function renderMermaidToANSI(
  source: string,
  options: RenderMermaidOptions = {},
): string {
  try {
    const result = renderMermaidASCII(source, {
      colorMode: "ansi256", // Use 256-color ANSI codes
      useAscii: options.useAscii ?? false,
      theme: options.theme,
    });
    return result;
  } catch (err) {
    const error = err as Error;
    return `[Mermaid render error: ${error.message}]`;
  }
}

/**
 * Parse mermaid source and return structured graph data.
 * Useful for debugging or custom rendering.
 */
export function parseMermaid(source: string): object {
  try {
    // Note: beautiful-mermaid's parseMermaid is internal API
    // For now, just validate by attempting render
    renderMermaidASCII(source, { colorMode: "none" });
    return { valid: true, source: source.slice(0, 100) };
  } catch {
    return { valid: false, source: source.slice(0, 100) };
  }
}

// ── Error message constants ───────────────────────────────────────────

export const ERROR_MESSAGES = {
  EMPTY_SOURCE: "[Mermaid: empty source]",
  INVALID_SYNTAX: "[Mermaid: invalid syntax]",
  UNSUPPORTED_TYPE: "[Mermaid: unsupported diagram type]",
} as const;