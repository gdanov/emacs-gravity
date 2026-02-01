# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Emacs UI for Claude Code, inspired by Google's AntiGravity and Cursor. Provides a Magit-style working memory interface that displays Claude Code's plan, tasks, tool execution status, and supports commenting/annotations.

## Architecture

```
Claude Code
    ↓ (PreToolUse / PostToolUse hooks)
emacs-bridge (Node.js)
    ↓ (JSON over Unix domain socket)
claude-gravity.el (Emacs)
```

- **emacs-bridge/**: Claude Code plugin. Shell hook scripts invoke `dist/index.js` which reads event JSON from stdin and forwards it over a Unix socket to Emacs.
- **claude-gravity.el**: Emacs package. Runs a Unix socket server, receives tool events, maintains state, and renders a Magit-section based UI with transient menus.

The socket path is `claude-gravity.sock` in the package directory. The bridge always returns valid JSON to stdout to avoid breaking Claude Code, even on errors.

## Build Commands

```bash
# Build the Node.js bridge (required after editing emacs-bridge/src/index.ts)
cd emacs-bridge && npm run build

# Install Node.js dependencies (first time setup)
cd emacs-bridge && npm install
```

For the Emacs Lisp code we use the `emacs` MCP to re-evaluate code.

## Emacs Dependencies

- `emacs >= 27.1`
- `magit-section >= 3.0.0` (hierarchical collapsible sections)
- `transient >= 0.3.0` (menu UI)

## Hook System

Hook scripts in `emacs-bridge/hooks/` are registered via `hooks.json`. Each hook is a shell script that pipes stdin to the Node.js bridge with the event name as an argument. Currently handles `PreToolUse` and `PostToolUse`.

## Debugging & testing

The Node.js bridge logs all activity to `/tmp/emacs-bridge.log`.

The emacs MCP can inspect buffers. Use that to feedback loop changes.
