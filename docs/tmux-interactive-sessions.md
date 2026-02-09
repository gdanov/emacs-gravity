# Tmux-based Interactive Claude Sessions

> **See also:** @/Users/gdanov/work/playground/emacs-gravity/DEVELOPMENT.md for testing procedures and tmux integration notes.

## Overview

This document describes the validated approach for controlling interactive Claude CLI (without `-p` flag) from Emacs using tmux and send-keys, and compares it with the programmatic JSON I/O approach.

## The Challenge

Claude Code CLI uses the Ink library's `ink-text-input` component for input handling. Ink distinguishes between physical keyboard input and programmatic stdin:

- **Physical keyboard**: Enter keypress triggers `onSubmit` handler → prompt submitted
- **Programmatic stdin**: `\n` or `\r` sent to stdin is treated as literal newline → prompt NOT submitted

This limitation is documented in [GitHub issue #15553](https://github.com/vadimdemedes/ink/issues/423).

## Solution: tmux send-keys

The tmux `send-keys` command sends actual keystrokes to the terminal session, not stdin. This triggers Ink's `onSubmit` handler properly.

### Implementation

```elisp
;; Start tmux session with TERM=dumb and hooks enabled
(defun claude-gravity-experiment-tmux-session (cwd)
  "Run interactive Claude in tmux with TERM=dumb, controlled via send-keys."
  (let* ((session-name (format "claude-tmux-%s" (format-time-string "%s")))
         (plugin-dir (expand-file-name "emacs-bridge" ...)))
    (call-process
     "tmux" nil nil nil
     "new-session" "-d" "-s" session-name
     "-c" cwd
     "env" "TERM=dumb" "claude" "--plugin-dir" plugin-dir)))

;; Send prompts using send-keys (not stdin)
(defun claude-gravity-experiment-tmux-send (prompt)
  "Send PROMPT to tmux session using send-keys."
  ;; Send text
  (call-process "tmux" nil nil nil "send-keys" "-t" session-name "-l" prompt)
  ;; Send Enter keystroke
  (call-process "tmux" nil nil nil "send-keys" "-t" session-name "Enter"))
```

### Key Points

1. **TERM=dumb**: Use `"env" "TERM=dumb" "claude"` as the command, NOT `-e "TERM=dumb"` flag
   - tmux overrides `-e` with its own TERM value (tmux-256color)
   - Using `env` in the command bypasses this override

2. **Plugin hooks**: Works with `--plugin-dir` to enable hooks (SessionStart, UserPromptSubmit, PreToolUse, PostToolUse, Stop)

3. **Prompt submission**: Use `-l` flag for literal text, then separate `send-keys "Enter"` for submission

## Validated Features

### Full Hook Lifecycle
All hooks fire correctly with interactive Claude (no `-p` flag):
- ✅ SessionStart
- ✅ UserPromptSubmit
- ✅ PreToolUse
- ✅ PostToolUse
- ✅ Stop
- ✅ SessionEnd

### Multi-turn Conversation
```elisp
;; First turn
(claude-gravity-experiment-tmux-send "What is 2 + 2?")
;; Response: "4"

;; Second turn with tool execution
(claude-gravity-experiment-tmux-send "List files in current directory")
;; Hook sequence: UserPromptSubmit → PreToolUse → PostToolUse → Stop
```

### Output Capture
```elisp
;; Capture pane content
(defun claude-gravity-experiment-tmux-capture ()
  (with-temp-buffer
    (call-process "tmux" nil t nil "capture-pane" "-t" session-name "-p")
    (buffer-string)))
```

## Comparison with JSON I/O Approach

### JSON I/O (Current Managed Sessions)

**Implementation**:
```elisp
(make-process
  :command '("claude" "-p"
             "--input-format" "stream-json"
             "--output-format" "stream-json")
  :filter #'claude-gravity--managed-process-filter)
```

**Advantages**:
- ✅ Precise event boundaries (structured JSON events)
- ✅ Rich metadata (tool IDs, content block IDs, agent IDs)
- ✅ Streaming text deltas (word-by-word updates)
- ✅ Partial message support
- ✅ No terminal emulation overhead

**Disadvantages**:
- ❌ Requires JSON parsing
- ❌ Dependent on CLI `--output-format` stability
- ❌ Can't see "raw" Claude output formatting

### Tmux Interactive (Validated Alternative)

**Advantages**:
- ✅ Uses standard interactive Claude (no `-p` flag)
- ✅ Hook-based event detection (no JSON parsing needed)
- ✅ Simpler architecture (tmux handles I/O)
- ✅ `TERM=dumb` compatibility validated
- ✅ Can capture "raw" output formatting

**Disadvantages**:
- ❌ Text extraction from terminal buffer (less structured)
- ❌ No streaming deltas (buffer updates are atomic)
- ❌ No content block IDs or tool attribution metadata
- ❌ Requires tmux dependency
- ❌ Higher overhead (terminal emulation)

## Failed Approaches (Documented for Reference)

### Approach A: eat Terminal Emulator
**Problem**: eat overrides `TERM` to `eat-truecolor`, can't use `TERM=dumb`
**Result**: ❌ Failed - TERM incompatibility

### Approach B: Direct process with stdin
**Problem**: `process-send-string` sends to stdin, not interpreted as submission by Ink
**Result**: ❌ Failed - Ink library limitation

### Approach C: expect Scripts
**Problem**: Even expect couldn't trigger Ink's onSubmit handler consistently
**Result**: ❌ Failed - unreliable submission

## Recommendation

**Use JSON I/O (`-p` flag) for managed sessions** where you need:
- Precise metadata and event boundaries
- Streaming updates
- Tool/agent attribution
- Robust parsing

**Use tmux interactive approach** when you need:
- Standard interactive Claude experience
- Raw output formatting
- Simpler hook-based integration
- Testing/debugging interactive mode

## References

- Ink library issue: https://github.com/vadimdemedes/ink/issues/423
- tmux send-keys: `man tmux` (SEND-KEYS section)
- Implementation: `claude-gravity.el` lines ~4020-4100
- Validation session: `/Users/gdanov/.claude/projects/.../9617d6ba-2cf0-47c9-b270-f6f578cb1337.jsonl`
