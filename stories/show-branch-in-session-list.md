# Show Git Branch in Session List

## Summary

Display the current git branch name alongside each session in the overview buffer so users can identify sessions by the branch they are working on. When running multiple sessions across different branches of the same project, the branch name is the most natural differentiator. Falls back to displaying the cwd when not inside a git repository.

## User Stories

- As a user running multiple Claude Code sessions on the same project, I want to see which branch each session is on so that I can switch to the right one without opening each session.
- As a user working across projects with non-unique project names, I want to see the cwd as a fallback so that I can distinguish sessions even outside git repositories.

## Acceptance Criteria

1. Each session line in the overview buffer (`*Structured Claude Sessions*`) displays the git branch name between the session indicator and the session label, formatted as `(branch-name)` in a distinct face.
2. The branch name is extracted **bridge-side from the session transcript**. Every transcript JSONL entry contains a `gitBranch` field (e.g., `"gitBranch": "master"`). The bridge already reads the transcript for slug extraction, assistant text, and token usage — extracting `gitBranch` is one additional field read. The bridge adds `branch` to the socket message sent to Emacs.
3. When `gitBranch` is absent from the transcript (non-git directory or missing transcript), the bridge omits the `branch` field. Emacs shows the abbreviated cwd instead, formatted as `(~/path/to/dir)` in the same face.
4. A detached HEAD state (`HEAD` returned by `git rev-parse --abbrev-ref`) is displayed literally as `(HEAD)`.
5. Since the bridge runs on every hook event, the branch value is naturally refreshed on every event. Emacs stores the latest value, so branch changes mid-session are picked up on the next hook invocation.
6. The session header-line (`claude-gravity--build-header-line`) also displays the branch or cwd fallback, using the same format and face as the overview line.
7. The `claude-gravity-switch-session` completing-read candidates include the branch name in each entry, between the status and the session label.
8. A new face `claude-gravity-branch` is defined in `claude-gravity-faces.el` for the branch/cwd text (suggested: muted teal or cyan, no bold).
9. The transcript read is already happening in the bridge — extracting `gitBranch` adds negligible overhead (one field access on an already-parsed JSON line).

## Edge Cases

- **Non-git directory**: Session cwd is `/tmp` or another non-repo path. Transcript entries have no `gitBranch` or it's empty. `:branch` is `nil`. Display shows abbreviated cwd `(/tmp)` using the same face.
- **Detached HEAD**: User checked out a commit hash. `git rev-parse --abbrev-ref HEAD` returns literal string `HEAD`. Display shows `(HEAD)`.
- **Branch name with slashes**: Branch is `feature/auth/login-fix`. Display shows full name `(feature/auth/login-fix)` -- no truncation.
- **Branch changes mid-session**: User runs `git checkout other-branch` between prompts. The next hook event (any type) refreshes `:branch` to `other-branch`.
- **Very long branch name (>40 chars)**: Display truncates to 37 chars + `...` to prevent the session line from becoming unreadable. Example: `(feature/very-long-branch-name-that...)`.
- **Session with no cwd**: Legacy or malformed session where `:cwd` is empty string. Display omits the branch/cwd component entirely (no empty parentheses).
- **Remote/network filesystem**: No concern — the bridge reads `gitBranch` from the transcript file (already local), not by running `git` commands.
- **Ended sessions**: Branch is still displayed for ended sessions (it was captured when the session was active). No refresh attempted for ended sessions.

## Out of Scope

- **Branch change detection via filesystem watcher**: We will not watch `.git/HEAD` for changes. The bridge already refreshes on every hook event.
- **Emacs-side git calls**: We will NOT run `git` from Emacs. The bridge extracts the branch from the transcript and sends it in the socket message, keeping Emacs as a pure consumer of event data.
- **Bridge-side git commands**: We will NOT run `git rev-parse` in the bridge. The `gitBranch` field is already in the transcript, written by Claude Code itself.
- **Project grouping by branch**: The overview groups sessions by `:project` (directory basename). This story does not change the grouping key.
- **Editable branch labels**: Users cannot rename the branch display. This is read-only, derived from git.
- **Git remote/upstream info**: We do not show tracking branch, ahead/behind counts, or remote name.
- **Tmux session branch display**: The tmux integration has its own session naming. Branch display there is a separate concern.

## Technical Notes

### Where to compute the branch

**Bridge-side from transcript** in `emacs-bridge/src/index.ts`. The bridge already reads the transcript for slug extraction (lines 701-709), assistant text, and token usage. Every transcript JSONL entry contains a `gitBranch` field set by Claude Code.

Hook stdin does NOT contain `gitBranch` — verified by capturing raw payload:
```
session_id, cwd, hook_event_name, permission_mode,
transcript_path, tool_use_id, tool_name, tool_input, tool_response
```

But the transcript file has it on every entry:
```json
{"type":"progress", "gitBranch":"master", "cwd":"/path/to/project", ...}
```

Implementation: extract `gitBranch` from the last transcript entry (or from the same read used for slug extraction) and include it as `branch` in the socket message to Emacs:

```typescript
// In extractSlug or a new extractGitBranch function:
function extractGitBranch(transcriptPath: string): string | null {
  // Read last line of transcript (or reuse existing transcript read)
  // Return entry.gitBranch || null
}
```

This adds zero external process calls — just one field access on already-parsed transcript data.

### Session plist changes

Add `:branch nil` to the initial session plist in `claude-gravity--ensure-session` (`claude-gravity-session.el`). Emacs stores the `branch` field from each incoming event via `plist-put`.

### Display format in overview

Current format (line 165 of `claude-gravity-ui.el`):
```
"●  fix-rendering   responding      [12 tools]"
```

New format:
```
"● (feature/auth)  fix-rendering   responding      [12 tools]"
```

The `(branch)` component appears right after the indicator dot and before the session label. When branch is nil and cwd is present, shows abbreviated cwd instead.

### Files to modify

| File | Change |
|------|--------|
| `emacs-bridge/src/index.ts` | Extract `gitBranch` from transcript (alongside existing slug extraction), include as `branch` in socket message |
| `claude-gravity-faces.el` | Add `claude-gravity-branch` face |
| `claude-gravity-session.el` | Add `:branch nil` to initial session plist in `--ensure-session` |
| `claude-gravity-events.el` | Store `branch` from event data via `plist-put` on every event |
| `claude-gravity-ui.el` lines 129-170 | Add branch/cwd display to overview session line |
| `claude-gravity-ui.el` lines 595-672 | Add branch/cwd display to session header-line |
| `claude-gravity-ui.el` lines 326-398 | Add branch/cwd to switch-session candidates |

### Existing patterns to follow

- Session metadata refresh on every event: see `claude-gravity-model-update-session-meta` called at line 59 of `claude-gravity-events.el` -- branch arrives in every event and is stored the same way.
- Face definition: follow the pattern of existing 37 faces in `claude-gravity-faces.el` (e.g., `claude-gravity-slug`).
- Abbreviated path: use `abbreviate-file-name` for the cwd fallback to get `~` expansion.
- Truncation: use `truncate-string-to-width` with ellipsis for branch names >40 chars.

### Relation to existing issues

This story is a concrete deliverable carved out of the broader brainstorm issue `emacs-gravity-8e0` (session title/slug identification). It addresses the "git branch" angle specifically. The parent issue remains open for other identification strategies (topic detection, user labels, summary lines).
