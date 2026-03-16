#!/bin/bash
# Installation smoke tests for emacs-gravity.
# Runs 6 levels of validation in an isolated environment.
# Expects: Node.js, npm, Emacs, tsx, claude CLI available.
# Uses temp sockets to avoid interfering with any running instance.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

EMACS="${EMACS:-emacs}"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/gravity-smoke.XXXXXX")"

export GRAVITY_HOOK_SOCK="$TMP/hooks.sock"
export GRAVITY_TERMINAL_SOCK="$TMP/terminal.sock"
export GRAVITY_PID_FILE="$TMP/server.pid"

PASSED=0
FAILED=0
SERVER_PID=""

# ── Helpers ───────────────────────────────────────────────────────────

pass() { echo "  PASS  $1"; PASSED=$((PASSED + 1)); }
fail() { echo "  FAIL  $1"; [ -n "${2:-}" ] && echo "        $2"; FAILED=$((FAILED + 1)); }

cleanup() {
  if [ -n "$SERVER_PID" ]; then
    kill -TERM "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" 2>/dev/null || true
  fi
  rm -rf "$TMP"
}
trap cleanup EXIT

version_ge() {
  # Returns 0 if $1 >= $2 (major version comparison)
  local have="$1" need="$2"
  [ "$(printf '%s\n' "$need" "$have" | sort -V | head -n1)" = "$need" ]
}

wait_for_socket() {
  local sock="$1" timeout="${2:-50}"
  local i=0
  while [ "$i" -lt "$timeout" ]; do
    [ -S "$sock" ] && return 0
    sleep 0.1
    i=$((i + 1))
  done
  return 1
}

# ── Level 1: Prerequisites ───────────────────────────────────────────

echo ""
echo "=== Level 1: Prerequisites ==="

# Node.js
node_ver="$(node --version 2>/dev/null | sed 's/^v//')" || node_ver=""
if [ -n "$node_ver" ] && version_ge "$node_ver" "18.0.0"; then
  pass "Node.js >= 18 (v$node_ver)"
else
  fail "Node.js >= 18" "got: ${node_ver:-not found}"
fi

# npm
npm_ver="$(npm --version 2>/dev/null)" || npm_ver=""
if [ -n "$npm_ver" ] && version_ge "$npm_ver" "8.0.0"; then
  pass "npm >= 8 ($npm_ver)"
else
  fail "npm >= 8" "got: ${npm_ver:-not found}"
fi

# Emacs
emacs_ver="$($EMACS --version 2>/dev/null | head -1 | grep -oE '[0-9]+\.[0-9]+')" || emacs_ver=""
if [ -n "$emacs_ver" ] && version_ge "$emacs_ver" "27.1"; then
  pass "Emacs >= 27.1 ($emacs_ver)"
else
  fail "Emacs >= 27.1" "got: ${emacs_ver:-not found}"
fi

# tsx
if tsx --version >/dev/null 2>&1; then
  pass "tsx available ($(tsx --version 2>/dev/null))"
else
  fail "tsx available" "not found — install with: npm install -g tsx"
fi

# Claude Code CLI
claude_ver="$(claude --version 2>/dev/null | head -1)" || claude_ver=""
if [ -n "$claude_ver" ]; then
  pass "Claude Code CLI available ($claude_ver)"
else
  fail "Claude Code CLI" "not found — install with: npm install -g @anthropic-ai/claude-code"
fi

# magit-section (batch mode doesn't load init.el, so we must package-initialize first)
if $EMACS --batch --eval '(progn (package-initialize) (require (quote magit-section)) (kill-emacs 0))' 2>/dev/null; then
  pass "magit-section loadable"
else
  fail "magit-section loadable" "install from MELPA"
fi

# transient
if $EMACS --batch --eval '(progn (package-initialize) (require (quote transient)) (kill-emacs 0))' 2>/dev/null; then
  pass "transient loadable"
else
  fail "transient loadable" "install from MELPA"
fi

if [ "$FAILED" -gt 0 ]; then
  echo ""
  echo "=== Prerequisites failed ($FAILED). Stopping. ==="
  exit 1
fi

# ── Level 2: Build Validation ────────────────────────────────────────

echo ""
echo "=== Level 2: Build Validation ==="

# npm install (always run in container — host node_modules may have wrong platform binaries)
echo "  ...  Running npm install..."
npm_out="$(npm install 2>&1)"
if [ $? -eq 0 ]; then
  pass "npm install succeeded"
else
  fail "npm install" "$(echo "$npm_out" | tail -3)"
fi

# Verify gravity-server entry point exists (tsx runs from source, no build needed)
if [ -f "packages/gravity-server/src/gravity-server.ts" ]; then
  pass "gravity-server.ts entry point exists"
else
  fail "gravity-server.ts" "packages/gravity-server/src/gravity-server.ts not found"
fi

# Elisp byte-compile
if $EMACS --batch -L . \
    --eval '(package-initialize)' \
    --eval '(setq byte-compile-error-on-warn t)' \
    -f batch-byte-compile claude-gravity.el 2>/dev/null; then
  pass "Elisp byte-compiles cleanly"
  rm -f claude-gravity.elc
else
  fail "Elisp byte-compile" "claude-gravity.el has warnings/errors"
  rm -f claude-gravity.elc
fi

if [ "$FAILED" -gt 0 ]; then
  echo ""
  echo "=== Build validation failed ($FAILED). Stopping. ==="
  exit 1
fi

# ── Start gravity-server for Levels 3-6 ──────────────────────────────

echo ""
echo "=== Starting gravity-server ==="

npx tsx packages/gravity-server/src/gravity-server.ts &
SERVER_PID=$!

if wait_for_socket "$GRAVITY_TERMINAL_SOCK" 50; then
  echo "  Server ready (pid $SERVER_PID)"
else
  fail "Server startup" "terminal socket not found after 5s"
  echo "=== Server failed to start. Stopping. ==="
  exit 1
fi

# ── Level 3: Server Lifecycle (via Node.js) ──────────────────────────

if node test/install-smoke.mjs level3; then
  : # pass count handled by Node.js output
else
  FAILED=$((FAILED + 1))
fi

# ── Level 4: Plugin Validation ───────────────────────────────────────

echo ""
echo "=== Level 4: Plugin Validation ==="

# Hook scripts executable
hook_dir="packages/emacs-bridge/hooks"
hook_count=0
hook_total=0
for hook in "$hook_dir"/*; do
  [ "$(basename "$hook")" = "_ensure-server" ] && continue
  hook_total=$((hook_total + 1))
  if [ -x "$hook" ]; then
    hook_count=$((hook_count + 1))
  fi
done
if [ "$hook_count" -eq "$hook_total" ]; then
  pass "Hook scripts executable ($hook_count/$hook_total)"
else
  fail "Hook scripts executable" "$hook_count/$hook_total executable"
fi

# claude plugin validate
if claude plugin validate packages/emacs-bridge 2>&1 | grep -q "Validation passed"; then
  pass "claude plugin validate (emacs-bridge)"
else
  fail "claude plugin validate (emacs-bridge)"
fi

if claude plugin validate .claude-plugin/marketplace.json 2>&1 | grep -q "Validation passed"; then
  pass "claude plugin validate (marketplace)"
else
  fail "claude plugin validate (marketplace)"
fi

# Hook event creates session (via Node.js)
if node test/install-smoke.mjs level4; then
  : # pass count handled by Node.js output
else
  FAILED=$((FAILED + 1))
fi

# ── Level 5: Emacs Client ────────────────────────────────────────────

echo ""
echo "=== Level 5: Emacs Client ==="

EMACS_OUTPUT="$TMP/emacs-test.log"
if $EMACS --batch --eval '(package-initialize)' -L . -L test \
    -l claude-gravity-smoke-test \
    -f ert-run-tests-batch-and-exit > "$EMACS_OUTPUT" 2>&1; then
  grep -E '(passed|FAILED|failed)' "$EMACS_OUTPUT" | while IFS= read -r line; do
    echo "  $line"
  done
  PASSED=$((PASSED + 3))
else
  cat "$EMACS_OUTPUT"
  FAILED=$((FAILED + 1))
fi

# ── Level 6: End-to-End ──────────────────────────────────────────────

if node test/install-smoke.mjs level6; then
  : # pass count handled by Node.js output
else
  FAILED=$((FAILED + 1))
fi

# ── Summary ──────────────────────────────────────────────────────────

echo ""
if [ "$FAILED" -gt 0 ]; then
  echo "=== FAILED ($FAILED shell-level failures) ==="
  exit 1
else
  echo "=== ALL LEVELS PASSED ==="
fi
