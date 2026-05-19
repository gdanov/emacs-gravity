EMACS ?= emacs

.PHONY: test test-elisp test-bridge test-server test-menubar test-install test-install-shell build build-bridge build-server sync-marketplace clean menubar kill-server restart-server check-settings

# Discover the published-marketplace install cache dir (newest by mtime).
# Only needed for sync-marketplace / restart-server (published-plugin testing).
MARKETPLACE_CACHE := $(shell ls -td $(HOME)/.claude/plugins/cache/emacs-gravity-marketplace/emacs-bridge/*/ 2>/dev/null | head -1)

test: test-elisp test-bridge test-server test-menubar check-settings

test-elisp:
	$(EMACS) -nw --batch --eval '(package-initialize)' -L . -L test \
		-l claude-gravity \
		-l claude-gravity-test \
		-l claude-gravity-patch-test \
		-l claude-gravity-pull-signal-test \
		-f ert-run-tests-batch-and-exit

test-bridge:
	cd packages/emacs-bridge && npx vitest run

test-server:
	cd packages/gravity-server && npx vitest run

test-menubar:
	cd packages/gravity-menubar && swift test

build: build-bridge build-server

build-bridge:
	cd packages/emacs-bridge && node build.mjs

build-server:
	cd packages/gravity-server && node build.mjs
	cp packages/gravity-server/dist/gravity-server.mjs packages/emacs-bridge/dist/gravity-server.mjs

# Stage freshly-built bundles into the published `emacs-gravity-marketplace`
# install dir. For contributors who run the published plugin and want to
# iterate on server/bridge code without uninstalling and rewiring a local
# marketplace. Staged files are overwritten on the next `/plugin update` —
# that's fine, just re-run after an auto-update.
sync-marketplace: build-bridge build-server
	@if [ -z "$(MARKETPLACE_CACHE)" ]; then \
		echo "error: marketplace install not found under"; \
		echo "       ~/.claude/plugins/cache/emacs-gravity-marketplace/emacs-bridge/"; \
		echo "       install first via: /plugin install emacs-bridge@emacs-gravity-marketplace"; \
		exit 1; \
	fi
	@echo "Staging bundles into $(MARKETPLACE_CACHE)"
	@mkdir -p \
		"$(MARKETPLACE_CACHE)dist" \
		"$(MARKETPLACE_CACHE)packages/emacs-bridge/dist" \
		"$(MARKETPLACE_CACHE)packages/gravity-server/dist"
	install -m 644 packages/emacs-bridge/dist/emacs-bridge.mjs "$(MARKETPLACE_CACHE)packages/emacs-bridge/dist/emacs-bridge.mjs"
	@# Server: stage to all three known layouts so this works for any released version.
	@#   v4.0.1 _ensure-server fallback #2 → packages/gravity-server/dist/
	@#   v4.0.2+ dev layout                → packages/emacs-bridge/dist/
	@#   v4.0.5+ published-release layout  → dist/  (hooks/_ensure-server resolves
	@#                                       $(dirname $0)/../dist/gravity-server.mjs)
	install -m 644 packages/gravity-server/dist/gravity-server.mjs "$(MARKETPLACE_CACHE)dist/gravity-server.mjs"
	install -m 644 packages/gravity-server/dist/gravity-server.mjs "$(MARKETPLACE_CACHE)packages/gravity-server/dist/gravity-server.mjs"
	install -m 644 packages/gravity-server/dist/gravity-server.mjs "$(MARKETPLACE_CACHE)packages/emacs-bridge/dist/gravity-server.mjs"
	@echo "Marketplace cache synced."

menubar:
	cd packages/gravity-menubar && swift build
	-pkill -f GravityMenuBar
	sleep 0.5
	packages/gravity-menubar/.build/arm64-apple-macosx/debug/GravityMenuBar &

kill-server:
	@# Kill via PID file (process group + direct)
	@if [ -f "$(HOME)/.local/state/gravity-server.pid" ]; then \
		pid=$$(cat "$(HOME)/.local/state/gravity-server.pid" 2>/dev/null); \
		if [ -n "$$pid" ]; then \
			kill -TERM "-$$pid" 2>/dev/null || true; \
			kill -TERM "$$pid" 2>/dev/null || true; \
		fi; \
		rm -f "$(HOME)/.local/state/gravity-server.pid"; \
	fi
	@# Catch orphans (tsx wrappers + node children)
	-@pkill -f "gravity-server\\.(ts|mjs)" 2>/dev/null || true
	@rm -f "$(HOME)/.local/state/gravity-hooks.sock" "$(HOME)/.local/state/gravity-terminal.sock"
	@sleep 0.3

restart-server: sync-marketplace kill-server
	@# Eagerly respawn the freshly-staged bundle so long-lived clients
	@# (menubar, Emacs) reconnect immediately without waiting for a
	@# Claude Code hook to fire. kill-server already cleaned up orphans
	@# and sockets, so we don't need _ensure-server's lock/cleanup logic.
	@# (Sourcing _ensure-server directly doesn't work here anyway: under
	@# `sh -c '. file'` $0 is `sh`, which breaks its $(dirname $0)/... path
	@# resolution. Direct spawn from MARKETPLACE_CACHE is cleaner.)
	@# Respawn from the same path _ensure-server uses (top-level dist/), so
	@# any subsequent hook-driven auto-spawn matches the manual restart.
	@echo "Respawning server from $(MARKETPLACE_CACHE)..."
	@nohup node "$(MARKETPLACE_CACHE)dist/gravity-server.mjs" >>/tmp/gravity-server.log 2>&1 &
	@for i in 1 2 3 4 5 6 7 8 9 10; do \
		if [ -S "$(HOME)/.local/state/gravity-hooks.sock" ]; then \
			echo "Server up."; exit 0; \
		fi; \
		sleep 0.2; \
	done; \
	echo "error: server did not come up within 2s — check /tmp/gravity-server.log"; exit 1

test-install:
	docker build -t gravity-smoke -f test/Dockerfile .
	docker run --rm \
		-v $(PWD):/workspace \
		-v /workspace/node_modules \
		-v /workspace/packages/emacs-bridge/node_modules \
		-v /workspace/packages/gravity-server/node_modules \
		-v /workspace/packages/shared/node_modules \
		gravity-smoke bash test/test-install.sh

test-install-shell:
	docker build -t gravity-smoke -f test/Dockerfile .
	docker run --rm -it \
		-v $(PWD):/workspace \
		-v /workspace/node_modules \
		-v /workspace/packages/emacs-bridge/node_modules \
		-v /workspace/packages/gravity-server/node_modules \
		-v /workspace/packages/shared/node_modules \
		gravity-smoke bash

check-settings:
	node scripts/check-project-settings.mjs

clean:
	rm -rf node_modules packages/*/node_modules packages/*/dist
