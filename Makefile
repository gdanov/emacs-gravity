EMACS ?= emacs

.PHONY: test test-elisp test-bridge test-server build build-server sync-cache clean menubar kill-server restart-server

PLUGIN_CACHE := $(HOME)/.claude/plugins/cache/local-emacs-marketplace/emacs-bridge/2.0.0

test: test-elisp test-bridge test-server

test-elisp:
	$(EMACS) -nw --batch -L . -L test \
		-l claude-gravity \
		-l cg-test-replay \
		-l claude-gravity-test \
		-l claude-gravity-contract-test \
		-l claude-gravity-patch-test \
		-f ert-run-tests-batch-and-exit

test-bridge:
	cd packages/emacs-bridge && npx vitest run

test-server:
	cd packages/gravity-server && npx vitest run

build:
	npm install

build-server:
	cd packages/gravity-server && node build.mjs

sync-cache: build-server
	@echo "Syncing to plugin cache..."
	rsync -a --delete packages/emacs-bridge/src/ $(PLUGIN_CACHE)/src/
	rsync -a packages/emacs-bridge/hooks/ $(PLUGIN_CACHE)/hooks/
	cp packages/emacs-bridge/package.json $(PLUGIN_CACHE)/package.json
	cp packages/gravity-server/dist/gravity-server.mjs $(PLUGIN_CACHE)/dist/
	@echo "Cache synced."

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

restart-server: sync-cache kill-server
	npx -w packages/gravity-server tsx src/gravity-server.ts &

clean:
	rm -rf node_modules packages/*/node_modules packages/*/dist
