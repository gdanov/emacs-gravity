EMACS ?= emacs

.PHONY: test test-elisp test-bridge test-server build build-server sync-cache clean

PLUGIN_CACHE := $(HOME)/.claude/plugins/cache/local-emacs-marketplace/emacs-bridge/2.0.0

test: test-elisp test-bridge test-server

test-elisp:
	$(EMACS) -nw --batch -L . -L test \
		-l claude-gravity \
		-l cg-test-replay \
		-l claude-gravity-test \
		-l claude-gravity-contract-test \
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
	cp packages/gravity-server/dist/server.mjs $(PLUGIN_CACHE)/dist/
	@echo "Cache synced."

clean:
	rm -rf node_modules packages/*/node_modules packages/*/dist
