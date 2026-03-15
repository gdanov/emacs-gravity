EMACS ?= emacs

.PHONY: test test-elisp test-bridge test-server build clean

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

clean:
	rm -rf node_modules packages/*/node_modules packages/*/dist
