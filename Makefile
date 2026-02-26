EMACS ?= emacs

.PHONY: test test-elisp test-bridge build clean

test: test-elisp test-bridge

test-elisp:
	$(EMACS) -nw --batch -L . -L test \
		-l claude-gravity \
		-l cg-test-replay \
		-l claude-gravity-test \
		-l claude-gravity-contract-test \
		-f ert-run-tests-batch-and-exit

test-bridge:
	cd emacs-bridge && npm ci --ignore-scripts && npx vitest run

build:
	cd emacs-bridge && npm ci

clean:
	rm -rf emacs-bridge/node_modules emacs-bridge/dist
