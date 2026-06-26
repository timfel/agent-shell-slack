EMACS ?= emacs
AGENT_SHELL_DIR ?= ../agent-shell-20260612.1526
EMACS_BATCH = TMPDIR=/tmp $(EMACS) -Q --batch --eval "(setq load-prefer-newer t native-comp-jit-compilation nil)" --eval "(package-initialize)"

.PHONY: test compile

test:
	$(EMACS_BATCH) -L . -L $(AGENT_SHELL_DIR) -l tests/agent-shell-slack-tests.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS_BATCH) -L . -L $(AGENT_SHELL_DIR) -f batch-byte-compile agent-shell-slack.el
