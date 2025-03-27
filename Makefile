# -*- Makefile -*-
SHELL = /bin/sh
EMACS ?= emacs

.PHONY: test solve-dependencies

EMACS_GENERIC_OPTS=--quick --directory . --directory .deps
EMACS_BATCH_OPTS:=--batch $(EMACS_GENERIC_OPTS)
RM=@rm -rf

solve-dependencies:
	@echo "Installing dependencies..."
	@mkdir -p ~/.emacs.d/lisp
	@if [ ! -d ~/.emacs.d/lisp/websocket-bridge ]; then \
		git clone https://github.com/ginqi7/websocket-bridge ~/.emacs.d/lisp/websocket-bridge; \
	else \
			echo "websocket-bridge already exists, skipping..."; \
	fi
	@if [ ! -d ~/.emacs.d/lisp/emacs-websocket ]; then \
		git clone https://github.com/ahyatt/emacs-websocket ~/.emacs.d/lisp/emacs-websocket; \
	else \
		echo "emacs-websocket already exists, skipping..."; \
	fi
	@echo "Dependencies installed installed successfully."

test:   solve-dependencies
	@$(EMACS) $(EMACS_BATCH_OPTS) --load ./tests/org-reminders-tests.el
