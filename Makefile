# -*- Makefile -*-
SHELL = /bin/sh
EMACS ?= emacs

.PHONY: test solve-dependencies

EMACS_GENERIC_OPTS=--quick --directory . --directory .deps
EMACS_BATCH_OPTS:=--batch $(EMACS_GENERIC_OPTS)
RM=@rm -rf

solve-dependencies:
	@sh ./dependencies.sh
test:   solve-dependencies
	@$(EMACS) $(EMACS_BATCH_OPTS) --load ./tests/org-reminders-tests.el
