# -*- Makefile -*-
SHELL = /bin/sh
EMACS ?= emacs

.PHONY: test

EMACS_GENERIC_OPTS=--quick --directory . --directory .deps
EMACS_BATCH_OPTS:=--batch $(EMACS_GENERIC_OPTS)
RM=@rm -rf

test:
	@$(EMACS) $(EMACS_BATCH_OPTS) --load ./tests/org-reminders-tests.el
