EMACS ?= emacs
SOURCES=diff-hl.el
SOURCES+=diff-hl-amend.el
SOURCES+=diff-hl-dired.el
SOURCES+=diff-hl-flydiff.el
SOURCES+=diff-hl-inline-popup.el
SOURCES+=diff-hl-margin.el
SOURCES+=diff-hl-show-hunk-posframe.el
SOURCES+=diff-hl-show-hunk.el

ARTIFACTS=$(patsubst %.el, %.elc, $(SOURCES))

RM ?= rm -f

# Lisp expression to initialize package for using other packages, like "async".
PACKAGE_INIT="(progn (require 'package) (package-initialize))"

all: compile test

test:
	$(EMACS) -batch -L . --eval ${PACKAGE_INIT} -l test/diff-hl-test.el -f diff-hl-run-tests

compile:
	$(EMACS) -batch -L . --eval ${PACKAGE_INIT} -f batch-byte-compile $(SOURCES)

clean:
	$(RM) $(ARTIFACTS)

.PHONY: all test compile plain clean
