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

all: compile test

test:
	$(EMACS) -batch -L . -l test/diff-hl-test.el -f diff-hl-run-tests

compile:
	$(EMACS) -batch -L . -f batch-byte-compile $(SOURCES)

clean:
	$(RM) $(ARTIFACTS)

.PHONY: all test compile plain clean
