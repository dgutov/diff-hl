EMACS ?= emacs
SOURCES=
SOURCES+=diff-hl-amend.el
SOURCES+=diff-hl-dired.el
SOURCES+=diff-hl-flydiff.el
SOURCES+=diff-hl-margin.el

ARTIFACTS=$(patsubst %.el, %.elc, $(SOURCES))

RM ?= rm -f

all: test

test:
	$(EMACS) -batch -L . -l test/diff-hl-test.el -f diff-hl-run-tests

compile:
	$(EMACS) -batch -f batch-byte-compile $(SOURCES)

clean:
	$(RM) $(ARTIFACTS)

.PHONY: all test compile plain clean
