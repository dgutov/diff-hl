EMACS ?= emacs
SOURCES=diff-hl.el
SOURCES+=diff-hl-amend.el
SOURCES+=diff-hl-dired.el
SOURCES+=diff-hl-flydiff.el
SOURCES+=diff-hl-show-hunk-inline.el
SOURCES+=diff-hl-margin.el
SOURCES+=diff-hl-show-hunk-posframe.el
SOURCES+=diff-hl-show-hunk.el

ARTIFACTS=$(patsubst %.el, %.elc, $(SOURCES))

RM ?= rm -f

all: compile test

test:
	$(EMACS) -batch -L . -l test/diff-hl-test.el -l test/diff-hl-adjust-test.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -batch -L . -f batch-byte-compile $(SOURCES)

compile-warn:
	${EMACS} -Q --batch -L . \
	--eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile diff-hl*.el test/*.el

clean:
	$(RM) $(ARTIFACTS)

.PHONY: all test compile plain clean
