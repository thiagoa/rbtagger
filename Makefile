emacs ?= emacs -Q --batch

all: compile checkdoc test

compile: FORCE
	$(emacs) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile rbtagger.el

test: FORCE
	cd test && $(emacs) -l rbtagger-test.el -f ert-run-tests-batch-and-exit

checkdoc: FORCE
	$(emacs) -l test/checkdoc.el

FORCE: ;
