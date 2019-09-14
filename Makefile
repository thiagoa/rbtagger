emacs ?= emacs -Q --batch

all: compile checkdoc package-lint test

compile: FORCE
	$(emacs) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile rbtagger.el

checkdoc: FORCE
	$(emacs) -l test/batch-mode/checkdoc.el

package-lint: FORCE
	$(emacs) -l test/batch-mode/package-lint.el -f package-lint-batch-and-exit rbtagger.el

test: FORCE
	cd test && $(emacs) -l rbtagger-test.el -f ert-run-tests-batch-and-exit

FORCE: ;
