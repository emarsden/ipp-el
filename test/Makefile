EMACS ?= emacs

test: test-ipp.el
	${EMACS} -Q --batch -l ../ipp.el -l test-ipp.el -f ipp-check-fixtures
