EMACS := emacs

all:
	$(EMACS) --batch --quick \
		--directory . \
		--load ob-base64-tests.el \
		--funcall ert-run-tests-batch
