EMACS := emacs

all:
	$(EMACS) --batch --quick \
		--directory . \
		--load ob-base64-tests.el \
		--funcall ert-run-tests-batch

.PHONY: tag
tag:
	$(MAKE) all
	git add -f . && git stash
	@grep ";; Version:" ob-base64.el | tee /dev/stderr | grep "$(TAG)"
	@git tag "$(TAG)" --sign
