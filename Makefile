EMACS := emacs

.PHONY: all
all: lint-makefile test

.PHONY: clean
clean:
	@-rm ob-base64*.elc 2>/dev/null
	@-rm ob-base64*.ok 2>/dev/null
	@-rm Makefile.ok 2>/dev/null

%.elc: %.el
	@-rm "$@" 2>/dev/null
	@$(EMACS) --batch --quick \
		--directory . \
		--load compile-setup \
		--eval '(byte-compile-file "$(subst .elc,.el,$@)")' \
	&& test -f "$@"

.PHONY: byte-compile
byte-compile: \
	ob-base64-tests.elc \
	ob-base64.elc

.PHONY: test
test: byte-compile main-tests

ob-base64-tests.ok: ob-base64.elc ob-base64-tests.elc
	@$(EMACS) --batch --quick \
		--directory . \
		--load ob-base64-tests.el \
		--funcall ert-run-tests-batch-and-exit \
	&& touch ob-base64-tests.ok
main-tests: ob-base64-tests.ok

Makefile.ok: Makefile
	@make -n all
	@docker run \
		--network=none \
		--volume "$(PWD)"/Makefile:/Makefile \
		backplane/checkmake /Makefile \
	&& touch Makefile.ok
lint-makefile: Makefile.ok

.PHONY: tag
tag:
	$(MAKE) all
	git add -f . && git stash
	@grep ";; Version:" ob-base64.el | tee /dev/stderr | grep "$(TAG)"
	@git tag "$(TAG)" --sign
