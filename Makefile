EMACS = emacs

PACKAGE = esqlite

ARCHIVE_DIR_PREFIX = ..

GOMI	= *.elc *~

RELEASE_FILES = \
	esqlite.el esqlite-helm.el

ifeq ($(shell uname -s), Darwin)
	BATCH_COMMAND = -eval "(ert-run-tests-batch-and-exit '(and (tag esqlite) (not (tag esqlite-stream))))"
else
	BATCH_COMMAND = -f ert-run-tests-batch-and-exit
endif

# Check with default settings
check: compile
	$(EMACS) -q -batch \
		-l ./test/env.el \
		-L ./Emacs-pcsv -l esqlite.el -l esqlite-helm.el -l esqlite-test.el \
		$(BATCH_COMMAND)
	$(EMACS) -q -batch \
		-l ./test/env.el \
		-L ./Emacs-pcsv -l esqlite.elc -l esqlite-helm.elc -l esqlite-test.el \
		$(BATCH_COMMAND)

check-developer :
	./test/all-test.sh

compile:
	$(EMACS) -q -batch -L . -L Emacs-pcsv \
	     -f batch-byte-compile $(RELEASE_FILES)

clean:
	rm -rf $(GOMI)



