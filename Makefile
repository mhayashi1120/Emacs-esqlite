EMACS = emacs

PACKAGE = esqlite

ARCHIVE_DIR_PREFIX = ..

GOMI	= *.elc *~

RELEASE_FILES = \
	esqlite-helm.el esqlite.el

# Check with default settings
check: compile
	$(EMACS) -q -batch \
		-l ./test/env.el \
		-L ./Emacs-pcsv -l esqlite.el -l esqlite-helm.el -l esqlite-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch \
		-l ./test/env.el \
		-L ./Emacs-pcsv -l esqlite.elc -l esqlite-helm.elc -l esqlite-test.el \
		-f ert-run-tests-batch-and-exit

check-developer :
	./test/all-test.sh

compile:
	$(EMACS) -q -batch -L . -L Emacs-pcsv \
	     -f batch-byte-compile $(RELEASE_FILES)

clean:
	rm -rf $(GOMI)



