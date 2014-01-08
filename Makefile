PACKAGE = esqlite

ARCHIVE_DIR_PREFIX = ..

GOMI	= *.elc *~

RELEASE_FILES = \
	esqlite-helm.el esqlite.el

# Check with default settings
check: compile
	emacs -q -batch \
		-l ./test/env.el \
		-l Emacs-pcsv/pcsv.el -l esqlite.el -l esqlite-helm.el -l esqlite-test.el \
		-eval "(ert-run-tests-batch-and-exit '(tag esqlite))"

check-developer :
	./test/all-test.sh

compile:
	emacs -q -batch -L . -L Emacs-pcsv -eval \
	  "(progn (byte-compile-file \"esqlite.el\") (byte-compile-file \"esqlite-helm.el\"))";

clean:
	rm -rf $(GOMI)



