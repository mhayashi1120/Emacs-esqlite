PACKAGE = esqlite

ARCHIVE_DIR_PREFIX = ..

GOMI	= *.elc *~

RELEASE_FILES = \
	esqlite-helm.el esqlite-mode.el	esqlite.el

check:
	emacs -q -batch -L . -L Emacs-pcsv -eval "(progn (byte-compile-file \"esqlite.el\") (byte-compile-file \"esqlite-helm.el\") (byte-compile-file \"esqlite-mode.el\"))"; \
	emacs -q -batch -l Emacs-pcsv/pcsv.el -l esqlite.el -l esqlite-helm.el -l esqlite-test.el \
		-eval "(ert-run-tests-batch-and-exit '(tag esqlite))"

clean:
	rm -rf $(GOMI)



