;;; esqlite.el --- sqlite file manipulate utilities

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: data
;; URL: https://github.com/mhayashi1120/Emacs-esqlite/raw/master/esqlite.el
;; Emacs: GNU Emacs 24 or later
;; Package-Requires: ((pcsv "1.3.3"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; esqlite.el is a implementation to handle sqlite database.
;;  (version 3 or later)
;; Following functions are provided:
;; * Read sqlite row as list of string.
;; * Async read sqlite row as list of string.
;; * sqlite process with being stationed
;; * Construct sqlite SQL.
;; * Escape SQL value to construct SQL
;; * Some of basic utilities.
;; * NULL handling (denote as :null keyword)

;;; Install:

;; Please install sqlite command.
;; Please install from ELPA. (TODO url)

;;; TODO:
;; * blob

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'pcsv)

(defgroup esqlite ()
  "Manipulate esqlite database."
  :prefix "esqlite-"
  :group 'applications)

(defcustom esqlite-sqlite-program "sqlite3"
  "Command name or path to command.
Default is the head of sqlite."
  :group 'esqlite
  :type '(choice file string))

;;;
;;; Basic utilities
;;;

(defun esqlite--replace (string init-table)
  (loop with table = init-table
        with res
        with prev
        for c across string
        concat (let ((pair (assq c table)))
                 (cond
                  ((not pair)
                   (setq table init-table)
                   (prog1
                       (concat (nreverse prev) (char-to-string c))
                     (setq prev nil)))
                  ((stringp (cdr pair))
                   (setq table init-table)
                   (setq prev nil)
                   (cdr pair))
                  ((consp (cdr pair))
                   (setq prev (cons c prev))
                   (setq table (cdr pair))
                   nil)
                  (t
                   (error "esqlite: Not supported yet"))))))

(defun esqlite--filter (filter list)
  (loop for o in list
        if (funcall filter o)
        collect o))

;;;
;;; System deps
;;;

(defcustom esqlite-mswin-fakecygpty-program nil
  "esqlite on cygwin cannot work since no pty.
Please download and install fakecygpty (Google it!!)"
  :group 'esqlite
  :type '(choice file string))

(defun esqlite-mswin-native-p ()
  (and (memq system-type '(windows-nt))
       (not (esqlite-mswin-cygwin-p))))

(defun esqlite-mswin-cygwin-p ()
  (and (memq system-type '(windows-nt))
       (executable-find "cygpath")
       (let ((program (executable-find esqlite-sqlite-program)))
         (and program
              ;; check program is under the cygwin installed directory.
              (let* ((rootdir
                      (let* ((line (esqlite-mswin-cygpath "/" "windows"))
                             (root (expand-file-name line)))
                        (file-name-as-directory root)))
                     (regexp (concat "\\`" (regexp-quote rootdir))))
                (string-match regexp program))))
       t))

(defun esqlite-mswin-cygpath (file type)
  (with-temp-buffer
    (let ((code (call-process "cygpath" nil t nil "-t" type file)))
      (unless (= code 0)
        (error "esqlite: cygpath failed with %d" code))
      (goto-char (point-min))
      (buffer-substring
       (point-min) (point-at-eol)))))

;;;
;;; Basic utilities to handle esqlite i/o.
;;;

;;
;; Generate temporary name
;;

(defun esqlite--temp-null (query)
  ;; esqlite command nullvalue assigned to 20 chars (19 + null).
  (esqlite--random-text query 19))

;; * random char restrict to `a-zA-Z0-9+/' (except last `=')
;; * md5 have 16 bytes
;; * base64 encode 16 bytes -> 44 chars
;; * to trim last `=', only use md5 first 15 bytes make (* 4 (/ 15 3)) 20 bytes
(defun esqlite--random-text (seed length)
  (loop for i from 0 to (/ (1- length) 20)
        concat (md5 (format "%d:%s:%s" i (current-time) seed))
        into hash
        finally return
         ;; only first 15 bytes
        (loop for i from 0 below (- (length hash) 2) by 2
              ;; make random unibyte string
              collect (let ((hex (substring hash i (+ i 2))))
                        (string-to-number hex 16))
              into res
              finally return
              ;; md5 hex fold to base64 area
              (let* ((unibytes (apply 'unibyte-string res))
                     (b64 (base64-encode-string unibytes t)))
                (substring b64 0 length)))))

(defun esqlite--unique-name (stream prefix &optional seed)
  (loop with objects = (esqlite-read--objects stream)
        with full-name
        while (let ((random-text (esqlite--random-text (or seed "") 40)))
                (setq full-name (format "%s_%s" prefix random-text))
                (member full-name objects))
        finally return full-name))

(defun esqlite-terminate-command (command)
  (cond
   ((not (string-match "\n\\'" command))
    (concat command "\n"))
   (t
    command)))

(defun esqlite-terminate-statement (sql)
  (cond
   ((not (string-match ";[ \t\n]*\\'" sql))
    (concat sql ";\n"))
   ((not (string-match "\n\\'" sql))
    (concat sql "\n"))
   (t sql)))

;;
;; constant / system dependent
;;

(defconst esqlite--rowid-columns
  '("_ROWID_" "ROWID" "OID"))

;;;###autoload
(defconst esqlite-file-header-regexp "\\`SQLite format 3\000")

;;;###autoload
(defun esqlite-file-guessed-database-p (file)
  "Guess the FILE is a esqlite database or not."
  (and (file-regular-p file)
       (with-temp-buffer
         (insert-file-contents file nil 0 256)
         (looking-at esqlite-file-header-regexp))))

;;;###autoload
(defun esqlite-sqlite-installed-p ()
  "Return non-nil if `esqlite-sqlite-program' is installed."
  (and (stringp esqlite-sqlite-program)
       (executable-find esqlite-sqlite-program)))

(defun esqlite-check-sqlite-program ()
  (unless (stringp esqlite-sqlite-program)
    (error "esqlite: No valid esqlite program"))
  (unless (executable-find esqlite-sqlite-program)
    (error "esqlite: %s not found" esqlite-sqlite-program)))

;;
;; process
;;

(defvar esqlite-process-coding-system
  (let* ((syseol (let ((type (coding-system-eol-type
                              ;; guess esqlite command environment
                              default-terminal-coding-system)))
                   (cond
                    ((or (symbolp type)
                         (numberp type))
                     type)
                    ((vectorp type)
                     (coding-system-eol-type (aref type 0))))))
         ;; utf-8 as a default coding-system of esqlite database.
         (basecs 'utf-8)
         (cs
          (coding-system-change-eol-conversion basecs syseol)))
    (cons cs cs))
  "Temporarily change coding system by this hiding parameter.")

(defconst esqlite-prompt "sqlite> ")
(defconst esqlite-continue-prompt "")

(defun esqlite-prompt-p ()
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    ;; when executed sql contains newline, continue prompt displayed
    ;; before last prompt "sqlite> "
    (esqlite-looking-at-prompt)))

(defconst esqlite-prompt-regexp
  (concat "^"
          "\\( *"
          (regexp-quote esqlite-continue-prompt)
          "\\)*"
          (regexp-quote esqlite-prompt)
          "\\'"))

(defvar esqlite--default-init-file nil)

;; To avoid user initialization file ~/.sqliterc
(defun esqlite--default-init-file (&optional refresh)
  (or (and (not refresh)
           (stringp esqlite--default-init-file)
           (file-exists-p esqlite--default-init-file)
           esqlite--default-init-file)
      (setq esqlite--default-init-file
            (let ((file (make-temp-file "emacs-esqlite-")))
              (with-temp-buffer
                (insert (format ".prompt \"%s\" \"%s\"\n"
                                esqlite-prompt
                                esqlite-continue-prompt))
                (write-region (point-min) (point-max) file nil 'no-msg))
              file))))

(defmacro esqlite--with-env (&rest form)
  `(let ((process-environment (copy-sequence process-environment))
         ;; non pty resulting in echoing query.
         (process-connection-type t)
         (default-process-coding-system
           (cond
            ((consp esqlite-process-coding-system)
             esqlite-process-coding-system)
            ((coding-system-p esqlite-process-coding-system)
             (cons esqlite-process-coding-system
                   esqlite-process-coding-system))
            (t nil))))
     ;; currently no meanings of this
     ;; in the future release may support i18n.
     (setenv "LANG" "C")
     ,@form))

(defmacro esqlite--with-process (proc &rest form)
  (declare (indent 1) (debug t))
  `(let ((buf (process-buffer proc)))
     (when (buffer-live-p buf)
       (with-current-buffer buf
         ,@form))))

(defmacro esqlite--with-parse (proc event &rest form)
  "Execute FORM in PROC buffer with appended EVENT.
This form check syntax error report from esqlite command."
  (declare (indent 2) (debug t))
  `(esqlite--with-process proc
     (save-excursion
       (goto-char (point-max))
       (insert event)
       (goto-char (point-min))
       (unless (process-get proc 'esqlite-syntax-error)
         ;; check only first time filter received data.

         ;;FIXME this error output to stderr. Separate stderr to other file??
         ;;     Altough rarely happens, worrying about confusing stdout/stderr
         (let ((errmsg (esqlite--read-syntax-error-at-point)))
           (process-put proc 'esqlite-syntax-error (or errmsg t))))
       ,@form)))

(defun esqlite-start-process (buffer &rest args)
  (esqlite--with-env
   (let ((cmdline (cons esqlite-sqlite-program args))
         (cygwinp (esqlite-mswin-cygwin-p)))
     (when cygwinp
       (unless esqlite-mswin-fakecygpty-program
         (error "%s %s"
                "esqlite on cygwin cannot work unless `fakecygpty'"
                "see `esqlite-mswin-fakecygpty-program' settings"))
       (setq cmdline
             (cons esqlite-mswin-fakecygpty-program cmdline)))
     (apply 'start-process "Esqlite" buffer cmdline))))

(defun esqlite-call-process-region (buffer start end &rest args)
  (esqlite--with-env
   (apply 'call-process-region
          start end esqlite-sqlite-program nil buffer nil args)))

(defun esqlite-expand-db-name (file)
  (if (esqlite-mswin-cygwin-p)
      ;; esqlite on cygwin cannot accept c:/hoge like path.
      ;; must convert /cygdrive/c/hoge
      ;; But seems accept such path in -init option.
      (esqlite-mswin-cygpath file "unix")
    (expand-file-name file)))

(defun esqlite--create-process-buffer ()
  (generate-new-buffer " *esqlite work* "))

(defun esqlite-start-csv-process (file &optional query nullvalue &rest args)
  "Start async esqlite process.

Cygwin: FILE contains multibyte char, may fail to open FILE as database."
  (let* ((init (esqlite--default-init-file))
         (db (esqlite-expand-db-name file))
         (filename (expand-file-name file))
         (null (or nullvalue (esqlite--temp-null query)))
         (args `(
                 ,@(if query `("-batch") '("-interactive"))
                 "-init" ,init
                 "-csv"
                 "-nullvalue" ,null
                 ;; prior than preceeding args
                 ,@args
                 ,db))
         (buf (esqlite--create-process-buffer))
         (proc (apply 'esqlite-start-process buf args)))
    (process-put proc 'esqlite-init-file init)
    (process-put proc 'esqlite-filename filename)
    (process-put proc 'esqlite-null-value null)
    (when query
      (process-send-string proc query)
      ;; imitate calling command
      ;; e.g. esqlite db.esqlite "select some,of,query from table"
      (process-send-string proc ";\n.exit\n"))
    proc))

(defun esqlite-call-csv-process (file query &optional nullvalue &rest args)
  "Call esqlite process.

Cygwin: FILE contains multibyte char, may fail to open FILE as database."
  (let* ((init (esqlite--default-init-file))
         (db (esqlite-expand-db-name file))
         (null (or nullvalue (esqlite--temp-null query)))
         (args `(
                 "-batch"
                 "-init" ,init
                 "-csv"
                 "-nullvalue" ,null
                 ;; prior than preceeding args
                 ,@args
                 ,db)))
    ;; Do not use `call-process' this case.  On windows if argument is
    ;; a multibyte string, command argument may be destroyed.
    ;; Although `call-process-region' may have same problem, but
    ;; decrease the risk of this.
    ;; For example, database file have a multibyte name.
    (apply
     'esqlite-call-process-region
     (current-buffer)
     ;; `call-process-region' start argument accept string
     (concat query ";\n") nil
     args)))

(defun esqlite-looking-at-prompt ()
  (looking-at esqlite-prompt-regexp))

(defun esqlite--read-syntax-error-at-point ()
  (and (looking-at "^Error: \\(.*\\)")
       (format "esqlite: %s" (match-string 1))))

(defun esqlite--maybe-raise-syntax-error (proc)
  (while (and (eq (process-status proc) 'run)
              (not (esqlite-prompt-p))
              (null (process-get proc 'esqlite-syntax-error)))
    (esqlite-sleep proc))
  (when (stringp (process-get proc 'esqlite-syntax-error))
    (error "%s" (process-get proc 'esqlite-syntax-error))))

;;
;; read csv from esqlite
;;

(defun esqlite--read-csv-line (&optional null)
  (let ((first (point))
        (line (pcsv-read-line)))
    ;; end of line is not a newline.
    ;; means ouput is progressing, otherwise prompt.
    (unless (bolp)
      (goto-char first)
      (signal 'invalid-read-syntax nil))
    (loop for datum in line
          collect (if (equal datum null)
                      :null
                    datum))))

(defun esqlite--read-csv-line-with-deletion (null)
  (let ((start (point))
        (row (esqlite--read-csv-line null)))
    (delete-region start (point))
    row))

(defun esqlite--read-csv-with-deletion (null)
  "Read csv data from current point.
Delete csv data if reading was succeeded."
  (let (res)
    (condition-case nil
        (while (not (eobp))
          (let ((start (point))
                (row (esqlite--read-csv-line-with-deletion null)))
            (setq res (cons row res))))
      ;; output is proceeding from process
      ;; finish the reading
      (invalid-read-syntax nil))
    (nreverse res)))

;;
;; esqlite data (csv) <-> emacs data
;;

(defun esqlite-format-object (object)
  (concat "\""
          (replace-regexp-in-string "\"" "\"\"" object)
          "\""))

(defun esqlite-format-value (object)
  (cond
   ((stringp object)
    (concat
     "'"
     (replace-regexp-in-string "'" "''" object)
     "'"))
   ((numberp object)
    (prin1-to-string object))
   ((eq object :null) "null")
   ((listp object)
    (mapconcat 'esqlite-format-value object ", "))
   (t
    (error "esqlite: Not a supported type %s" object))))

(eval-and-compile
  (defvar esqlite-format--table
    (eval-when-compile
      '(
        ("s" . (lambda (x)
                 (cond
                  ((stringp x) x)
                  ((numberp x) (number-to-string x))
                  (t (prin1-to-string x)))))
        ("t" . esqlite-escape-string)
        ("T" . esqlite-text)
        ("l" . esqlite-escape-like)
        ("L" . (lambda (x)
                 (concat
                  (esqlite-text (esqlite-escape-like x))
                  " ESCAPE '\\' ")))
        ;; some database object
        ("o" . esqlite-format-object)
        ;; column list
        ("O" . (lambda (l)
                 (mapconcat (lambda (x) (esqlite-format-object x))
                            l ", ")))
        ;; some value list (string, number, list) with properly quoted
        ("V" . esqlite-format-value)
        ))))

;;;###autoload
(defun esqlite-format (esqlite-fmt &rest esqlite-objects)
  "Prepare sql with ESQLITE-FMT like `format'.

ESQLITE-FMT is a string or list of string.
 each list item join with newline.

Each directive accept arg which contains variable name.
  This variable name must not contain `esqlite-' prefix.
e.g.
\(let ((some-var \"FOO\")) (esqlite-format \"%s %s{some-var}\" \"HOGE\"))
 => \"HOGE FOO\"

%s: raw text (With no escape)
%t: escape text
%T: same as %t but with quote
%l: escape LIKE pattern escape char is '\\'
%L: similar to %l but with quoting/escaping and ESCAPE statement
%o: escape db object with quote
%O: escape db objects with quote (joined by \", \")
%V: escape sql value(s) with quote if need
  (if list, joined by \", \" without paren)

\(fn fmt &rest objects)"
  (with-temp-buffer
    (when (listp esqlite-fmt)
      (setq esqlite-fmt (mapconcat 'identity esqlite-fmt "\n")))
    (insert esqlite-fmt)
    (goto-char (point-min))
    (let ((esqlite-fmt-regexp
           (eval-when-compile
             (concat
              (regexp-opt (mapcar 'car esqlite-format--table) t)
              ;; Optional varname
              "\\(?:{\\(.+?\\)}\\)?")))
          (case-fold-search nil))
      (while (search-forward "%" nil t)
        (cond
         ((eq (char-after) ?%)
          (delete-char 1))
         ((looking-at esqlite-fmt-regexp)
          (let* ((spec (match-string 1))
                 (varname (match-string 2))
                 (varsym (intern-soft varname))
                 (fn (assoc-default spec esqlite-format--table))
                 obj)
            (when (and varname (string-match "\\`esqlite-" varname))
              (error "esqlite: Unable use esqlite- prefix variable %s"
                     varname))
            ;; Delete formatter directive
            (delete-region (1- (point)) (match-end 0))
            (cond
             (varsym
              ;; raise error asis if varname is not defined.
              (setq obj (symbol-value varsym)))
             (esqlite-objects
              (setq obj (car esqlite-objects))
              (setq esqlite-objects (cdr esqlite-objects)))
             (t
              (error "esqlite: No value assigned to `%%%s'" spec)))
            (unless fn
              (error "esqlite: Invalid format character: `%%%s'" spec))
            (unless (functionp fn)
              (error "esqlite: Assert"))
            (let ((val obj)
                  text)
              (setq text (funcall fn val))
              (insert text)))))))
    (when esqlite-objects
      (error "esqlite: args out of range %s" esqlite-objects))
    (buffer-string)))

;; http://www.sqlite.org/syntaxdiagrams.html#numeric-literal
(defconst esqlite-numeric-literal-regexp
  (eval-when-compile
    (concat
     "\\`"
     "[+-]?"
     "\\([0-9]+\\(\\.\\([0-9]*\\)\\)?\\|\\.[0-9]+\\)"
     "\\(E[+-]?[0-9]+\\)?"
     "\\'")))

(defun esqlite-numeric-text-p (text)
  "Utility function to check TEXT is numeric"
  (and (string-match esqlite-numeric-literal-regexp text)
       t))

;;
;; sleep in process filter
;;

(defvar esqlite-sleep-second
  ;;FIXME
  ;; This check calculate response of external command which does not accept
  ;; from process output.
  (apply 'min (loop for _ in '(1 2 3 4 5)
                    collect
                    (let ((start (float-time)))
                      (call-process "echo" nil nil nil "1")
                      (let ((end (float-time)))
                        (- end start))))))

(defun esqlite-sleep (_dummy)
  ;; Other code affect to buffer while `sleep-for'.
  ;; TODO why need save-excursion? timer? filter? I can't get clue.
  (save-excursion
    ;; Achieve like a asynchronous behavior (ex: draw mode line)
    (redisplay)
    (sleep-for esqlite-sleep-second)))

;;
;; Escape text
;;

(defun esqlite-escape--like-table (escape-char &optional override)
  (let ((escape (or escape-char ?\\)))
    (append
     override
     `((?\% . ,(format "%c%%" escape))
       (?\_ . ,(format "%c_"  escape))
       (,escape . ,(format "%c%c"  escape escape))))))

;;;###autoload
(defun esqlite-text (string &optional quote-char)
  "Convenience function to provide make quoted STRING in sql."
  (setq quote-char (or quote-char ?\'))
  (format "%c%s%c"
          quote-char
          (esqlite-escape-string string quote-char)
          quote-char))

;;;###autoload
(defun esqlite-escape-string (string &optional quote-char)
  "Escape STRING as a esqlite string object context.
Optional QUOTE-CHAR arg indicate quote-char

e.g.
\(let ((user-input \"a\\\"'b\"))
  (format \"SELECT * FROM T WHERE a = '%s'\" \
\(esqlite-escape-string user-input ?\\')))
  => \"SELECT * FROM T WHERE a = 'a\\\"''b'\"

\(let ((user-input \"a\\\"'b\"))
  (format \"SELECT * FROM T WHERE a = \\\"%s\\\"\" \
\(esqlite-escape-string user-input ?\\\")))
  => \"SELECT * FROM T WHERE a = \\\"a\\\"\\\"'b\\\"\"
"
  (setq quote-char (or quote-char ?\'))
  (esqlite--replace
   string
   `((,quote-char . ,(format "%c%c" quote-char quote-char)))))

;;;###autoload
(defun esqlite-escape-like (query &optional escape-char)
  "Escape QUERY as a sql LIKE context.
This function is not quote single-quote (') you should use with
`esqlite-escape-string' or `esqlite-text'.

ESCAPE-CHAR is optional char (default '\\') for escape sequence expressed
following esqlite syntax.

e.g. fuzzy search of \"100%\" text in `value' column in `hoge' table.
   SELECT * FROM hoge WHERE value LIKE '%100\\%%' ESCAPE '\\'

To create the like pattern:
   => (concat \"%\" (esqlite-escape-like \"100%\" ?\\\\) \"%\")
   => \"%100\\%%\""
  (esqlite--replace
   query
   (esqlite-escape--like-table escape-char)))

;;;
;;; Esqlite stream
;;;

(defun esqlite-stream-p (obj)
  (and (processp obj)
       (process-get obj 'esqlite-stream-process-p)))

(defun esqlite-stream-alive-p (stream)
  (and (eq (process-status stream) 'run)
       ;; check buffer either.
       ;; process-status still `run' after killing buffer
       (buffer-live-p (process-buffer stream))))

(defun esqlite-stream-filename (stream)
  (process-get stream 'esqlite-filename))

(defun esqlite-stream--reuse (file)
  (loop with filename = (expand-file-name file)
        for p in (process-list)
        if (and (esqlite-stream-alive-p p)
                (string= (esqlite-stream-filename p) filename))
        return p))

(defun esqlite-stream--open (file)
  (let* ((stream (esqlite-start-csv-process file)))
    (process-put stream 'esqlite-stream-process-p t)
    ;; Do not show confirm prompt when exiting.
    ;; `esqlite-killing-emacs' close all stream.
    (set-process-query-on-exit-flag stream nil)
    (set-process-filter stream 'esqlite-stream--filter)
    (set-process-sentinel stream 'esqlite-stream--sentinel)
    (let ((inhibit-redisplay t))
      (esqlite-stream--wait stream))
    stream))

;;;###autoload
(defun esqlite-stream-open (file &optional force-open)
  "Open FILE stream as esqlite database if not open.
Optional FORCE-OPEN indicate do not reuse opened stream.

This function return process as stream object, but
 do not use this as a process object. This object style
 may be changed in future release."
  (esqlite-check-sqlite-program)
  (or (and (not force-open) (esqlite-stream--reuse file))
      (esqlite-stream--open file)))

(defun esqlite-stream-close (stream)
  (unless (process-get stream 'esqlite-stream-process-p)
    (error "esqlite: Not a esqlite process"))
  (when (eq (process-status stream) 'run)
    (with-timeout (5 (kill-process stream))
      ;; DO NOT use `esqlite-stream-send-command'
      ;; No need to wait prompt.
      (process-send-string stream ".quit\n")
      (let ((inhibit-redisplay t))
        (while (eq (process-status stream) 'run)
          (esqlite-sleep stream)))))
  ;; delete process forcibly
  (delete-process stream))

(defun esqlite-stream-set-coding-system (stream decoding encoding)
  (set-process-coding-system stream decoding encoding))

(defun esqlite-stream--filter (proc event)
  (esqlite--with-parse proc event
    (let ((filter (process-get proc 'esqlite-filter)))
      (when (functionp filter)
        (funcall filter proc)))))

(defun esqlite-stream--sentinel (proc event)
  (esqlite--with-process proc
    (when (memq (process-status proc) '(exit signal))
      (kill-buffer (current-buffer)))))

(defun esqlite-stream--csv-filter (proc)
  (let* ((null (process-get proc 'esqlite-null-value))
         (data (esqlite--read-csv-with-deletion null))
         (accum (process-get proc 'esqlite-accumulation)))
    (process-put proc 'esqlite-accumulation
                 (append accum data))))

(defun esqlite-stream-send (stream query)
  "High level api to send QUERY to STREAM by user manually.
If QUERY is a meta-command, return string of output from STREAM.
If QUERY is a sql statement, return just `t'"
  (cond
   ((string-match "^[ \t\n]*\\." query)
    (esqlite-stream-send-command-0 stream query))
   (t
    (esqlite-stream-send-sql stream query))))

(defun esqlite-stream-send-command (stream command &rest args)
  "Send COMMAND and ARGS to STREAM without checking COMMAND error.
You can call this function as a API. Not need to publish user.
 Use `esqlite-stream-send' as a high level API."
  (let ((line (format "%s %s\n" command (mapconcat 'esqlite-text args " "))))
    (esqlite-stream-send-command-0 stream line)))

(defun esqlite-stream-send-command-0 (stream query)
  "Send a meta command to esqlite STREAM.
This function return after checking syntax error of QUERY.

QUERY is a command line that may ommit newline."
  (let ((buf (process-buffer stream)))
    (with-current-buffer buf
      (esqlite-stream--until-prompt stream)
      ;; clear all text contains prompt.
      (erase-buffer)
      (process-put stream 'esqlite-syntax-error nil)
      (process-send-string stream (esqlite-terminate-command query))
      ;; only check syntax error.
      ;; This check maybe promptly return from esqlite process.
      (esqlite--maybe-raise-syntax-error stream)
      (goto-char (point-max))
      (buffer-substring (point-min) (point-at-eol 0)))))

(defun esqlite-stream-send-sql (stream sql)
  "Send SQL to esqlite STREAM.
This function return after checking syntax error of SQL.

SQL is a sql statement that may ommit statement end `;'.
 Do Not send multiple statements or comment statement.
 This may cause STREAM stalling with no error report.

Examples:
Good: SELECT * FROM table1;
Good: SELECT * FROM table1
Good: SELECT * FROM table1\n
 Bad: SELECT * FROM table1; SELECT * FROM table2;"
  ;; wait until previous sql was finished.
  (let ((buf (process-buffer stream)))
    (with-current-buffer buf
      (esqlite-stream--until-prompt stream)
      ;; clear all text contains prompt.
      (erase-buffer)
      (process-put stream 'esqlite-syntax-error nil)
      (process-send-string stream (esqlite-terminate-statement sql))
      ;; only check syntax error.
      ;; This check maybe promptly return from esqlite process.
      (esqlite--maybe-raise-syntax-error stream)
      t)))

(defalias 'esqlite-stream-execute 'esqlite-stream-send-sql)

(defun esqlite-stream-read (stream query)
  (unless (esqlite-stream-alive-p stream)
    (error "esqlite: Stream has been closed"))
  ;; handling NULL text
  ;; Use different null text each time when executing query.
  (let ((nullvalue (esqlite--temp-null query)))
    (esqlite-stream-send-command stream ".nullvalue" nullvalue)
    (process-put stream 'esqlite-null-value nullvalue))
  ;; send synchrounous variables.
  (process-put stream 'esqlite-filter 'esqlite-stream--csv-filter)
  (unwind-protect
      (progn
        ;; reset accumulate variable
        (process-put stream 'esqlite-accumulation nil)
        (esqlite-stream-send-sql stream query)
        ;; wait until prompt is displayed.
        ;; filter function handling csv data.
        (esqlite-stream--wait stream))
    (process-put stream 'esqlite-filter nil))
  (process-get stream 'esqlite-accumulation))

(defun esqlite-stream-read-top (stream query)
  "Convenience function with wrapping `esqlite-stream-read' to get a first row
of the results.

No performance advantage. You need to choose LIMIT statement by your own."
  (car-safe (esqlite-stream-read stream query)))

(defun esqlite-stream-read-atom (stream query)
  "Convenience function with wrapping `esqlite-stream-read-top'
to get a first item of the results."
  (car-safe (esqlite-stream-read-top stream query)))

;; wait until prompt to buffer
(defun esqlite-stream--wait (stream)
  (let ((buf (process-buffer stream)))
    (with-current-buffer buf
      (esqlite-stream--until-prompt stream))))

(defun esqlite-stream--until-prompt (stream)
  (while (and (eq (process-status stream) 'run)
              (not (esqlite-prompt-p)))
    (esqlite-sleep stream)))

(defun esqlite-stream-prompt-p (stream)
  (let ((buf (process-buffer stream)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (esqlite-prompt-p)))))

(defun esqlite-stream-buffer-string (stream)
  (let ((buf (process-buffer stream)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (buffer-string)))))

;;;
;;; Esqlite asynchronous read/execute
;;;

;;;###autoload
(defun esqlite-async-read (file query filter &rest args)
  "Execute QUERY in esqlite FILE and immediately exit the esqlite process.
FILTER called with one arg that is parsed csv line or `:EOF'.
  Please use `:EOF' argument finish this async process.
  This FILTER is invoked in process buffer.

If QUERY contains syntax error, raise the error result before return from
this function.
ARGS accept esqlite command arguments. (e.g. -header)"
  (esqlite-check-sqlite-program)
  (unless (stringp query)
    (error "esqlite: No query is provided"))
  (let* ((proc (apply 'esqlite-start-csv-process file query nil args)))
    (process-put proc 'esqlite-filter filter)
    (set-process-filter proc 'esqlite-async-read--filter)
    (set-process-sentinel proc 'esqlite-async-read--sentinel)
    (with-current-buffer (process-buffer proc)
      (esqlite--maybe-raise-syntax-error proc))
    nil))

(defun esqlite-async-read--filter (proc event)
  (esqlite--with-parse proc event
    (let ((filter (process-get proc 'esqlite-filter))
          (errmsg (process-get proc 'esqlite-syntax-error)))
      (cond
       ;; ignore if error
       ((stringp errmsg))
       ((functionp filter)
        (let* ((null (process-get proc 'esqlite-null-value))
               (data (esqlite--read-csv-with-deletion null)))
          (dolist (row data)
            (funcall filter row))))))))

(defun esqlite-async-read--sentinel (proc event)
  (esqlite--with-process proc
    (when (memq (process-status proc) '(exit))
      (let ((errmsg (process-get proc 'esqlite-syntax-error))
            (filter (process-get proc 'esqlite-filter)))
        (cond
         ;; ignore if error
         ((stringp errmsg))
         ((functionp filter)
          ;; If QUERY contains some error, `esqlite--maybe-raise-syntax-error'
          ;; should report error before sentinel.
          (funcall filter :EOF)))))
    (unless (memq (process-status proc) '(run))
      (kill-buffer (current-buffer)))))

;;;###autoload
(defun esqlite-async-execute (file query &optional finalize &rest args)
  "Utility function to wrap `esqlite-async-read'
This function expect non result set QUERY.
FINALIZE is function which call with no argument.
Other arguments are passed to `esqlite-async-read'."
  (let ((filter (if finalize
                    `(lambda (xs)
                       (when (eq xs :EOF)
                         (funcall ,finalize)))
                  `(lambda (xs)))))
    (apply 'esqlite-async-read file query filter args))
  nil)

;;;
;;; Synchronous utilities
;;;

;;;###autoload
(defun esqlite-call/stream (file func)
  "Open FILE as esqlite database.
FUNC accept just one arg created stream object from `esqlite-stream-open'."
  (let* ((stream (esqlite-stream-open file))
         ;; like a synchronously function
         (inhibit-redisplay t))
    (unwind-protect
        (funcall func stream)
      (esqlite-stream-close stream))))

(put 'esqlite-call/stream 'lisp-indent-function 1)

;;;###autoload
(defun esqlite-call/transaction (file func)
  "Open FILE as esqlite database and begin/commit/rollback transaction.
FUNC accept just one arg created stream object from `esqlite-stream-open'."
  (esqlite-call/stream file
    (lambda (stream)
      (esqlite-stream-send-sql stream "BEGIN")
      (condition-case err
          (prog1
              (funcall func stream)
            (esqlite-stream-send-sql stream "COMMIT"))
        (error
         ;; stream close automatically same as doing ROLLBACK.
         ;; but explicitly call this.
         (esqlite-stream-send-sql stream "ROLLBACK")
         (signal (car err) (cdr err)))))))

(put 'esqlite-call/transaction 'lisp-indent-function 1)

;;
;; Esqlite synchronous read/execute
;;

;;;###autoload
(defun esqlite-read (file query &rest args)
  "Read QUERY result in esqlite FILE.
This function designed with SELECT QUERY, but works fine another
 sql query (UPDATE/INSERT/DELETE).

ARGS accept some of esqlite command arguments but do not use it
 unless you understand what you are doing.
"
  (esqlite-check-sqlite-program)
  (with-temp-buffer
    (let* ((nullvalue (esqlite--temp-null query))
           (exit-code (apply 'esqlite-call-csv-process
                             file query nullvalue args)))
      (goto-char (point-min))
      (unless (eq exit-code 0)
        (let ((errmsg (esqlite--read-syntax-error-at-point)))
          (when errmsg
            (error "esqlite: %s" errmsg)))
        ;; raise error anyway
        (error "esqlite: %s" (buffer-string)))
      (esqlite--read-csv-with-deletion nullvalue))))

;;;###autoload
(defun esqlite-read-top (file query &rest args)
  "Convenience function with wrapping `esqlite-read' to get a first row
of the results.

No performance advantage. You need to choose LIMIT statement by your own."
  (car-safe (apply 'esqlite-read file query args)))

;;;###autoload
(defun esqlite-read-atom (file query &rest args)
  "Convenience function with wrapping `esqlite-read-top' to get a first item
of the results."
  (car-safe (apply 'esqlite-read-top file query args)))

;;;###autoload
(defun esqlite-execute (file sql)
  "Same as `esqlite-read' but intentional to use non SELECT statement."
  (esqlite-read file sql)
  nil)

;;
;; Read object from esqlite database
;;

(defun esqlite-read--objects (stream &optional type)
  (let* ((query
          (esqlite-format
           `(
             "SELECT name "
             " FROM sqlite_master "
             " WHERE 1 = 1 "
             ,@(and type
                    `(" AND type = %T{type}")))))
         (data (esqlite-stream-read stream query)))
    (mapcar 'car data)))

(defun esqlite-read-views (stream)
  (esqlite-read--objects stream "view"))

(defun esqlite-read-tables (stream)
  (esqlite-read--objects stream "table"))

(defun esqlite-read-indexes (stream)
  (esqlite-read--objects stream "index"))

(defun esqlite-read-triggers (stream)
  (esqlite-read--objects stream "trigger"))

(defun esqlite-read-table-columns (stream table)
  (loop for (_r1 col . _ignore) in (esqlite-read-table-schema stream table)
        collect col))

(defun esqlite-read-table-schema (stream table)
  "Get TABLE information in FILE.
Elements of the item list are:
0. cid
1. name with lowcase
2. type with UPCASE
3. not null (boolean)
4. default_value
5. primary key (boolean)"
  (loop for row in (esqlite-stream-read
                    stream (esqlite-format
                            "PRAGMA table_info(%o)"
                            table))
        collect (list
                 (string-to-number (nth 0 row))
                 (downcase (nth 1 row))
                 (upcase (nth 2 row))
                 (equal (nth 3 row) "1")
                 (nth 4 row)
                 (equal (nth 5 row) "1"))))

(defun esqlite-file-tables (file)
  "esqlite FILE tables"
  (esqlite-call/stream file
    (lambda (stream)
      (esqlite-read-tables stream))))

(defun esqlite-file-table-columns (file table)
  "esqlite FILE TABLE columns"
  (esqlite-call/stream file
    (lambda (stream)
      (esqlite-read-table-columns stream table))))

(defun esqlite-file-table-schema (file table)
  "See `esqlite-read-table-schema'"
  (esqlite-call/stream file
    (lambda (stream)
      (esqlite-read-table-schema stream table))))

;;;
;;; Package load/unload
;;;

(defun esqlite-killing-emacs ()
  (dolist (proc (process-list))
    (when (process-get proc 'esqlite-stream-process-p)
      (condition-case err
          (esqlite-stream-close proc)
        (error (message "esqlite: %s" err)))))
  (when (and (stringp esqlite--default-init-file)
             (file-exists-p esqlite--default-init-file))
    (delete-file esqlite--default-init-file)))

(defun esqlite-unload-function ()
  (remove-hook 'kill-emacs-hook 'esqlite-killing-emacs))

(add-hook 'kill-emacs-hook 'esqlite-killing-emacs)

(provide 'esqlite)

;;; esqlite.el ends here
