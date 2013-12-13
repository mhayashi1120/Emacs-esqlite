;;; sqlite3.el --- sqlite3 file manipulate utilities

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: data
;; URL: https://github.com/mhayashi1120/sqlite3.el/raw/master/sqlite3.el
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

;; sqlite3.el is a implementation to handle sqlite3 database.
;; Following functions are provided:
;; * Read sqlite3 row as list of string.
;; * Async read sqlite3 row as list of string.
;; * sqlite3 process with being stationed
;; * Construct sqlite3 SQL.
;; * Escape SQL value to construct SQL
;; * Some of basic utilities.
;; * NULL handling (denote as :null keyword)

;;; Install:

;; Please install from ELPA. (TODO url)

;;; TODO:
;; * blob

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'pcsv)

(defgroup sqlite3 ()
  "Manipulate sqlite3 database."
  :prefix "sqlite3-"
  :group 'applications)

(defcustom sqlite3-program "sqlite3"
  "Command name or path to command."
  :group 'sqlite3
  :type '(choice file string))

;;;
;;; Basic utilities
;;;

(defun sqlite3--replace (string init-table)
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
                   (error "sqlite3: Not supported yet"))))))

(defun sqlite3--filter (filter list)
  (loop for o in list
        if (funcall filter o)
        collect o))

;;;
;;; System deps
;;;

(defcustom sqlite3-mswin-fakecygpty-program nil
  "sqlite3 on cygwin cannot work since no pty.
Please download and install fakecygpty (Google it!!)"
  :group 'sqlite3
  :type '(choice file string))

(defun sqlite3-mswin-native-p ()
  (and (memq system-type '(windows-nt))
       (not (sqlite3-mswin-cygwin-p))))

(defun sqlite3-mswin-cygwin-p ()
  (and (memq system-type '(windows-nt))
       (executable-find "cygpath")
       (let ((program (executable-find sqlite3-program)))
         (and program
              ;; check program is under the cygwin installed directory.
              (let* ((rootdir
                      (let* ((line (sqlite3-mswin-cygpath "/" "windows"))
                             (root (expand-file-name line)))
                        (file-name-as-directory root)))
                     (regexp (concat "\\`" (regexp-quote rootdir))))
                (string-match regexp program))))
       t))

(defun sqlite3-mswin-cygpath (file type)
  (with-temp-buffer
    (let ((code (call-process "cygpath" nil t nil "-t" type file)))
      (unless (= code 0)
        (error "sqlite3: cygpath failed with %d" code))
      (goto-char (point-min))
      (buffer-substring
       (point-min) (point-at-eol)))))

;;;
;;; Basic utilities to handle sqlite3 i/o.
;;;

;;
;; Generate temporary name
;;

(defun sqlite3--temp-null (query)
  ;; sqlite3 command nullvalue assigned to 20 chars (19 + null).
  (sqlite3--random-text query 19))

;; * random char restrict to `a-zA-Z0-9+/' (except last `=')
;; * md5 have 16 bytes
;; * base64 encode 16 bytes -> 44 chars
;; * to trim last `=', only use md5 first 15 bytes make (* 4 (/ 15 3)) 20 bytes
(defun sqlite3--random-text (seed length)
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

(defun sqlite3--unique-name (stream prefix &optional seed)
  (loop with objects = (sqlite3-read--objects stream)
        with full-name
        while (let ((random-text (sqlite3--random-text (or seed "") 40)))
                (setq full-name (format "%s_%s" prefix random-text))
                (member full-name objects))
        finally return full-name))

(defun sqlite3-terminate-command (command)
  (cond
   ((not (string-match "\n\\'" command))
    (concat command "\n"))
   (t
    command)))

(defun sqlite3-terminate-statement (sql)
  (cond
   ((not (string-match ";[ \t\n]*\\'" sql))
    (concat sql ";\n"))
   ((not (string-match "\n\\'" sql))
    (concat sql "\n"))
   (t sql)))

;;
;; constant / system dependent
;;

(defconst sqlite3--rowid-columns
  '("_ROWID_" "ROWID" "OID"))

;;;###autoload
(defconst sqlite3-file-header-regexp "\\`SQLite format 3\000")

;;;###autoload
(defun sqlite3-file-guessed-database-p (file)
  "Guess the FILE is a sqlite3 database or not."
  (and (file-regular-p file)
       (with-temp-buffer
         (insert-file-contents file nil 0 256)
         (looking-at sqlite3-file-header-regexp))))

;;;###autoload
(defun sqlite3-installed-p ()
  "Return non-nil if `sqlite3-program' is installed."
  (and (stringp sqlite3-program)
       (executable-find sqlite3-program)))

(defun sqlite3-check-program ()
  (unless (stringp sqlite3-program)
    (error "No valid sqlite3 program"))
  (unless (executable-find sqlite3-program)
    (error "%s not found" sqlite3-program)))

;;
;; process
;;

(defvar sqlite3-process-coding-system
  (let* ((syseol (let ((type (coding-system-eol-type
                              ;; guess sqlite3 command environment
                              default-terminal-coding-system)))
                   (cond
                    ((or (symbolp type)
                         (numberp type))
                     type)
                    ((vectorp type)
                     (coding-system-eol-type (aref type 0))))))
         ;; utf-8 as a default coding-system of sqlite3 database.
         (basecs 'utf-8)
         (cs
          (coding-system-change-eol-conversion basecs syseol)))
    (cons cs cs))
  "Temporarily change coding system by this hiding parameter.")

(defconst sqlite3-prompt "sqlite> ")
(defconst sqlite3-continue-prompt "")

(defun sqlite3-prompt-p ()
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    ;; when executed sql contains newline, continue prompt displayed
    ;; before last prompt "sqlite> "
    (sqlite3-looking-at-prompt)))

(defconst sqlite3-prompt-regexp
  (concat "^"
          "\\( *"
          (regexp-quote sqlite3-continue-prompt)
          "\\)*"
          (regexp-quote sqlite3-prompt)
          "\\'"))

(defvar sqlite3--default-init-file nil)

;; To avoid user initialization file ~/.sqliterc
(defun sqlite3--default-init-file (&optional refresh)
  (or (and (not refresh)
           (stringp sqlite3--default-init-file)
           (file-exists-p sqlite3--default-init-file)
           sqlite3--default-init-file)
      (setq sqlite3--default-init-file
            (let ((file (make-temp-file "emacs-sqlite3-")))
              (with-temp-buffer
                (insert (format ".prompt \"%s\" \"%s\"\n"
                                sqlite3-prompt
                                sqlite3-continue-prompt))
                (write-region (point-min) (point-max) file nil 'no-msg))
              file))))

(defmacro sqlite3--with-env (&rest form)
  `(let ((process-environment (copy-sequence process-environment))
         ;; non pty resulting in echoing query.
         (process-connection-type t)
         (default-process-coding-system
           (cond
            ((consp sqlite3-process-coding-system)
             sqlite3-process-coding-system)
            ((coding-system-p sqlite3-process-coding-system)
             (cons sqlite3-process-coding-system sqlite3-process-coding-system))
            (t nil))))
     ;; currently no meanings of this
     ;; in the future release may support i18n.
     (setenv "LANG" "C")
     ,@form))

(defmacro sqlite3--with-process (proc &rest form)
  (declare (indent 1) (debug t))
  `(let ((buf (process-buffer proc)))
     (when (buffer-live-p buf)
       (with-current-buffer buf
         ,@form))))

(defmacro sqlite3--with-parse (proc event &rest form)
  "Execute FORM in PROC buffer with appended EVENT.
This form check syntax error report from sqlite3 command."
  (declare (indent 2) (debug t))
  `(sqlite3--with-process proc
     (save-excursion
       (goto-char (point-max))
       (insert event)
       (goto-char (point-min))
       (unless (process-get proc 'sqlite3-syntax-error)
         ;; check only first time filter received data.

         ;;FIXME this error output to stderr. Separate stderr to other file??
         ;;     Altough rarely happens, worrying about confusing stdout/stderr
         (let ((errmsg (sqlite3--read-syntax-error-at-point)))
           (process-put proc 'sqlite3-syntax-error (or errmsg t))))
       ,@form)))

(defun sqlite3-start-process (buffer &rest args)
  (sqlite3--with-env
   (let ((cmdline (cons sqlite3-program args))
         (cygwinp (sqlite3-mswin-cygwin-p)))
     (when cygwinp
       (unless sqlite3-mswin-fakecygpty-program
         (error "%s %s"
                "sqlite3 on cygwin cannot work unless `fakecygpty'"
                "see `sqlite3-mswin-fakecygpty-program' settings"))
       (setq cmdline
             (cons sqlite3-mswin-fakecygpty-program cmdline)))
     (apply 'start-process "Sqlite3" buffer cmdline))))

(defun sqlite3-call-process-region (buffer start end &rest args)
  (sqlite3--with-env
   (apply 'call-process-region start end sqlite3-program nil buffer nil args)))

(defun sqlite3-expand-db-name (file)
  (if (sqlite3-mswin-cygwin-p)
      ;; sqlite3 on cygwin cannot accept c:/hoge like path.
      ;; must convert /cygdrive/c/hoge
      ;; But seems accept such path in -init option.
      (sqlite3-mswin-cygpath file "unix")
    (expand-file-name file)))

(defun sqlite3--create-process-buffer ()
  (generate-new-buffer " *sqlite3 work* "))

(defun sqlite3-start-csv-process (file &optional query nullvalue &rest args)
  "Start async sqlite3 process.

Cygwin: FILE contains multibyte char, may fail to open FILE as database."
  (let* ((init (sqlite3--default-init-file))
         (db (sqlite3-expand-db-name file))
         (filename (expand-file-name file))
         (null (or nullvalue (sqlite3--temp-null query)))
         (args `(
                 ,@(if query `("-batch") '("-interactive"))
                 "-init" ,init
                 "-csv"
                 "-nullvalue" ,null
                 ;; prior than preceeding args
                 ,@args
                 ,db))
         (buf (sqlite3--create-process-buffer))
         (proc (apply 'sqlite3-start-process buf args)))
    (process-put proc 'sqlite3-init-file init)
    (process-put proc 'sqlite3-filename filename)
    (process-put proc 'sqlite3-null-value null)
    (when query
      (process-send-string proc query)
      ;; imitate calling command
      ;; e.g. sqlite3 db.sqlite3 "select some,of,query from table"
      (process-send-string proc ";\n.exit\n"))
    proc))

(defun sqlite3-call-csv-process (file query &optional nullvalue &rest args)
  "Call sqlite3 process.

Cygwin: FILE contains multibyte char, may fail to open FILE as database."
  (let* ((init (sqlite3--default-init-file))
         (db (sqlite3-expand-db-name file))
         (null (or nullvalue (sqlite3--temp-null query)))
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
     'sqlite3-call-process-region
     (current-buffer)
     ;; `call-process-region' start argument accept string
     (concat query ";\n") nil
     args)))

(defun sqlite3-looking-at-prompt ()
  (looking-at sqlite3-prompt-regexp))

(defun sqlite3--read-syntax-error-at-point ()
  (and (looking-at "^Error: \\(.*\\)")
       (format "sqlite3: %s" (match-string 1))))

(defun sqlite3--maybe-raise-syntax-error (proc)
  (while (and (eq (process-status proc) 'run)
              (not (sqlite3-prompt-p))
              (null (process-get proc 'sqlite3-syntax-error)))
    (sqlite3-sleep proc))
  (when (stringp (process-get proc 'sqlite3-syntax-error))
    (error "%s" (process-get proc 'sqlite3-syntax-error))))

;;
;; read csv from sqlite3
;;

(defun sqlite3--read-csv-line (&optional null)
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

(defun sqlite3--read-csv-line-with-deletion (null)
  (let ((start (point))
        (row (sqlite3--read-csv-line null)))
    (delete-region start (point))
    row))

(defun sqlite3--read-csv-with-deletion (null)
  "Read csv data from current point.
Delete csv data if reading was succeeded."
  (let (res)
    (condition-case nil
        (while (not (eobp))
          (let ((start (point))
                (row (sqlite3--read-csv-line-with-deletion null)))
            (setq res (cons row res))))
      ;; output is proceeding from process
      ;; finish the reading
      (invalid-read-syntax nil))
    (nreverse res)))

;;
;; sqlite3 data (csv) <-> emacs data
;;

(defun sqlite3-format-object (object)
  (concat "\""
          (replace-regexp-in-string "\"" "\"\"" object)
          "\""))

(defun sqlite3-format-value (object)
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
    (mapconcat 'sqlite3-format-value object ", "))
   (t
    (error "sqlite3: Not a supported type %s" object))))

(eval-and-compile
  (defvar sqlite3-format--table
    (eval-when-compile
      '(
        ("s" . (lambda (x)
                 (cond
                  ((stringp x) x)
                  ((numberp x) (number-to-string x))
                  (t (prin1-to-string x)))))
        ("t" . sqlite3-escape-string)
        ("T" . sqlite3-text)
        ("l" . sqlite3-escape-like)
        ("L" . (lambda (x)
                 (concat
                  (sqlite3-text (sqlite3-escape-like x))
                  " ESCAPE '\\' ")))
        ;; some database object
        ("o" . sqlite3-format-object)
        ;; column list
        ("O" . (lambda (l)
                 (mapconcat (lambda (x) (sqlite3-format-object x))
                            l ", ")))
        ;; some value list (string, number, list) with properly quoted
        ("V" . sqlite3-format-value)
        ))))

;;;###autoload
(defun sqlite3-format (sqlite3-fmt &rest sqlite3-objects)
  "Prepare sql with SQLITE3-FMT like `format'.

SQLITE3-FMT is a string or list of string.
 each list item join with newline.

Each directive accept arg which contains variable name.
  This variable name must not contain `sqlite3-' prefix.
e.g.
\(let ((some-var \"FOO\")) (sqlite3-format \"%s %s{some-var}\" \"HOGE\"))
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
    (when (listp sqlite3-fmt)
      (setq sqlite3-fmt (mapconcat 'identity sqlite3-fmt "\n")))
    (insert sqlite3-fmt)
    (goto-char (point-min))
    (let ((sqlite3-fmt-regexp
           (eval-when-compile
             (concat
              (regexp-opt (mapcar 'car sqlite3-format--table) t)
              ;; Optional varname
              "\\(?:{\\(.+?\\)}\\)?")))
          (case-fold-search nil))
      (while (search-forward "%" nil t)
        (cond
         ((eq (char-after) ?%)
          (delete-char 1))
         ((looking-at sqlite3-fmt-regexp)
          (let* ((spec (match-string 1))
                 (varname (match-string 2))
                 (varsym (intern-soft varname))
                 (fn (assoc-default spec sqlite3-format--table))
                 obj)
            (when (and varname (string-match "\\`sqlite3-" varname))
              (error "sqlite3: Unable use sqlite3- prefix variable %s" varname))
            ;; Delete formatter directive
            (delete-region (1- (point)) (match-end 0))
            (cond
             (varsym
              ;; raise error asis if varname is not defined.
              (setq obj (symbol-value varsym)))
             (sqlite3-objects
              (setq obj (car sqlite3-objects))
              (setq sqlite3-objects (cdr sqlite3-objects)))
             (t
              (error "sqlite3: No value assigned to `%%%s'" spec)))
            (unless fn
              (error "sqlite3: Invalid format character: `%%%s'" spec))
            (unless (functionp fn)
              (error "Assert"))
            (let ((val obj)
                  text)
              (setq text (funcall fn val))
              (insert text)))))))
    (when sqlite3-objects
      (error "sqlite3: args out of range %s" sqlite3-objects))
    (buffer-string)))

;; http://www.sqlite.org/syntaxdiagrams.html#numeric-literal
(defconst sqlite3-numeric-literal-regexp
  (eval-when-compile
    (concat
     "\\`"
     "[+-]?"
     "\\([0-9]+\\(\\.\\([0-9]*\\)\\)?\\|\\.[0-9]+\\)"
     "\\(E[+-]?[0-9]+\\)?"
     "\\'")))

(defun sqlite3-numeric-text-p (text)
  "Utility function to check TEXT is numeric"
  (and (string-match sqlite3-numeric-literal-regexp text)
       t))

;;
;; sleep in process filter
;;

(defvar sqlite3-sleep-second
  ;;FIXME
  ;; This check calculate response of external command which does not accept
  ;; from process output.
  (apply 'min (loop for _ in '(1 2 3 4 5)
                    collect
                    (let ((start (float-time)))
                      (call-process "echo" nil nil nil "1")
                      (let ((end (float-time)))
                        (- end start))))))

(defun sqlite3-sleep (_dummy)
  ;; Other code affect to buffer while `sleep-for'.
  ;; TODO why need save-excursion? timer? filter? I can't get clue.
  (save-excursion
    ;; Achieve like a asynchronous behavior (ex: draw mode line)
    (redisplay)
    (sleep-for sqlite3-sleep-second)))

;;
;; Escape text
;;

(defun sqlite3-escape--like-table (escape-char &optional override)
  (let ((escape (or escape-char ?\\)))
    (append
     override
     `((?\% . ,(format "%c%%" escape))
       (?\_ . ,(format "%c_"  escape))
       (,escape . ,(format "%c%c"  escape escape))))))

;;;###autoload
(defun sqlite3-text (string &optional quote-char)
  "Convenience function to provide make quoted STRING in sql."
  (setq quote-char (or quote-char ?\'))
  (format "%c%s%c"
          quote-char
          (sqlite3-escape-string string quote-char)
          quote-char))

;;;###autoload
(defun sqlite3-escape-string (string &optional quote-char)
  "Escape STRING as a sqlite3 string object context.
Optional QUOTE-CHAR arg indicate quote-char

e.g.
\(let ((user-input \"a\\\"'b\"))
  (format \"SELECT * FROM T WHERE a = '%s'\" (sqlite3-escape-string user-input ?\\')))
  => \"SELECT * FROM T WHERE a = 'a\\\"''b'\"

\(let ((user-input \"a\\\"'b\"))
  (format \"SELECT * FROM T WHERE a = \\\"%s\\\"\" (sqlite3-escape-string user-input ?\\\")))
  => \"SELECT * FROM T WHERE a = \\\"a\\\"\\\"'b\\\"\"
"
  (setq quote-char (or quote-char ?\'))
  (sqlite3--replace
   string
   `((,quote-char . ,(format "%c%c" quote-char quote-char)))))

;;;###autoload
(defun sqlite3-escape-like (query &optional escape-char)
  "Escape QUERY as a sql LIKE context.
This function is not quote single-quote (') you should use with
`sqlite3-escape-string' or `sqlite3-text'.

ESCAPE-CHAR is optional char (default '\\') for escape sequence expressed
following sqlite3 syntax.

e.g. fuzzy search of \"100%\" text in `value' column in `hoge' table.
   SELECT * FROM hoge WHERE value LIKE '%100\\%%' ESCAPE '\\'

To create the like pattern:
   => (concat \"%\" (sqlite3-escape-like \"100%\" ?\\\\) \"%\")
   => \"%100\\%%\""
  (sqlite3--replace
   query
   (sqlite3-escape--like-table escape-char)))

;;;
;;; Sqlite3 stream
;;;

(defun sqlite3-stream-p (obj)
  (and (processp obj)
       (process-get obj 'sqlite3-stream-process-p)))

(defun sqlite3-stream-alive-p (stream)
  (and (eq (process-status stream) 'run)
       ;; check buffer either.
       ;; process-status still `run' after killing buffer
       (buffer-live-p (process-buffer stream))))

(defun sqlite3-stream-filename (stream)
  (process-get stream 'sqlite3-filename))

(defun sqlite3-stream--reuse (file)
  (loop with filename = (expand-file-name file)
        for p in (process-list)
        if (and (sqlite3-stream-alive-p p)
                (string= (sqlite3-stream-filename p) filename))
        return p))

(defun sqlite3-stream--open (file)
  (let* ((stream (sqlite3-start-csv-process file)))
    (process-put stream 'sqlite3-stream-process-p t)
    ;; Do not show confirm prompt when exiting.
    ;; `sqlite3-killing-emacs' close all stream.
    (set-process-query-on-exit-flag stream nil)
    (set-process-filter stream 'sqlite3-stream--filter)
    (set-process-sentinel stream 'sqlite3-stream--sentinel)
    (let ((inhibit-redisplay t))
      (sqlite3-stream--wait stream))
    stream))

;;;###autoload
(defun sqlite3-stream-open (file &optional force-open)
  "Open FILE stream as sqlite3 database if not open.
Optional FORCE-OPEN indicate do not reuse opened stream.

This function return process as stream object, but do not use this as a process object.
This object style may be changed in future release."
  (sqlite3-check-program)
  (or (and (not force-open) (sqlite3-stream--reuse file))
      (sqlite3-stream--open file)))

(defun sqlite3-stream-close (stream)
  (unless (process-get stream 'sqlite3-stream-process-p)
    (error "sqlite3: Not a sqlite3 process"))
  (when (eq (process-status stream) 'run)
    (with-timeout (5 (kill-process stream))
      ;; DO NOT use `sqlite3-stream-send-command'
      ;; No need to wait prompt.
      (process-send-string stream ".quit\n")
      (let ((inhibit-redisplay t))
        (while (eq (process-status stream) 'run)
          (sqlite3-sleep stream)))))
  ;; delete process forcibly
  (delete-process stream))

(defun sqlite3-stream-set-coding-system (stream decoding encoding)
  (set-process-coding-system stream decoding encoding))

(defun sqlite3-stream--filter (proc event)
  (sqlite3--with-parse proc event
    (let ((filter (process-get proc 'sqlite3-filter)))
      (when (functionp filter)
        (funcall filter proc)))))

(defun sqlite3-stream--sentinel (proc event)
  (sqlite3--with-process proc
    (when (memq (process-status proc) '(exit signal))
      (kill-buffer (current-buffer)))))

(defun sqlite3-stream--csv-filter (proc)
  (let* ((null (process-get proc 'sqlite3-null-value))
         (data (sqlite3--read-csv-with-deletion null))
         (accum (process-get proc 'sqlite3-accumulation)))
    (process-put proc 'sqlite3-accumulation
                 (append accum data))))

(defun sqlite3-stream-send (stream query)
  "High level api to send QUERY to STREAM by user manually.
If QUERY is a meta-command, return string of output from STREAM.
If QUERY is a sql statement, return just `t'"
  (cond
   ((string-match "^[ \t\n]*\\." query)
    (sqlite3-stream-send-command-0 stream query))
   (t
    (sqlite3-stream-send-sql stream query))))

(defun sqlite3-stream-send-command (stream command &rest args)
  "Send COMMAND and ARGS to STREAM without checking COMMAND error.
You can call this function as a API. Not need to publish user.
 Use `sqlite3-stream-send' as a high level API."
  (let ((line (format "%s %s\n" command (mapconcat 'sqlite3-text args " "))))
    (sqlite3-stream-send-command-0 stream line)))

(defun sqlite3-stream-send-command-0 (stream query)
  "Send a meta command to sqlite3 STREAM.
This function return after checking syntax error of QUERY.

QUERY is a command line that may ommit newline."
  (let ((buf (process-buffer stream)))
    (with-current-buffer buf
      (sqlite3-stream--until-prompt stream)
      ;; clear all text contains prompt.
      (erase-buffer)
      (process-put stream 'sqlite3-syntax-error nil)
      (process-send-string stream (sqlite3-terminate-command query))
      ;; only check syntax error.
      ;; This check maybe promptly return from sqlite3 process.
      (sqlite3--maybe-raise-syntax-error stream)
      (goto-char (point-max))
      (buffer-substring (point-min) (point-at-eol 0)))))

(defun sqlite3-stream-send-sql (stream sql)
  "Send SQL to sqlite3 STREAM.
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
      (sqlite3-stream--until-prompt stream)
      ;; clear all text contains prompt.
      (erase-buffer)
      (process-put stream 'sqlite3-syntax-error nil)
      (process-send-string stream (sqlite3-terminate-statement sql))
      ;; only check syntax error.
      ;; This check maybe promptly return from sqlite3 process.
      (sqlite3--maybe-raise-syntax-error stream)
      t)))

(defalias 'sqlite3-stream-execute 'sqlite3-stream-send-sql)

(defun sqlite3-stream-read (stream query)
  (unless (sqlite3-stream-alive-p stream)
    (error "sqlite3: Stream has been closed"))
  ;; handling NULL text
  ;; Use different null text each time when executing query.
  (let ((nullvalue (sqlite3--temp-null query)))
    (sqlite3-stream-send-command stream ".nullvalue" nullvalue)
    (process-put stream 'sqlite3-null-value nullvalue))
  ;; send synchrounous variables.
  (process-put stream 'sqlite3-filter 'sqlite3-stream--csv-filter)
  (unwind-protect
      (progn
        ;; reset accumulate variable
        (process-put stream 'sqlite3-accumulation nil)
        (sqlite3-stream-send-sql stream query)
        ;; wait until prompt is displayed.
        ;; filter function handling csv data.
        (sqlite3-stream--wait stream))
    (process-put stream 'sqlite3-filter nil))
  (process-get stream 'sqlite3-accumulation))

(defun sqlite3-stream-read-top (stream query)
  "Convenience function with wrapping `sqlite3-stream-read' to get a first row
of the results."
  (car-safe (sqlite3-stream-read stream query)))

(defun sqlite3-stream-read-atom (stream query)
  "Convenience function with wrapping `sqlite3-stream-read-top' to get a first item
of the results."
  (car-safe (sqlite3-stream-read-top stream query)))

;; wait until prompt to buffer
(defun sqlite3-stream--wait (stream)
  (let ((buf (process-buffer stream)))
    (with-current-buffer buf
      (sqlite3-stream--until-prompt stream))))

(defun sqlite3-stream--until-prompt (stream)
  (while (and (eq (process-status stream) 'run)
              (not (sqlite3-prompt-p)))
    (sqlite3-sleep stream)))

(defun sqlite3-stream-prompt-p (stream)
  (let ((buf (process-buffer stream)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (sqlite3-prompt-p)))))

(defun sqlite3-stream-buffer-string (stream)
  (let ((buf (process-buffer stream)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (buffer-string)))))

;;;
;;; Sqlite3 asynchronous read/execute
;;;

;;;###autoload
(defun sqlite3-async-read (file query filter &rest args)
  "Execute QUERY in sqlite3 FILE and immediately exit the sqlite3 process.
FILTER called with one arg that is parsed csv line or `:EOF'.
  Please use `:EOF' argument finish this async process.
  This FILTER is invoked in process buffer.

If QUERY contains syntax error, raise the error result before return from
this function.
ARGS accept sqlite3 command arguments. (e.g. -header)"
  (sqlite3-check-program)
  (unless (stringp query)
    (error "sqlite3: No query is provided"))
  (let* ((proc (apply 'sqlite3-start-csv-process file query nil args)))
    (process-put proc 'sqlite3-filter filter)
    (set-process-filter proc 'sqlite3-async-read--filter)
    (set-process-sentinel proc 'sqlite3-async-read--sentinel)
    (with-current-buffer (process-buffer proc)
      (sqlite3--maybe-raise-syntax-error proc))
    nil))

(defun sqlite3-async-read--filter (proc event)
  (sqlite3--with-parse proc event
    (let ((filter (process-get proc 'sqlite3-filter))
          (errmsg (process-get proc 'sqlite3-syntax-error)))
      (cond
       ;; ignore if error
       ((stringp errmsg))
       ((functionp filter)
        (let* ((null (process-get proc 'sqlite3-null-value))
               (data (sqlite3--read-csv-with-deletion null)))
          (dolist (row data)
            (funcall filter row))))))))

(defun sqlite3-async-read--sentinel (proc event)
  (sqlite3--with-process proc
    (when (memq (process-status proc) '(exit))
      (let ((errmsg (process-get proc 'sqlite3-syntax-error))
            (filter (process-get proc 'sqlite3-filter)))
        (cond
         ;; ignore if error
         ((stringp errmsg))
         ((functionp filter)
          ;; If QUERY contains some error, `sqlite3--maybe-raise-syntax-error'
          ;; should report error before sentinel.
          (funcall filter :EOF)))))
    (unless (memq (process-status proc) '(run))
      (kill-buffer (current-buffer)))))

;;;###autoload
(defun sqlite3-async-execute (file query &optional finalize &rest args)
  "Utility function to wrap `sqlite3-async-read'
This function expect non result set QUERY.
FINALIZE is function which call with no argument.
Other arguments are passed to `sqlite3-async-read'."
  (let ((filter (if finalize
                    `(lambda (xs)
                       (when (eq xs :EOF)
                         (funcall ,finalize)))
                  `(lambda (xs)))))
    (apply 'sqlite3-async-read file query filter args))
  nil)

;;;
;;; Synchronous utilities
;;;

;;;###autoload
(defun sqlite3-call/stream (file func)
  "Open FILE as sqlite3 database.
FUNC accept just one arg created stream object from `sqlite3-stream-open'."
  (let* ((stream (sqlite3-stream-open file))
         ;; like a synchronously function
         (inhibit-redisplay t))
    (unwind-protect
        (funcall func stream)
      (sqlite3-stream-close stream))))

(put 'sqlite3-call/stream 'lisp-indent-function 1)

;;;###autoload
(defun sqlite3-call/transaction (file func)
  "Open FILE as sqlite3 database and begin/commit/rollback transaction.
FUNC accept just one arg created stream object from `sqlite3-stream-open'."
  (sqlite3-call/stream file
    (lambda (stream)
      (sqlite3-stream-send-sql stream "BEGIN")
      (condition-case err
          (prog1
              (funcall func stream)
            (sqlite3-stream-send-sql stream "COMMIT"))
        (error
         ;; stream close automatically same as doing ROLLBACK.
         ;; but explicitly call this.
         (sqlite3-stream-send-sql stream "ROLLBACK")
         (signal (car err) (cdr err)))))))

(put 'sqlite3-call/transaction 'lisp-indent-function 1)

;;
;; Sqlite3 synchronous read/execute
;;

;;;###autoload
(defun sqlite3-read (file query &rest args)
  "Read QUERY result in sqlite3 FILE.
This function designed with SELECT QUERY, but works fine another
 sql query (UPDATE/INSERT/DELETE).

ARGS accept some of sqlite3 command arguments but do not use it
 unless you understand what you are doing.
"
  (sqlite3-check-program)
  (with-temp-buffer
    (let* ((nullvalue (sqlite3--temp-null query))
           (exit-code (apply 'sqlite3-call-csv-process
                             file query nullvalue args)))
      (goto-char (point-min))
      (unless (eq exit-code 0)
        (let ((errmsg (sqlite3--read-syntax-error-at-point)))
          (when errmsg
            (error "%s" errmsg)))
        (error "sqlite3: %s" (buffer-string)))
      (sqlite3--read-csv-with-deletion nullvalue))))

;;;###autoload
(defun sqlite3-read-top (file query &rest args)
  "Convenience function with wrapping `sqlite3-read' to get a first row
of the results."
  (car-safe (apply 'sqlite3-read file query args)))

;;;###autoload
(defun sqlite3-read-atom (file query &rest args)
  "Convenience function with wrapping `sqlite3-read-top' to get a first item
of the results."
  (car-safe (apply 'sqlite3-read-top file query args)))

;;;###autoload
(defun sqlite3-execute (file sql)
  "Same as `sqlite3-read' but intentional to use non SELECT statement."
  (sqlite3-read file sql)
  nil)

;;
;; Read object from sqlite3 database
;;

(defun sqlite3-read--objects (stream &optional type)
  (let* ((query
          (sqlite3-format
           `(
             "SELECT name "
             " FROM sqlite_master "
             " WHERE 1 = 1 "
             ,@(and type
                    `(" AND type = %T{type}")))))
         (data (sqlite3-stream-read stream query)))
    (mapcar 'car data)))

(defun sqlite3-read-views (stream)
  (sqlite3-read--objects stream "view"))

(defun sqlite3-read-tables (stream)
  (sqlite3-read--objects stream "table"))

(defun sqlite3-read-indexes (stream)
  (sqlite3-read--objects stream "index"))

(defun sqlite3-read-triggers (stream)
  (sqlite3-read--objects stream "trigger"))

(defun sqlite3-read-table-columns (stream table)
  (loop for (_r1 col . _ignore) in (sqlite3-read-table-schema stream table)
        collect col))

(defun sqlite3-read-table-schema (stream table)
  "Get TABLE information in FILE.
Elements of the item list are:
0. cid
1. name with lowcase
2. type with UPCASE
3. not null (boolean)
4. default_value
5. primary key (boolean)"
  (loop for row in (sqlite3-stream-read
                    stream (sqlite3-format
                            "PRAGMA table_info(%o)"
                            table))
        collect (list
                 (string-to-number (nth 0 row))
                 (downcase (nth 1 row))
                 (upcase (nth 2 row))
                 (equal (nth 3 row) "1")
                 (nth 4 row)
                 (equal (nth 5 row) "1"))))

(defun sqlite3-file-tables (file)
  "sqlite3 FILE tables"
  (sqlite3-call/stream file
    (lambda (stream)
      (sqlite3-read-tables stream))))

(defun sqlite3-file-table-columns (file table)
  "sqlite3 FILE TABLE columns"
  (sqlite3-call/stream file
    (lambda (stream)
      (sqlite3-read-table-columns stream table))))

(defun sqlite3-file-table-schema (file table)
  "See `sqlite3-read-table-schema'"
  (sqlite3-call/stream file
    (lambda (stream)
      (sqlite3-read-table-schema stream table))))

;;;
;;; Package load/unload
;;;

(defun sqlite3-killing-emacs ()
  (dolist (proc (process-list))
    (when (process-get proc 'sqlite3-stream-process-p)
      (condition-case err
          (sqlite3-stream-close proc)
        (error (message "sqlite3: %s" err)))))
  (when (and (stringp sqlite3--default-init-file)
             (file-exists-p sqlite3--default-init-file))
    (delete-file sqlite3--default-init-file)))

(defun sqlite3-unload-function ()
  (remove-hook 'kill-emacs-hook 'sqlite3-killing-emacs))

(add-hook 'kill-emacs-hook 'sqlite3-killing-emacs)

(provide 'sqlite3)

;;; sqlite3.el ends here
