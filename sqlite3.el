;;; sqlite3.el --- sqlite3 file manipulate utilities

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: data
;; URL: http://github.com/mhayashi1120/sqlite3.el/raw/master/sqlite3.el
;; Emacs: GNU Emacs 24 or later

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

;; TODO

;; * save-all-buffers...
;; * Specification
;; allow invalid character in data.
;; disallow invalid character in column and table.

;;; Install:

;; (autoload 'sqlite3-file-guessed-database-p "sqlite3"
;;   "Guess the FILE is a sqlite3 database or not")

;;; TODO:
;; * blob
;; * describe about :null

;;; Code:

;;TODO when table is locked

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
                   (error "Not supported yet"))))))

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
        (error "cygpath failed with %d" code))
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
  (loop with key = (format "%s:%s" (current-time) query)
        with hash = (md5 key)
        for i from 0 below (length hash) by 2
        ;; make random unibyte string
        concat (let* ((hex (substring hash i (+ i 2)))
                      (c (string-to-number hex 16)))
                 (char-to-string c))
        into res
        finally return
        ;; md5 hex fold to base64 area
        (let ((b64 (base64-encode-string res t)))
          ;; sqlite3 command nullvalue assigned to 20 chars (19 + null).
          (substring b64 0 19))))

(defun sqlite3--temp-name (objects prefix)
  (loop with name = (concat prefix "_backup")
        with v = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        while (member name objects)
        do (setq name
                 (concat prefix
                         "_backup_"
                         (loop repeat 8
                               collect (aref v (random 62)) into res
                               finally return (concat res))))
        finally return name))

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

(defvar sqlite3-default-coding-system
  ;; TODO reconsider it
  ;; -> portable db.
  ;; -> temporary change coding-system
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
    (cons cs cs)))

(defconst sqlite3-prompt "sqlite> ")
(defconst sqlite3-continue-prompt "")

(defun sqlite3-prompt-p ()
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    ;; when executed sql contains newline, continue prompt displayed
    ;; before last prompt "sqlite> "
    (sqlite3-looking-at-prompt)))

;;TODO why continue prompt??
;; (looking-at "^\\( *\\.\\.\\.> \\)*sqlite> \\'")
;;TODO
(defconst sqlite3-prompt-regexp
  (concat "^"
          "\\( *"
          (regexp-quote sqlite3-continue-prompt)
          "\\)*"
          (regexp-quote sqlite3-prompt)
          "\\'"))

(defvar sqlite3--default-init-file nil)

(defun sqlite3--create-init-file ()
  (let ((file (make-temp-file "emacs-sqlite3-")))
    (with-temp-buffer
      (insert (format ".prompt \"%s\" \"%s\"\n"
                      sqlite3-prompt
                      sqlite3-continue-prompt))
      (write-region (point-min) (point-max) file nil 'no-msg))
    file))

;; To avoid user initialization file ~/.sqliterc
(defun sqlite3--default-init-file (&optional refresh)
  (or (and (not refresh) sqlite3--default-init-file)
      (setq sqlite3--default-init-file
            (sqlite3--create-init-file))))

(defmacro sqlite3--with-env (&rest form)
  `(let ((process-environment (copy-sequence process-environment))
         ;; non pty resulting in echoing query.
         (process-connection-type t)
         (default-process-coding-system sqlite3-default-coding-system))
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

;; TODO should accept signal when too many output?
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
       (format "Sqlite3 Error: %s" (match-string 1))))

;;TODO rename or some of caller function
(defun sqlite3--maybe-raise-syntax-error (proc)
  (while (and (eq (process-status proc) 'run)
              (not (sqlite3-prompt-p))
              (null (process-get proc 'sqlite3-syntax-error)))
    (sqlite3-sleep proc))
  (when (stringp (process-get proc 'sqlite3-syntax-error))
    (error "%s" (process-get proc 'sqlite3-syntax-error))))

(defconst sqlite3--signal-support-p
  (let* ((file (make-temp-file "emacs-sqlite3-"))
         (process (sqlite3-start-csv-process file "SELECT 1")))
    (unwind-protect
        (progn
          (and
           ;; suspend
           (eq (signal-process process 19) 0)
           ;; continue
           (eq (signal-process process 18) 0)))
      (delete-process process)
      (kill-buffer (process-buffer process))
      (delete-file file))))

;;
;; read csv from sqlite3
;;

(defun sqlite3--read-csv-line ()
  (let ((first (point))
        (line (pcsv-read-line)))
    ;; end of line is not a newline.
    ;; means ouput is progressing, otherwise prompt.
    (unless (bolp)
      (goto-char first)
      (signal 'invalid-read-syntax nil))
    line))

(defun sqlite3--read-csv-line-with-deletion (null)
  (let ((start (point))
        (row (sqlite3--read-csv-line)))
    (delete-region start (point))
    (loop for datum in row
          collect (if (equal datum null)
                      :null
                    datum))))

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
    (error "Not a supported type %s" object))))

(defvar sqlite3-format--table
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
    ;; some value (string, number, list)
    ("V" . sqlite3-format-value)
    ))

;; format directive is reference from sqlite C level api. (e.g. sqlite_mprintf)
;; suffix format is some alphabet char.
;;     lowcase todo
;;     upcase todo

;;TODO refine imp
;;;###autoload
(defun sqlite3-format (fmt &rest objects)
  "Prepare sql by FMT like `format' with extension.

FMT is a string or list of string.
 each list item join with newline.

Each directive accept arg which contain variable name.
e.g.
\(let ((some-var \"FOO\")) (sqlite3-format \"%s %s{some-var}\" \"HOGE\"))
 => \"HOGE FOO\"

%s: raw text (With no escape)
%t: escape text
%T: same as %t but with quote
%l: escape LIKE pattern escape char is '\\'
%L: same as %l but with quote and with ESCAPE statement
%o: escape db object with quote
%O: escape db objects with quote (joined by \", \")
%V: escape sql value(s) with quote if need
  (if list, joined by \", \" TODO about paren)
"
  (with-temp-buffer
    (when (listp fmt)
      (setq fmt (mapconcat 'identity fmt "\n")))
    (insert fmt)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (cond
       ((eq (char-after) ?%)
	(delete-char 1))
       ((looking-at "\\(?:\\([a-zA-Z]\\)\\)\\(?:{\\(.+?\\)}\\)?")
        (let* ((spec (match-string 1))
               (varname (intern-soft (match-string 2)))
	       (fn (assoc-default spec sqlite3-format--table))
               obj)
          ;; Delete formatter directive
          (delete-region (1- (point)) (match-end 0))
          (cond
           (varname
            (setq obj (symbol-value varname)))
           (objects
            (setq obj (car objects))
            (setq objects (cdr objects)))
           (t
            (error "No value assigned to `%%%s'" spec)))
	  (unless fn
	    (error "Invalid format character: `%%%s'" spec))
	  (unless (functionp fn)
            (error "Assert"))
          (let ((val obj)
                text)
            (setq text (funcall fn val))
            (insert text))))))
    (when objects
      (error "args out of range %s" objects))
    (buffer-string)))

(defun sqlite3-guess-type (text)
  (cond
   ((not (stringp text)) 'null)
   ;;TODO 1.e2
   ((string-match "\\`[0-9.]+\\'" text) 'number)
   (t 'text)))

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

;;TODO remove proc arg
(defun sqlite3-sleep (proc)
  ;; Other code affect to buffer while `sleep-for'.
  ;; TODO timer? filter? I can't get clue.
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
  "Convenient function to provide make quoted STRING in sql."
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
  (eq (process-status stream) 'run))

(defun sqlite3-stream-filename (stream)
  (process-get stream 'sqlite3-filename))

;;;###autoload
(defun sqlite3-stream-open (file)
  "Open FILE stream as sqlite3 database.
Optional NULLVALUE indicate text expression of NULL. This option may improve
the response of stream."
  (sqlite3-check-program)
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

(defun sqlite3-stream-close (stream)
  (unless (process-get stream 'sqlite3-stream-process-p)
    (error "Not a sqlite3 process"))
  (when (eq (process-status stream) 'run)
    (with-timeout (5 (kill-process stream))
      ;; DO NOT use `sqlite3-stream-send-command'
      (process-send-string stream ".quit\n")
      (let ((inhibit-redisplay t))
        (while (eq (process-status stream) 'run)
          (sqlite3-sleep stream)))))
  ;; delete process forcibly
  (delete-process stream))

(defun sqlite3-stream--filter (proc event)
  (sqlite3--with-parse proc event
    (let ((filter (process-get proc 'sqlite3-filter)))
      (when (functionp filter)
        (funcall filter proc)))))

(defun sqlite3-stream--sentinel (proc event)
  (sqlite3--with-process proc
    (unless (eq (process-status proc) 'run)
      (kill-buffer (current-buffer)))))

(defun sqlite3-stream--csv-filter (proc)
  (let* ((null (process-get proc 'sqlite3-null-value))
         (data (sqlite3--read-csv-with-deletion null))
         (accum (process-get proc 'sqlite3-accumulation)))
    (process-put proc 'sqlite3-accumulation
                 (append accum data))))

;;TODO high level api to control stream behavior by user.
(defun sqlite3-stream-send (stream query)
  (cond
   ((string-match "^[ \t\n]*\\." query)
    (process-send-string stream (concat query "\n"))
    ;;TODO check error?
    (sqlite3-stream--until-prompt stream)
    nil)
   (t
    (sqlite3-stream-send-sql stream query))))

(defun sqlite3-stream-send-command (stream command &rest args)
  "Send COMMAND and ARGS to STREAM without checking COMMAND error.
TODO You can call this function as a API. Not publish to program user.
"
  (let ((buf (process-buffer stream)))
    (with-current-buffer buf
      (sqlite3-stream--until-prompt stream)
      ;; clear all text contains prompt.
      (erase-buffer)
      (process-send-string
       stream
       (format "%s %s\n" command (mapconcat 'sqlite3-text args " ")))
      (sqlite3-stream--until-prompt stream)
      (goto-char (point-max))
      (buffer-substring (point-min) (point-at-eol 0)))))

(defun sqlite3-stream-send-sql (stream sql)
  "Send SQL to sqlite3 STREAM. (currently STREAM is a process object)
This function return after checking syntax error of SQL.

SQL is a sql statement that can ommit statement end (`;').
 Do Not send multiple statements.

Examples:
Good: SELECT * FROM table1;
Good: SELECT * FROM table1
Good: SELECT * FROM table1\n
 Bad: SELECT * FROM table1; SELECT * FROM table2;
"
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

(defalias 'sqlite3-stream-execute-sql 'sqlite3-stream-send-sql)

(defun sqlite3-stream-read-query (stream query)
  ;;TODO check stream alive or error more understandable (Deleting buffer)
  ;; TODO see `sqlite3-stream-send-sql'.
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
;;; Sqlite3 onetime stream
;;;

;;;###autoload
(defun sqlite3-async-read (file query filter &rest args)
  "Execute QUERY in sqlite3 FILE and immediately exit the sqlite3 process.
FILTER called with one arg that is parsed csv line or `:EOF'.
  Please use `:EOF' argument finish this async process.
  This FILTER is invoked in process buffer.

If QUERY contains syntax error, check the error result before first call of FILTER.
ARGS accept sqlite3 command arguments 
e.g. -header"
  (sqlite3-check-program)
  (unless (stringp query)
    (error "No query is provided"))
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
    (unless (memq (process-status proc) '(run signal))
      (let ((errmsg (process-get proc 'sqlite3-syntax-error))
            (filter (process-get proc 'sqlite3-filter)))
        (cond
         ;; ignore if error
         ((stringp errmsg))
         ((functionp filter)
          ;; If QUERY contains some error, `sqlite3--maybe-raise-syntax-error'
          ;; should report error before sentinel.
          (funcall filter :EOF))))
      (kill-buffer (current-buffer)))))

;;;
;;; Sqlite3 lazy reader
;;;

;;;###autoload
(defun sqlite3-reader-open (file query)
  ;; Open FILE with QUERY as sqlite3 database.
  ;; sqlite3-reader-* family can handle the return object.
  (let ((stream (sqlite3-stream-open file)))
    (sqlite3-stream-send-command stream ".header" "ON")
    (sqlite3-stream-send-sql stream query)
    (list :stream stream :results nil
          :position nil :fields nil)))

(defun sqlite3-reader-close (reader)
  (let ((stream (plist-get reader :stream)))
    (sqlite3-stream-close stream)))

(defun sqlite3-reader-open-p (reader)
  (let ((stream (plist-get reader :stream)))
    (sqlite3-stream-alive-p stream)))

(defun sqlite3-reader-read (reader)
  (let ((stream (plist-get reader :stream))
        (pos (plist-get reader :position))
        (results (plist-get reader :results))
        row)
    (unless (sqlite3-stream-alive-p stream)
      (error "Stream is closed"))
    (if (or
         ;; reader has not stepped yet.
         (null pos)
         ;; end of current reader
         (>= pos (length results)))
        (sqlite3-reader--step reader)
      ;; TODO should not make cache.. may have too many results
      (let* ((max (1- (length results)))
             (tidx (max (- max pos) 0)))
        (prog1
            (nth tidx results)
          (plist-put reader :position (1+ pos)))))))

(defun sqlite3-reader--step (reader)
  (let* ((stream (plist-get reader :stream))
         (pos (plist-get reader :position))
         (nullvalue (process-get stream 'sqlite3-null-value))
         row)
    (catch 'eof
      (with-current-buffer (process-buffer stream)
        ;; wait until output comes.
        (while (= (point-min) (point-max))
          (sqlite3-sleep stream))
        (goto-char (point-min))
        (when (sqlite3-looking-at-prompt)
          ;;TODO re-consider it!
          (sqlite3-reader-close reader)
          ;;TODO throw :EOF ??? like async-read
          (throw 'eof nil))
        (unless (plist-get reader :fields)
          ;; stock result of header
          (let ((fields (sqlite3--read-csv-line-with-deletion nullvalue)))
            (plist-put reader :fields fields))
          (plist-put reader :position 0)
          (setq pos 0))
        ;; read and delete it.
        (setq row (sqlite3--read-csv-line-with-deletion nullvalue))
        (plist-put reader :position (1+ pos))
        (let* ((old (plist-get reader :results))
               (new (cons row old)))
          (plist-put reader :results new)))
      row)))

(defun sqlite3-reader-seek (reader pos)
  (let ((res (plist-get reader :results)))
    (cond
     ((< (length res) pos)
      (while (and (sqlite3-reader-read reader)
                  ;; read to specified position
                  (< (plist-get reader :position) pos))))
     (t
      (plist-put reader :position pos)))
    (plist-get reader :position)))

(defun sqlite3-reader-peek (reader &optional pos)
  "Utility to read a row at POS or READER current position.
"
  (let* ((prev (plist-get reader :position))
         (pos
          (cond
           ((null pos)
            (plist-get reader :position))
           ((sqlite3-reader-open-p reader)
            ;; set new position to actual position
            (sqlite3-reader-seek reader pos))
           (t pos))))
    (unwind-protect
        (cond
         ((null pos)
          ;; reader has not yet opened.
          (sqlite3-reader-read reader))
         (t
          (let* ((res (plist-get reader :results))
                 (max (1- (length res)))
                 (tidx (- max pos)))
            (nth tidx res))))
      (sqlite3-reader-seek reader prev))))

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

;;
;; Sqlite3 onetime query
;;

;;;###autoload
(defun sqlite3-read (file query &rest args)
  "Read QUERY result in sqlite3 FILE.
This function designed with SELECT QUERY, but works fine another
 sql query (UPDATE/INSERT/DELETE)."
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
        (error "%s" (buffer-string)))
      (sqlite3--read-csv-with-deletion nullvalue))))

;;
;; Read object from sqlite3 database
;;

(defun sqlite3-read--objects (stream type)
  (let* ((query
          (sqlite3-format
           '("SELECT name "
             " FROM sqlite_master "
             " WHERE type = %T")
           type))
         (data (sqlite3-stream-read-query stream query)))
    (loop for row in data
          collect (car row))))

(defun sqlite3-read-views (stream)
  (sqlite3-read--objects stream "view"))

(defun sqlite3-read-tables (stream)
  (sqlite3-read--objects stream "table"))

(defun sqlite3-read-indexes (stream)
  (sqlite3-read--objects stream "index"))

(defun sqlite3-read-triggers (stream)
  (sqlite3-read--objects stream "trigger"))

(defun sqlite3-read-table-schema (stream table)
  "Get TABLE information in FILE.
Elements of the item list are:
0. cid
1. name with lowcase
2. type with UPCASE
3. not null (boolean)
4. default_value
5. primary key (boolean)"
  (loop for row in (sqlite3-stream-read-query
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
  (loop for (r1 col . ignore) in (sqlite3-file-table-schema file table)
        collect col))

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
        (error (message "Sqlite3: %s" err)))))
  (when (and (stringp sqlite3--default-init-file)
             (file-exists-p sqlite3--default-init-file))
    (delete-file sqlite3--default-init-file)))

(defun sqlite3-unload-function ()
  (remove-hook 'kill-emacs-hook 'sqlite3-killing-emacs))

(add-hook 'kill-emacs-hook 'sqlite3-killing-emacs)

;; TODO testing
(defun sqlite3-stream--reuse (file)
  (loop with filename = (expand-file-name file)
        for p in (process-list)
        if (and (sqlite3-stream-alive-p p)
                (string= (sqlite3-stream-filename p) filename))
        return p))

(provide 'sqlite3)

;;; sqlite3.el ends here
