;;; sqlite3.el --- sqlite3 file manipulate utilities

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: data
;; URL: http://github.com/mhayashi1120/sqlite3.el/raw/master/sqlite3.el
;; Emacs: GNU Emacs 24 or later
;; Version: 0.0.0
;; Package-Requires: ((pcsv "1.3.0"))

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
;; * windows (cygwin)
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
  :type 'file
  :group 'sqlite3)

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
;;; Basic utilities to handle sqlite3 i/o.
;;;

;;
;; initialization file
;;

(defun sqlite3--create-init-file (&optional commands)
  (let ((file (make-temp-file "emacs-sqlite3-")))
    (with-temp-buffer
      (dolist (com commands)
        (insert com "\n"))
      (write-region (point-min) (point-max) file nil 'no-msg))
    file))

(defvar sqlite3--default-init-file nil)

;; To avoid user initialization file ~/.sqliterc
(defun sqlite3--default-init-file (&optional refresh)
  (or (and (not refresh) sqlite3--default-init-file)
      (setq sqlite3--default-init-file
            (sqlite3--create-init-file))))

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

;;
;; handle constant
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

(defun sqlite3-prompt-p ()
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    ;; when executed sql contains newline, continue prompt displayed
    ;; before last prompt "sqlite> "
    (sqlite3-looking-at-prompt)))

(defun sqlite3-looking-at-prompt ()
  (looking-at "^\\( *\\.\\.\\.> \\)*sqlite> \\'"))

(defun sqlite3--read-syntax-error-at-point ()
  (and (looking-at "^Error: \\(.*\\)")
       (format "Sqlite3 Error: %s" (match-string 1))))

(defun sqlite3--maybe-raise-syntax-error (stream)
  (while (and (eq (process-status stream) 'run)
              (not (sqlite3-prompt-p))
              (null (process-get stream 'sqlite3-syntax-error)))
    (sqlite3-sleep stream))
  (when (stringp (process-get stream 'sqlite3-syntax-error))
    (error "%s" (process-get stream 'sqlite3-syntax-error))))

;;
;; process
;;

(defvar sqlite3-default-coding-system '(utf-8 . utf-8))

(defmacro sqlite3--with-env (&rest form)
  `(let ((process-environment (copy-sequence process-environment))
         ;; `pty' non pty resulting in echoing query.
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
   (let ((proc (apply 'start-process "Sqlite3 Stream" buffer
                      sqlite3-program args)))
     (process-put proc 'sqlite3-stream-process-p t)
     proc)))

(defun sqlite3-call-process (buffer &rest args)
  (sqlite3--with-env
   (apply 'call-process sqlite3-program nil buffer nil args)))

(defun sqlite3-start-csv-process (file query usernull &rest args)
  (let* ((init (sqlite3--default-init-file))
         (db (expand-file-name file))
         (nullvalue (or usernull (sqlite3--temp-null query)))
         (args `(
                 ,@(if query `("-batch") '("-interactive"))
                 "-init" ,init
                 "-csv"
                 "-nullvalue" ,nullvalue
                 ,@args
                 ,db ,@(and query `(,query))))
         (buf (sqlite3-stream--create-buffer))
         (proc (apply 'sqlite3-start-process buf args)))
    (process-put proc 'sqlite3-init-file init)
    (process-put proc 'sqlite3-filename db)
    (process-put proc 'sqlite3-user-null-value usernull)
    (process-put proc 'sqlite3-null-value nullvalue)
    proc))

(defun sqlite3-call-csv-process (file query nullvalue &rest args)
  (let* ((init (sqlite3--default-init-file))
         (db (expand-file-name file))
         (args `(
                 "-batch"
                 "-init" ,init
                 "-csv"
                 "-nullvalue" ,nullvalue
                 ,@args
                 ,db ,query)))
    (apply 'sqlite3-call-process (current-buffer) args)))

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
;; read csv from sqlite3
;;

(defun sqlite3--read-csv-with-deletion (null)
  "Read csv data from current point.
Delete csv data if reading was succeeded."
  (let (pcsv--eobp res)
    (condition-case nil
        (while (not (eobp))
          (let ((start (point))
                (row (sqlite3--read-csv-line)))
            (delete-region start (point))
            (setq res
                  (cons
                   (mapcar
                    (lambda (datum)
                      (if (equal datum null)
                          :null
                        datum))
                    row)
                   res))))
      ;; output is proceeding from process
      ;; finish the reading
      (invalid-read-syntax nil))
    (nreverse res)))

(defun sqlite3--read-csv-line ()
  (let ((first (point))
        (line (pcsv-read-line)))
    ;; end of line is not a newline.
    ;; means ouput is progressing, otherwise prompt.
    (unless (bolp)
      (goto-char first)
      (signal 'invalid-read-syntax nil))
    line))

;;
;; sqlite3 data (csv) <-> emacs data
;;

(defun sqlite3-format-object (object)
  ;;TODO some escape?
  (format "[%s]" object))

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
    (concat
     "("
     (mapconcat 'sqlite3-format-value object ", ")
     ")"))
   (t
    (error "Not a supported type %s" object))))

(defun sqlite3-quote-maybe (text)
  (case (sqlite3-guess-type text)
    (null "NULL")
    (number (sqlite3-format-value (string-to-number text)))
    (text (sqlite3-format-value text))
    (error "Not supported `%s'" text)))

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
  ;; This check calculate response of `fork' not a accepting from process output.
  ;; and sometime may delay fork the program.
  (apply 'min (loop for _ in '(1 2 3 4 5)
                    collect
                    (let ((start (float-time)))
                      (call-process "echo" nil nil nil "1")
                      (let ((end (float-time)))
                        (- end start))))))

;;TODO remove stream arg
(defun sqlite3-sleep (stream)
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
(defun sqlite3-stream-open (file &optional nullvalue)
  "Open FILE stream as sqlite3 database.
Optional NULLVALUE indicate text expression of NULL. This option may improve
the response of stream."
  (sqlite3-check-program)
  (when (and nullvalue 
             (> (length nullvalue) 19))
    (error "Null text too long"))
  (let* ((stream (sqlite3-start-csv-process file nil nullvalue)))
    (set-process-filter stream 'sqlite3-stream--filter)
    (set-process-sentinel stream 'sqlite3-stream--sentinel)
    (sqlite3-stream--wait stream)
    stream))

(defun sqlite3-stream-close (stream)
  (unless (process-get stream 'sqlite3-stream-process-p)
    (error "Not a sqlite3 process"))
  (when (eq (process-status stream) 'run)
    (with-timeout (5 (kill-process stream))
      (process-send-string stream ".quit\n")
      (while (eq (process-status stream) 'run)
        (sqlite3-sleep stream))))
  ;; delete process forcibly
  (delete-process stream))

(defun sqlite3-stream--create-buffer ()
  (generate-new-buffer " *Sqlite3 work* "))

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

(defun sqlite3-stream-invoke-query (stream query)
  ;;TODO check stream alive
  (let ((nullvalue (sqlite3--temp-null query)))
    ;; handling NULL text
    ;; Use different null text each time when executing query.
    (unless (process-get stream 'sqlite3-user-null-value)
      (sqlite3-stream--send-command
       stream (format ".nullvalue '%s'" nullvalue))
      (process-put stream 'sqlite3-null-value nullvalue))
    ;; send synchrounous variables.
    (process-put stream 'sqlite3-filter 'sqlite3-stream--csv-filter)
    (unwind-protect
        (progn
          ;; reset accumulate variable
          (process-put stream 'sqlite3-accumulation nil)
          (sqlite3-stream-execute-sql stream query)
          ;; wait until prompt is displayed.
          ;; filter function handling csv data.
          (sqlite3-stream--wait stream))
      (process-put stream 'sqlite3-filter nil))
    (process-get stream 'sqlite3-accumulation)))

;;TODO make async interface..
;; TODO describe about ((inhibit-redisplay t))
(defun sqlite3-stream-execute-query (stream query)
  "Get QUERY result from STREAM
TODO about redisplay
TODO see `sqlite3-stream-execute-sql'.
"
  (let ((inhibit-redisplay t))
    (sqlite3-stream-invoke-query stream query)))

(defun sqlite3-stream-set-option (stream command)
  (sqlite3-stream--send-command stream command))

(defun sqlite3-stream--send-command (stream command)
  "Send COMMAND to STREAM with omitting check the COMMAND error.
TODO You can call this function as a API. Not publish to program user.
"
  (let ((buf (process-buffer stream)))
    (with-current-buffer buf
      (sqlite3-stream--until-prompt stream)
      ;; clear all text contains prompt.
      (erase-buffer)
      (process-send-string stream command)
      (unless (string-match "\n\\'" command)
        (process-send-string stream "\n"))
      (sqlite3-stream--until-prompt stream)
      (goto-char (point-max))
      (buffer-substring (point-min) (line-end-position 0)))))

(defun sqlite3-stream-execute-sql (stream sql)
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
      (process-send-string stream sql)
      (cond
       ((not (string-match ";[ \t\n]*\\'" sql))
        (process-send-string stream ";\n"))
       ((not (string-match "\n\\'" sql))
        (process-send-string stream "\n")))
      ;; only check syntax error.
      ;; This check maybe promptly return from sqlite3 process.
      (sqlite3--maybe-raise-syntax-error stream)
      t)))

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
  (with-current-buffer (process-buffer stream)
    ;;TODO hide stream is process
    (sqlite3-prompt-p)))

;;;
;;; Sqlite3 onetime stream
;;;

;;;###autoload
(defun sqlite3-onetime-stream (file query filter &rest args)
  "Execute QUERY in sqlite3 FILE just one time.
FILTER called with one arg that is parsed csv line or `:EOF'.
  This FILTER is invoked in process buffer."
  (sqlite3-check-program)
  (let* ((stream (apply 'sqlite3-start-csv-process file query nil args)))
    (process-put stream 'sqlite3-filter filter)
    (set-process-filter stream 'sqlite3-onetime-stream--filter)
    (set-process-sentinel stream 'sqlite3-onetime-stream--sentinel)
    (sqlite3--maybe-raise-syntax-error stream)
    stream))

(defun sqlite3-onetime-stream--filter (proc event)
  (sqlite3--with-parse proc event
    (let ((filter (process-get proc 'sqlite3-filter)))
      (when (functionp filter)
        (let* ((null (process-get proc 'sqlite3-null-value))
               (data (sqlite3--read-csv-with-deletion null)))
          (dolist (row data)
            (funcall filter row)))))))

(defun sqlite3-onetime-stream--sentinel (proc event)
  (sqlite3--with-process proc
    (unless (eq (process-status proc) 'run)
      (let ((filter (process-get proc 'sqlite3-filter)))
        (when (functionp filter)
          (funcall filter :EOF)))
      (kill-buffer (current-buffer))
      ;;TODO consider using `sqlite3--default-init-file'
      (let ((init (process-get proc 'sqlite3-init-file)))
        (when (and init (file-exists-p init))
          (delete-file init))))))

;;;
;;; Sqlite3 lazy reader
;;;

;;;###autoload
(defun sqlite3-reader-open (file query)
  ;; Open FILE with QUERY as sqlite3 database.
  ;; sqlite3-reader-* family can handle the return object.
  (let* ((nullvalue (sqlite3--temp-null query))
         (stream (sqlite3-stream-open file nullvalue)))
    (sqlite3-stream-execute-sql stream query)
    ;; TODO :fields with-header option?
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
      (let* ((max (1- (length results)))
             (tidx (max (- max pos) 0)))
        (prog1
            (nth tidx results)
          (plist-put reader :position (1+ pos)))))))

(defun sqlite3-reader--step (reader)
  (let ((stream (plist-get reader :stream))
        (pos (plist-get reader :position))
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
          (throw 'eof nil))
        (unless (plist-get reader :fields)
          ;; stock result of header
          (let ((fields (sqlite3--read-csv-line)))
            (delete-region (point-min) (point))
            (plist-put reader :fields fields))
          (plist-put reader :position 0)
          (setq pos 0))
        ;; read and delete it.
        (setq row (sqlite3--read-csv-line))
        (delete-region (point-min) (point))
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
(defmacro sqlite3-call/stream (file func)
  "Open FILE as sqlite3 database.
FUNC accept just one arg created stream object from `sqlite3-stream-open'."
  (declare (indent 1) (debug t))
  `(let* ((inhibit-redisplay t)         ; like a synchronously function
          (stream (sqlite3-stream-open ,file)))
     (unwind-protect
         (funcall ,func stream)
       (sqlite3-stream-close stream))))

;;;###autoload
(defmacro sqlite3-call/transaction (file func)
  "Open FILE as sqlite3 database and begin/commit/rollback transaction.
FUNC accept just one arg created stream object from `sqlite3-stream-open'."
  (declare (indent 1) (debug t))
  `(sqlite3-call/stream ,file
     (lambda (stream)
       (sqlite3-stream-execute-sql stream "BEGIN")
       (condition-case err
           (prog1
               (funcall ,func stream)
             (sqlite3-stream-execute-sql stream "COMMIT"))
         (error
          ;; stream close automatically same as doing ROLLBACK.
          ;; but explicitly call this.
          (sqlite3-stream-execute-sql stream "ROLLBACK")
          (signal (car err) (cdr err)))))))

;;
;; Sqlite3 onetime query
;;

;;;###autoload
(defun sqlite3-onetime-query (file query &rest args)
  "Execute QUERY in sqlite3 FILE just one time.
FILTER called with one arg that is parsed csv line or `:EOF'."
  ;;TODO reconsider it
  (when (member "-nullvalue" args)
    (error "nullvalue: arg unacceptable"))
  (sqlite3-check-program)
  (let ((buf (sqlite3-stream--create-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (let* ((nullvalue (sqlite3--temp-null query))
                 (exit-code (apply 'sqlite3-call-csv-process file query nullvalue args)))
            (goto-char (point-min))
            (unless (eq exit-code 0)
              (let ((errmsg (sqlite3--read-syntax-error-at-point)))
                (when errmsg
                  (error "%s" errmsg)))
              (error "%s" (buffer-string)))
            (sqlite3--read-csv-with-deletion nullvalue)))
      (kill-buffer buf))))

;;TODO remove
(defun sqlite3-plist-merge (src dest)
  (loop for props on src by 'cddr
        do (let ((name (car props))
                 (val (cadr props)))
             (plist-put dest name val)))
  dest)

;;
;; Read object from sqlite3 database
;;

(defun sqlite3-read--objects (stream type)
  (let* ((query
          (format "SELECT name FROM sqlite_master WHERE type='%s'" type))
         (data (sqlite3-stream-invoke-query stream query)))
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
1. name
2. type
3. not null
4. default_value
5. primary key"
  (loop for row in (sqlite3-stream-invoke-query
                    stream (format "PRAGMA table_info(%s)" table))
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

(provide 'sqlite3)

;;; sqlite3.el ends here
