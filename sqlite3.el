;;; sqlite3.el --- TODO

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: sqlite3
;; URL: http://github.com/mhayashi1120/Emacs-sqlite3/raw/master/sqlite3.el
;; Emacs: TODO GNU Emacs 24 or later
;; Version: 0.0.1
;; Package-Requires: ((pcsv "1.1.0"))

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

;;; Install:

;;TODO

;;; Code:

;;TODO when table is locked

(require 'pcsv)

(defgroup sqlite3 ()
  "TODO"
  )

(defcustom sqlite3-program "sqlite3"
  ""
  :type 'file
  :group 'sqlite3)

(defvar sqlite3-mode-map nil)

(unless sqlite3-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-c" 'sqlite3-mode-toggle-display)
    (define-key map "\C-c\C-q" 'sqlite3-mode-send-query)
    (define-key map "\C-x\C-s" 'sqlite3-mode-commit-changes)
    (define-key map "\C-c>" 'sqlite3-mode-forward-page)
    (define-key map "\C-c<" 'sqlite3-mode-backward-page)
    ;TODO
    (define-key map "\C-c\C-f" 'sqlite3-mode-narrow-down)

    (setq sqlite3-mode-map map)))

(defvar sqlite3-mode--stream nil)
(make-variable-buffer-local 'sqlite3-mode--stream)

(defvar sqlite3-mode-hook nil)

(defun sqlite3-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sqlite3-mode)
  (setq mode-name "Sqlite3")
  (setq buffer-undo-list t)
  (setq truncate-lines t)
  (setq backup-inhibited t)
  ;;TODO
  (setq revert-buffer-function nil)
  (setq sqlite3-mode--current-page 0)
  (use-local-map sqlite3-mode-map)
  ;;TODO
  (run-mode-hooks 'sqlite3-mode-hook))

;;TODO
(defun sqlite3-mode-commit-changes ()
  (interactive)
  )

;;TODO
;; raw-data <-> table-view
(defun sqlite3-mode-toggle-display ()
  (interactive)
  )

;;TODO
(defun sqlite3-mode-send-query (query)
  (interactive "sSQL: ")
  (sqlite3-mode--send-query query))

(defun sqlite3-mode-forward-page ()
  "TODO"
  (interactive)
  (unless (sqlite3-mode-draw-data
           (1+ sqlite3-mode--current-page))
    (error "No more next page")))

(defun sqlite3-mode-backward-page ()
  "TODO"
  (interactive)
  (when (= sqlite3-mode--current-page 0)
    (error "This is a first page"))
  (unless (sqlite3-mode-draw-data
           (1- sqlite3-mode--current-page))
    (error "No more previous page")))

;;TODO name
(defun sqlite3-mode-narrow-down ()
  (interactive)
  ;;
  )

;;;
;;; `sqlite3-mode' functions
;;;

(defun sqlite3-mode--format-text (text)
  (pp-to-string text))

;;TODO
(defun sqlite3-mode--insert-row (row)
  ;; (car row) is ROWID
  (loop for v in (cdr row)
        for i from 0
        do (progn
             (when (> i 0)
               (insert " "))
             (let ((start (point)))
               (insert (sqlite3-mode--format-text v))
               (put-text-property start (point) 'sqlite3-original v))))
  (insert "\n"))

(defun sqlite3-mode-open-table (table)
  (interactive (let ((table
                      (completing-read
                       "Table: "
                       (mapcar
                        (lambda (x) (car x))
                        (sqlite3-mode-read-data sqlite3-select-table-query)))))
                 (list table)))
  (setq sqlite3-mode--current-table table)
  (sqlite3-mode-draw-data 0))

(defun sqlite3-mode--send-query (query)
  (sqlite3-mode--check-stream)
  (sqlite3-stream--send-query sqlite3-mode--stream query))

(defun sqlite3-mode--check-stream ()
  "Check current buffer's database file is opend by sqlite."
  (unless (and sqlite3-mode--stream
               (eq (process-status sqlite3-mode--stream) 'run))
    (setq sqlite3-mode--stream
          (sqlite3-stream-open buffer-file-name))))

(defun sqlite3-mode-read-data (query)
  (sqlite3-mode--check-stream)
  (sqlite3-stream--read-result sqlite3-mode--stream query))

(defun sqlite3-mode-draw-data (page)
  (save-excursion
    (let* ((where (or sqlite3-mode--current-cond "1 = 1"))
           (query (format 
                   "SELECT ROWID, * FROM %s WHERE %s LIMIT %s OFFSET %s * %s"
                   sqlite3-mode--current-table
                   where
                   sqlite3-mode--maximum
                   sqlite3-mode--maximum
                   page))
           (data (sqlite3-mode-read-data query)))
      (when data
        (let ((inhibit-read-only t))
          (erase-buffer)
          (mapc
           (lambda (row)
             (sqlite3-mode--insert-row row))
           data)
          (set-buffer-modified-p nil))
        (setq buffer-read-only t)
        (setq sqlite3-mode--current-page page)))))

(defvar sqlite3-mode--current-order nil)
(make-variable-buffer-local 'sqlite3-mode--current-order)

(defvar sqlite3-mode--current-cond nil)
(make-variable-buffer-local 'sqlite3-mode--current-cond)

(defvar sqlite3-mode--current-table nil)
(make-variable-buffer-local 'sqlite3-mode--current-page)

(defvar sqlite3-mode--current-page nil)
(make-variable-buffer-local 'sqlite3-mode--current-page)

(defvar sqlite3-mode--maximum 100)

(defun sqlite3--update-query (table columns rowid row)
  (format "UPDATE %s SET %s WHERE ROWID = %s;"
          table
          (mapconcat
           (lambda (x)
             (format "%s = %s"
                     (car x)
                     (sqlite3-to-escaped-string (cdr x))))
           (loop for c in columns
                 for datum in row
                 collect (cons c datum))
           ", ")
          rowid))

(defun sqlite3--insert-query (table columns row)
  (format "INSERT INTO %s (%s) VALUES (%s);"
          table
          (mapconcat 'identity columns ", ")
          (mapconcat
           (lambda (x) (sqlite3-to-escaped-string x))
           row ", ")))

(defun sqlite3--delete-query (table rowid)
  (format "DELETE FROM %s WHERE ROWID = %s;" rowid))

;;TODO order by
;; TODO threshold of getting.
(defun sqlite3--select-query (table)
  (format "SELECT * FROM %s;" table))

(defun sqlite3--csv-select-filter ()
  (let ((data (sqlite3--read-csv-with-deletion)))
    (setq sqlite3-stream--accumulation
          (append sqlite3-stream--accumulation data))))

(defun sqlite3--csv-select-sentinel ()
  ;;TODO
  ;; (let ((header (car sqlite3-stream--accumulation))
  ;;       (data (cdr sqlite3-stream--accumulation)))
  ;;   (setq sqlite3-stream--accumulation
  ;;         (mapcar
  ;;          (lambda (row)
  ;;            (loop for datum in row
  ;;                  for name in header
  ;;                  collect (cons name datum)))
  ;;          data)))
  )

;;;
;;; Sqlite3 stream
;;;

(defmacro sqlite3--with-process (proc &rest form)
  (declare (indent 1))
  `(let ((buf (process-buffer proc)))
     (when (buffer-live-p buf)
       (with-current-buffer buf
         ,@form))))

(defun sqlite3-stream-open (file)
  (let* ((buf (sqlite3-stream--create-buffer))
         (proc (start-process "Sqlite3 Stream" buf
                              sqlite3-program
                              "-header"
                              "-init" (sqlite3--init-file)
                              "-csv" file)))
    (process-put proc 'sqlite3-stream-process-p t)
    (set-process-filter proc 'sqlite3-stream--filter)
    (set-process-sentinel proc 'sqlite3-stream--sentinel)
    (sqlite3-stream--wait-until-prompt proc)
    proc))

(defun sqlite3-stream-close (stream)
  (let ((proc stream))
    (unless (process-get proc 'sqlite3-stream-process-p)
      (error "Not a sqlite3 process"))
    (delete-process stream)))

(defun sqlite3-stream--create-buffer ()
  (generate-new-buffer " *Sqlite3 work* "))

(defun sqlite3-stream--parse-error ()
  (save-excursion
    (goto-char (point-min))
    ;;FIXME this error output to stderr. Separate stderr to other file??
    ;;     Anxious to confuse stdout/stderr but rarely happens.
    (and (looking-at "^Error: \\(.*\\)")
         (format "Sqlite3 Error: %s" (match-string 1)))))

(defun sqlite3-stream--filter (proc event)
  (sqlite3--with-process proc
    (goto-char (point-max))
    (insert event)
    (unless sqlite3-stream--error
      ;; check only first time filter received data.
      (setq sqlite3-stream--error
            (or (sqlite3-stream--parse-error) t)))
    (goto-char (point-min))
    (when (functionp sqlite3-stream--filter-function)
      (funcall sqlite3-stream--filter-function))
    (when (sqlite3--prompt-waiting-p)
      (when (functionp sqlite3-stream--sentinel-function)
        (funcall sqlite3-stream--sentinel-function)))))

(defun sqlite3-stream--sentinel (proc event)
  (sqlite3--with-process proc
    (unless (eq (process-status proc) 'run)
      (kill-buffer (current-buffer)))))

(defvar sqlite3-stream--filter-function nil)
(defvar sqlite3-stream--sentinel-function nil)
(defvar sqlite3-stream--accumulation nil)

(defun sqlite3-stream--read-result (stream query)
  "Pass STREAM and QUERY to `sqlite3-stream--send-query'

TODO comment
This function return nil immediately unless FILTER and SENTINEL.
When previous QUERY is running, QUERY is waiting until QUERY was finished.

FILTER is called while receiving data from sqlite3 command.
SENTINEL is called when return from QUERY.
"
  ;; wait until previous query was finished.
  (sqlite3-stream--wait-until-prompt stream)
  ;; send synchrounous variables.
  (setq sqlite3-stream--filter-function 
        'sqlite3--csv-select-filter)
  ;;TODO
  (setq sqlite3-stream--sentinel-function nil)
  (setq sqlite3-stream--accumulation nil)
  (sqlite3-stream--send-query stream query)
  ;; wait until prompt is displayed.
  ;; filter function handling csv data.
  (sqlite3-stream--wait-until-prompt stream)
  sqlite3-stream--accumulation)

(defvar sqlite3-stream--error nil)
(make-variable-buffer-local 'sqlite3-stream--error)

(defun sqlite3-stream--send-query (stream query)
  "Send QUERY to sqlite3 STREAM. (currently STREAM is a process object)
This function check syntax error of QUERY.

QUERY is a sql statement. Do Not have multiple statements.
Good: SELECT * FROM table1;
 Bad: SELECT * FROM table1; SELECT * FROM table2;
"
  ;; wait until previous query was finished.
  (sqlite3-stream--wait-until-prompt stream)
  (let* ((proc stream)
         (buf (process-buffer proc)))
    (with-current-buffer buf
      ;; clear all text contains prompt.
      (erase-buffer)
      (setq sqlite3-stream--error nil)
      (process-send-string proc query)
      (cond
       ((not (string-match ";[ \t\n]*$" query))
        (process-send-string proc ";\n"))
       ((not (string-match "\n+" query))
        (process-send-string proc "\n")))
      ;; only check syntax error.
      (while (and (eq (process-status proc) 'run)
                  (not (sqlite3--prompt-waiting-p))
                  (null sqlite3-stream--error))
        (sit-for 0.1))
      (when (stringp sqlite3-stream--error)
        (error "%s" sqlite3-stream--error))
      t)))

(defun sqlite3-stream--wait-until-prompt (proc)
  (let ((buf (process-buffer proc)))
    (with-current-buffer buf
      (while (and (eq (process-status proc) 'run)
                  (not (sqlite3--prompt-waiting-p)))
        (sit-for 0.1)))))

;;;
;;; Utilities to handle any sqlite3 item.
;;;

(defvar sqlite3--init-file nil)

(defun sqlite3--init-file (&optional refresh)
  (or (and (not refresh) sqlite3--init-file)
      (setq sqlite3--init-file
            (sqlite3--create-init-file))))

(defun sqlite3--create-init-file ()
  (let ((file (make-temp-file "emacs-sqlite3-")))
    (with-temp-buffer
      (write-region (point-min) (point-max) file nil 'no-msg))
    file))

(defconst sqlite3-file-header-regexp "\\`SQLite format 3\000")

;;TODO
;; (add-to-list 'magic-mode-alist
;;              `(,sqlite3-file-header-regexp . sqlite3-mode))

(defun sqlite3-find-file (db-file)
  (interactive "FSqlite3 File: ")
  (unless (sqlite3-file-guessed-valid-p db-file)
    (error "Not a valid database file"))
  ;;TODO
  (let ((buf (get-file-buffer db-file)))
    (unless buf
      (setq buf (create-file-buffer (file-name-nondirectory db-file)))
      (with-current-buffer buf
        (set-visited-file-name db-file)
        (set-buffer-modified-p nil)))
    (switch-to-buffer buf)
    (sqlite3-mode)))

(defun sqlite3-file-guessed-valid-p (file)
  (with-temp-buffer
    (insert-file-contents file nil 0 256)
    (looking-at sqlite3-file-header-regexp)))

(defun sqlite3--prompt-waiting-p ()
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    (looking-at "^sqlite> \\'")))

(defun sqlite3--read-csv-with-deletion ()
  (let ((pcsv-quoted-value-regexp  (pcsv-quoted-value-regexp))
        (pcsv-value-regexp (pcsv-value-regexp))
        pcsv-eobp res)
    (condition-case nil
        (while (not (eobp))
          (let ((start (point)))
            (let ((l (sqlite3--read-csv-line)))
              (delete-region start (point))
              (setq res (cons l res)))))
      ;;TODO error is continue.
      (invalid-read-syntax nil))
    (nreverse res)))

(defun sqlite3--read-csv-line ()
  (let ((first (point))
        (line (loop with v = nil
                    while (setq v (pcsv-read))
                    collect v
                    until (bolp))))
    (unless (memq (char-before) '(?\n ?\r))
      (goto-char first)
      (signal 'invalid-read-syntax nil))
    line))

(defun sqlite3-to-escaped-string (object)
  (cond
   ((stringp object)
    (concat
     "'"
     (replace-regexp-in-string "'" "''" object)
     "'"))
   ((numberp object)
    (prin1-to-string object))
   (t
    (sqlite3-to-escaped-string (prin1-to-string object)))))

(defconst sqlite3-select-table-query
  "SELECT name FROM sqlite_master WHERE type='table'")

(defun sqlite3-stream--tables (stream)
  (mapcar
   'car
   (sqlite3-stream--read-result 
    stream sqlite3-select-table-query)))

(defun sqlite3-stream--table-info (stream table)
  "Get TABLE information in FILE.
Elements of the item list are:
0. cid
1. name
2. type
3. not null
4. default_value
5. primary key"
  (mapcar
   (lambda (row)
     (list
      (string-to-number (nth 0 row))
      (nth 1 row)
      (nth 2 row)
      (equal (nth 3 row) "1")
      (nth 4 row)
      (equal (nth 5 row) "1")))
   (cdr
    (sqlite3-stream--read-result 
     stream (format "PRAGMA table_info(%s)" table)))))

;;;
;;; Synchronous utilities
;;;

(defmacro sqlite3-with-db-stream (file stream-var &rest form)
  (declare (indent 2))
  `(let ((,stream-var (sqlite3-stream-open file)))
     (unwind-protect
         (progn ,@form)
       (sqlite3-stream-close ,stream-var))))

;; TODO order by where
(defun sqlite3-db-select-from-table (file table)
  (sqlite3-with-db-stream file stream
    (sqlite3-stream--read-result
     stream (format "SELECT * FROM %s" table))))

(defun sqlite3-db-tables (file)
  (sqlite3-with-db-stream file stream
    (sqlite3-stream--tables stream)))

(defun sqlite3-db-table-columns (file table)
  (mapcar
   (lambda (r) (nth 1 r))
   (sqlite3-db-table-info file table)))

(defun sqlite3-db-table-info (file table)
  "See `sqlite3-stream--table-info'"
  (sqlite3-with-db-stream file stream
    (sqlite3-stream--table-info stream table)))

(provide 'sqlite3)

;;TODO other file
(require 'ert)

(ert-deftest sqlite3-normal-0001 ()
  :tags '(sqlite3)
  (let* ((db (make-temp-file "sqlite3-test-"))
         (stream (sqlite3-stream-open db)))
    (unwind-protect
        (progn
          (should (sqlite3-stream--send-query stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY, text TEXT)"))
          (should (equal (sqlite3-db-table-columns db "hoge") '("id" "text")))
          (should (sqlite3-stream--send-query stream "INSERT INTO hoge VALUES (1, 'a')"))
          (should (sqlite3-stream--send-query stream "INSERT INTO hoge VALUES (2, 'b')"))
          (should (equal (sqlite3-stream--read-result
                          stream "SELECT * FROM hoge ORDER BY id")
                         '(("id" "text") ("1" "a") ("2" "b"))))
          (should (sqlite3-stream--send-query stream "UPDATE hoge SET id = id + 10, text = text || 'z'"))
          (should (equal
                   (sqlite3-stream--read-result stream "SELECT * FROM hoge")
                   '(("id" "text") ("11" "az") ("12" "bz")))))
      (sqlite3-stream-close stream))))


;;; sqlite3.el ends here
