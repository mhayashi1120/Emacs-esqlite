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

(defgroup sqlite3 ()
  "TODO"
  )

(defcustom sqlite3-program "sqlite3"
  ""
  :type 'file
  :group 'sqlite3)

(defvar sqlite3-mode-hook nil)
(defun sqlite3-mode ()
  (interactive)
  (kill-all-local-variables)
  ;;TODO
  (run-mode-hooks 'sqlite3-mode-hook))

;;TODO
(defun sqlite3-send-query (query)
  (interactive "sSQL: ")
  (sqlite3--send-query query))

;;TODO
(defun sqlite3-forward-cursor-page ()
  (interactive)
  )

;;TODO
(defun sqlite3-backward-cursor-page ()
  (interactive)
  )

(defvar sqlite3-mode-map nil)

(unless sqlite3-mode-map
  (let ((map (make-sparse-keymap)))
    (setq sqlite3-mode-map map)))

(defvar sqlite3--buffer-process nil)
(make-variable-buffer-local 'sqlite3--buffer-process)

(defvar sqlite3--init-file nil)

(defun sqlite3--init-file ()
  (or sqlite3--init-file
      (setq sqlite3--init-file
            (sqlite3--create-init-file))))

(defun sqlite3--create-init-file ()
  (let ((file (make-temp-file "emacs-sqlite3-")))
    (with-temp-buffer
      (write-region (point-min) (point-max) file nil 'no-msg))
    file))

;;;
;;; Asynchronous function (for `sqlite3-mode')
;;;

(defun sqlite3--create-process-buffer ()
  (generate-new-buffer " *Sqlite3 work* "))

(defmacro sqlite3--with-process (proc &rest form)
  (declare (indent 1))
  `(let ((buf (process-buffer proc)))
     (when (buffer-live-p buf)
       (with-current-buffer buf
         ,@form))))

(defun sqlite3-format-text (text)
  (concat "\""
          (replace-regexp-in-string "\n" "\\\\n" text)
          "\""))

;;TODO
(defun sqlite3-insert-row (row)
  (mapc
   (lambda (v)
     (let ((start (point)))
       (insert (sqlite3-format-text (cdr v)))
       (put-text-property start (point) 'sqlite3-original v)))
   (cdr row))
  (insert "\n"))

(defvar sqlite3-filter-function nil)
(make-variable-buffer-local 'sqlite3-filter-function)

(defvar sqlite3-filter-error nil)
(make-variable-buffer-local 'sqlite3-filter-error)

(defvar sqlite3-filter-accumulation nil)
(make-variable-buffer-local 'sqlite3-filter-accumulation)

(defun sqlite3--process-filter (proc event)
  (sqlite3--with-process proc
    (goto-char (point-max))
    (insert event)
    (unless sqlite3-filter-error
      ;; check only first time filter received data.
      (setq sqlite3-filter-error 
            (or (sqlite3-filter-error-message) t)))
    (goto-char (point-min))
    (when (functionp sqlite3-filter-function)
      (funcall sqlite3-filter-function))))

(defun sqlite3--process-sentinel (proc event)
  (sqlite3--with-process proc
    (unless (eq (process-status proc) 'run)
      (kill-buffer (current-buffer)))))

(defun sqlite3--check-command ()
  "Check current buffer's database file is opend by sqlite."
  (unless (and sqlite3--buffer-process
               (eq (process-status sqlite3--buffer-process) 'run))
    (let* ((buf (sqlite3--create-process-buffer))
           (proc (start-process "sqlite3" buf
                                sqlite3-program
                                "-header"
                                "-init" (sqlite3--init-file)
                                "-csv" buffer-file-name)))
      (set-process-filter proc 'sqlite3--process-filter)
      (set-process-sentinel proc 'sqlite3--process-sentinel)
      (setq sqlite3--buffer-process proc))))

;; QUERY is a sql statement. Do Not have multiple statements.
;;
;; Good: SELECT * FROM table1; 
;; Bad:  SELECT * FROM table1; SELECT * FROM table2;
(defun sqlite3--send-query (query &optional filter)
  (sqlite3--check-command)
  (let* ((proc sqlite3--buffer-process)
         (buf (process-buffer proc)))
    (with-current-buffer buf
      (erase-buffer)
      (setq sqlite3-filter-function filter)
      (setq sqlite3-filter-accumulation nil)
      (setq sqlite3-filter-error nil)
      (process-send-string proc query)
      (process-send-string proc "\n")
      (while (and (eq (process-status proc) 'run)
                  (not (sqlite3--prompt-waiting-p proc)))
        (sit-for 0.1))
      (when (stringp sqlite3-filter-error)
        (error "%s" sqlite3-filter-error))
      sqlite3-filter-accumulation)))

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

(defun sqlite3--update-query (table rowid row)
  (format "UPDATE %s SET %s WHERE ROWID = %s;"
          table
          (mapconcat
           (lambda (x) (format "%s = %s" 
                               (car x) 
                               (sqlite3-to-escaped-string (cdr x))))
           row ", ")
          rowid))

(defun sqlite3--insert-query (table row)
  (format "INSERT INTO %s (%s) VALUES (%s);"
          table
          (mapconcat
           (lambda (x) (format "%s" (car x) ))
           row ", ")
          (mapconcat
           (lambda (x) (sqlite3-to-escaped-string (cdr x)))
           row ", ")))

(defun sqlite3--delete-query (table rowid)
  (format "DELETE FROM %s WHERE ROWID = %s;" rowid))

(defun sqlite3-filter-error-message ()
  (save-excursion
    (goto-char (point-min))
    (and (looking-at "^Error: \\(.*\\)")
         (format "Sqlite3 Error: %s" (match-string 1)))))

;;TODO order by 
;; TODO threshold of getting.
(defun sqlite3--select-query (table)
  (format "SELECT * FROM %s;" table))

(defun sqlite3--filter-select ()
  (let ((data (sqlite3--read-csv-with-delete)))
    (setq sqlite3-filter-accumulation
          (append sqlite3-filter-accumulation data))))
        

(defun sqlite3-open (table)
  (let* ((query (sqlite3--select-query table))
         (data (sqlite3--send-query query 'sqlite3--filter-select)))
    ;+TODO
    (princ data)))

;;;
;;; TODO
;;;

;;TODO magic-mode-alist
;;TODO
(defun sqlite3-file-guessed-as-valid-p (file)
  (with-temp-buffer
    (insert-file-contents file nil 0 256)
    (looking-at "SQLite format 3\000")))

(require 'pcsv)

(defun sqlite3--prompt-waiting-p (proc)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (forward-line 0)
      (sqlite3--prompt-line-p))))

(defun sqlite3--prompt-line-p ()
  (looking-at "^sqlite> \\'"))

(defun sqlite3--read-csv-with-delete ()
  (let ((pcsv-quoted-value-regexp  (pcsv-quoted-value-regexp))
        (pcsv-value-regexp (pcsv-value-regexp))
        pcsv-eobp res)
    (condition-case nil
        (while (not (eobp))
          (let ((start (point)))
            (let ((l (sqlite3--read-csv-line)))
              (delete-region start (point))
              (setq res (cons l res)))))
      (parse-error nil))
    (nreverse res)))

(defun sqlite3--read-csv-line ()
  (let ((first (point))
        (line (loop with v = nil
                    while (setq v (pcsv-read))
                    collect v
                    until (bolp))))
    (unless (memq (char-before) '(?\n ?\r))
      (goto-char first)
      (signal 'parse-error nil))
    line))

;;;
;;; Synchronous utilities
;;;

(defun sqlite3-db-select-from-table (file table)
  (sqlite3-select-from-file 
   file (format "SELECT * FROM %s;" table)))

(defun sqlite3-db-tables (file)
  (mapcar
   'car
   (sqlite3-select-from-file 
    file "SELECT name FROM sqlite_master WHERE type='table';")))

(defun sqlite3-db-table-columns (file table)
  (mapcar
   (lambda (r) (nth 1 r))
   (sqlite3-select-from-file
    file (format "PRAGMA table_info(%s);" table))))

(defun sqlite3--parse-all-buffer ()
  (require 'pcsv)
  (pcsv-parse-buffer))

(defun sqlite3-select-from-file (file select-query)
  (with-temp-buffer
    (unless (eq (call-process sqlite3-program nil t nil
                              "-init" (sqlite3--init-file)
                              "-csv" file select-query) 0)
      (error "Unable execute `%s' from `%s'" select-query file))
    (sqlite3--parse-all-buffer)))

(provide 'sqlite3)

;;; sqlite3.el ends here
