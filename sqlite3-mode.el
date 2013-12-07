;;; sqlite3-mode.el --- sqlite3 file editing mode

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: data
;; URL: https://github.com/mhayashi1120/sqlite3.el/raw/master/sqlite3-mode.el
;; Emacs: GNU Emacs 24 or later
;; Version: 0.0.1
;; Package-Requires: ((sqlite3 "0.0.1"))

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

;;  This module provides view/edit/search sqlite3 database file like
;;  a normal file.

;;; Install:

;; (autoload 'sqlite3-find-file "sqlite3"
;;   "Open FILE as a Sqlite3 database" t)

;;; TODO:
;; * incremental search all data in table (by helm)
;; * to right side if number column.
;; * blob
;; * create hook (ex: commit, update delete...)
;; * clone row
;; * easy filter
;; * order by
;; * sqlite_temp_master

;; * sqlite3-mode
;;     table is valid -> schema -> read data
;; * sqlite3-mode
;;   tool menu, easy menu

;; * how to edit exclusively. DO NOT USE visited file time. that is nasty.
;; * multiple stream will be created too many revert. why??
;; * search query by SELECT\|UPDATE\|DELETE\|INSERT\|CREATE\|ALTER

;; * sqlite3-mode
;; ** mode
;;  - auto transaction (logging)
;;  - immediately (logging)
;;  - no change (logging)
;; ** start edit
;;  + start edit with C-x C-q
;;  + select edit mode
;;  + select logging mode (primary key / ROWID mode)

;; * logging sql
;;   - with exclusive cond. like .NET
;;     CREATE TABLE hoge (c1,c2,c3) a,b,NULL -> change A,NULL,C
;;     UPDATE hoge SET c1 = 'A', c2 = NULL, c3 = 'C' WHERE c1 = 'a' AND c2 = 'b' AND c3 IS NULL
;;   - with no exclusive cond.
;;     only primary key donot use ROWID
;; * stats and explain
;;    sqlite3 -stats file "select * from table"
;;    sqlite3 file "explain select * from table"

;; * SELECT ifnull((SELECT 1 WHERE changes() = 1), abs(-9223372036854775808));
;;   more refine way.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'sqlite3)
(require 'easymenu)

(defgroup sqlite3-mode ()
  "Manipulate sqlite3 database."
  :prefix "sqlite3-"
  ;; TODO change group?
  :group 'sqlite3)

;;;
;;; sqlite3-mode
;;;

;; external variables
(defvar quit-flag)
(defvar with-timeout-timers)
(defvar indent-tabs-mode)
(defvar revert-buffer-function)
(defvar last-command)
(defvar tool-bar-mode)
(defvar tool-bar-images-pixel-height)
(defvar menu-bar-mode)

(defcustom sqlite3-use-highlight-line t
  "Use `hl-line-mode' or not."
  :type 'boolean
  :group 'sqlite3-mode)

(defface sqlite3-header-background
  '((((type x w32 mac ns) (class color))
     :background "LightSteelBlue" :foreground "black")
    (((class color))
     (:background "white" :foreground "black")))
  "Face to fontify background of header line."
  :group 'sqlite3-mode)

(defface sqlite3-selected-face
  '((t (:inherit mode-line-highlight)))
  "Face for highlighting current cell."
  :group 'sqlite3-mode)

(defface sqlite3-error-line-face
  '((t (:inherit isearch-fail)))
  "Face for highlighting failed part in changing row."
  :group 'sqlite3-mode)

(defface sqlite3-null-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for any NULL."
  :group 'sqlite3-mode)

(defface sqlite3-mode-object-face
  '((t (:inherit font-lock-function-name-face)))
  "Face to fontify sqlite3 object."
  :group 'sqlite3-mode)

(defface sqlite3-mode-table-face
  '((t (:inherit sqlite3-mode-object-face)))
  "Face to fontify sqlite3 table."
  :group 'sqlite3-mode)

(defface sqlite3-mode-column-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face to fontify sqlite3 table column."
  :group 'sqlite3-mode)

(defvar sqlite3-mode-map nil)

(unless sqlite3-mode-map
  (let ((map (make-sparse-keymap)))

    (suppress-keymap map)

    (define-key map "\C-c!" 'sqlite3-mode-send-query)
    (define-key map "\C-c\C-q" 'sqlite3-mode-send-query)
    (define-key map "\C-x\C-s" 'sqlite3-mode-commit-changes)
    (define-key map "\C-c\C-r" 'sqlite3-mode-rollback)
    (define-key map "\C-c\C-c" 'sqlite3-mode-toggle-view)
    (define-key map "g" 'revert-buffer)

    (setq sqlite3-mode-map map)))

(defconst sqlite3-mode-menu-spec
  '(
    ["Toggle View Binary/Sqlite3" sqlite3-mode-toggle-view t]
    ["Discard Changes" sqlite3-mode-rollback t]
    ["Save Changes" sqlite3-mode-commit-changes t]
    ["Send Query" sqlite3-mode-send-query t]
    ["Revert Buffer" revert-buffer t]
    ))

(defcustom sqlite3-mode-hook nil
  "Hook called enter the `sqlite3-mode'."
  :group 'sqlite3-mode
  :type 'hook)

(defcustom sqlite3-mode-before-transaction-hook nil
  "Run before transaction is started."
  :group 'sqlite3-mode
  :type 'hook)

(defcustom sqlite3-mode-after-commit-hook nil
  "Run after transaction is commit. "
  :group 'sqlite3-mode
  :type 'hook)

(defcustom sqlite3-mode-after-rollback-hook nil
  "Run after transaction is rollback.
TODO safe-hook"
  :group 'sqlite3-mode
  :type 'hook)

(defcustom sqlite3-mode-after-transaction-hook nil
  "Run after transaction is finished. (commit / rollback)
TODO safe-hook"
  :group 'sqlite3-mode
  :type 'hook)

(defvar sqlite3-mode--context nil)
(make-variable-buffer-local 'sqlite3-mode--context)
(put 'sqlite3-mode--context 'permanent-local t)

(defvar sqlite3-table-mode--page-rows 100)

(defconst sqlite3-mode--cell-min-width 3)

(define-derived-mode sqlite3-mode fundamental-mode "Sqlite3"
  "Basic mode to manipulate sqlite3 database."
  (unless buffer-file-name
    (error "Not a file buffer"))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays (point-min) (point-max))
  (sqlite3-mode--new-context)
  (setq buffer-undo-list t)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq header-line-format nil)
  ;; to insert non tab space
  (setq indent-tabs-mode nil)
  (add-hook 'kill-buffer-hook
            'sqlite3-mode--after-kill-buffer nil t)
  (add-hook 'local-write-file-hooks
            'sqlite3-mode--write-buffer nil t)
  (when (fboundp 'hl-line-mode)
    (hl-line-mode -1))
  ;; disable creating #hoge.sqlite# file
  (set (make-local-variable 'backup-inhibited) t)
  (auto-save-mode -1)
  (sqlite3-mode-setup-mode-line)
  (run-mode-hooks 'sqlite3-mode-hook))

(defvar sqlite3-mode-read-query-history nil)

(defun sqlite3-mode-read-query ()
  (read-from-minibuffer "Query: " nil nil nil 'sqlite3-mode-read-query-history))

(defvar sqlite3-mode-read-table-history nil)

(defun sqlite3-mode--read-table-name ()
  (sqlite3-mode--check-stream)
  (let ((completion-ignore-case t))
    (completing-read
     "Table: "
     ;;TODO view
     (sqlite3-read-tables (sqlite3-mode-ref :stream))
     nil t nil 'sqlite3-mode-read-table-history)))

(defun sqlite3-mode--new-context ()
  (unless sqlite3-mode--context
    (setq sqlite3-mode--context
          (list
           :stream nil
           :transaction nil
           :schemaview nil))))

(defvar sqlite3-mode-debug nil)
(defvar sqlite3-mode--debug-buffer "*Sqlite3 Debug*")

(defun sqlite3-mode-log (fmt &rest args)
  (when sqlite3-mode-debug
    (with-current-buffer (get-buffer-create sqlite3-mode--debug-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (apply 'format fmt args) "\n")))))

(defun sqlite3-mode-ref (key)
  (plist-get sqlite3-mode--context key))

(defun sqlite3-mode-set (key value)
  (plist-put sqlite3-mode--context key value))

;;TODO
(defun sqlite3-mode--write-buffer ()
  (message "Not saved (Not yet implemented)")
  t)

(defun sqlite3-mode--after-kill-buffer ()
  (when (sqlite3-mode-ref :stream)
    (sqlite3-stream-close (sqlite3-mode-ref :stream)))
  (when (sqlite3-mode-ref :schemaview)
    (kill-buffer (sqlite3-mode-ref :schemaview)))
  (sqlite3-table-mode--cleanup-timer))

;;TODO separate mode-line-format each mode
(defun sqlite3-mode-setup-mode-line ()
  (setq mode-line-process
        '((:eval
           (when (and (eq major-mode 'sqlite3-table-mode)
                      (sqlite3-table-mode-ref :source :name))
             (concat " ["
                     (propertize (sqlite3-table-mode-ref :source :name)
                                 'face 'sqlite3-mode-table-face)
                     ":"
                     ;;TODO consider this. too many execution.
                     (let ((max (sqlite3-table-mode-ref :cache :max-page)))
                       (if (and (numberp max) (zerop max))
                           "(No Data)"
                         (format "%d/%s"
                                 (1+ (sqlite3-table-mode-ref :source :view :page))
                                 (or max "Unknown"))))
                     "]")))
          (:eval
           (concat
            " "
            (let ((status (sqlite3-mode-stream-status)))
              (cond
               ((eq status 'prompt)
                (propertize "Run" 'face 'minibuffer-prompt))
               ((eq status 'transaction)
                (propertize "Transaction" 'face 'compilation-info))
               ((eq status 'querying)
                (propertize "Querying" 'face 'compilation-warning))
               (t
                (propertize "No Process" 'face 'shadow)))))))))

(defun sqlite3-mode--to-text (datum)
  (cond
   ((stringp datum)
    (let ((oneline (or
                    (and (string-match "^\\([^\n]+\\)" datum)
                         (match-string 1 datum))
                    datum)))
      (replace-regexp-in-string "\t" "\\\\t" oneline)))
   ((eq datum :null) "null")
   (t "")))

(defun sqlite3-mode--calculate-width (data)
  ;; decide list length by header line
  (loop with all-width = (make-list (length (car data))
                                    sqlite3-mode--cell-min-width)
        for row in data
        do (loop for datum in row
                 for pair on all-width
                 for idx from 0
                 do (let* ((text (sqlite3-mode--to-text datum))
                           (wid (string-width text)))
                      (setcar pair (max (car pair) wid))))
        finally return all-width))

(defun sqlite3-mode-property-region (point property &optional restrict-line)
  (let ((val (get-text-property point property)))
    (and val
         (let* ((start (previous-single-property-change
                        point property
                        nil (or (and restrict-line (point-at-bol))
                                (point-min))))
                (end (next-single-property-change
                      point property
                      nil (or (and restrict-line (point-at-eol))
                              (point-max)))))
           (unless (eq (get-text-property start property) val)
             (setq start (point)))
           (unless (eq (get-text-property (1- end) property) val)
             (setq end (point)))
           (cons start end)))))

(defun sqlite3-mode--propertize-background-header (string)
  (let ((end (length string)))
    (add-text-properties 0 end
                         `(
                           face sqlite3-header-background
                                tab-separator t)
                         string)
    string))

(defun sqlite3-mode-open-table (table)
  (sqlite3-mode--check-stream)
  (sqlite3-table-mode)
  (sqlite3-table-mode--load-schema table)
  (let ((src-view (copy-sequence (sqlite3-table-mode-ref :source :view))))
    (sqlite3-table-mode--draw-page src-view)
    ;; redraw header forcibly
    (sqlite3-table-mode--delayed-draw-header)))

(defun sqlite3-mode--send (query)
  (sqlite3-mode--check-stream)
  (let ((stream (sqlite3-mode-ref :stream)))
    (sqlite3-stream-send stream query)))

(defun sqlite3-mode--execute-sql (sql)
  (sqlite3-mode--check-stream)
  (let ((stream (sqlite3-mode-ref :stream)))
    (sqlite3-stream-execute stream sql)))

(defun sqlite3-mode--check-stream ()
  "Check current buffer's database file is opend by sqlite."
  (let ((stream (sqlite3-mode-ref :stream)))
    (unless (and stream
                 (sqlite3-stream-alive-p stream))
      (when stream
        (sqlite3-stream-close stream))
      (setq stream (sqlite3-stream-open buffer-file-name))
      (sqlite3-mode-set :stream stream))))

(defun sqlite3-mode-redraw-page ()
  (cond
   ((eq major-mode 'sqlite3-table-mode)
    (sqlite3-table-mode-redraw-page))
   ((eq major-mode 'sqlite3-schema-mode)
    (sqlite3-mode-open-schema-mode t))
   (t (error "Not supported in this mode"))))

(defun sqlite3-mode-query (query)
  (sqlite3-mode--check-stream)
  (let* ((stream (sqlite3-mode-ref :stream))
         (data (sqlite3-stream-read stream query)))
    data))

(defun sqlite3-mode-stream-status ()
  (let ((stream (sqlite3-mode-ref :stream)))
    (cond
     ((null stream) 'exit)
     ((not (sqlite3-stream-alive-p stream)) 'exit)
     ((sqlite3-stream-prompt-p stream)
      (cond
       ((sqlite3-mode-ref :transaction)
        'transaction)
       (t
        'prompt)))
     ;;TODO continue prompt
     (t 'querying))))

(defun sqlite3-mode--transaction-begin ()
  (run-hooks 'sqlite3-mode-before-transaction-hook)
  (sqlite3-mode--execute-sql "BEGIN")
  (sqlite3-mode-set :transaction t))

(defun sqlite3-mode--transaction-rollback ()
  (sqlite3-mode--execute-sql "ROLLBACK")
  (sqlite3-mode-set :transaction nil)
  (sqlite3-mode-safe-run-hooks 'sqlite3-mode-after-transaction-hook)
  (sqlite3-mode-safe-run-hooks 'sqlite3-mode-after-rollback-hook))

(defun sqlite3-mode--transaction-commit ()
  (sqlite3-mode--execute-sql "COMMIT")
  (sqlite3-mode-set :transaction nil)
  (sqlite3-mode-safe-run-hooks 'sqlite3-mode-after-transaction-hook)
  (sqlite3-mode-safe-run-hooks 'sqlite3-mode-after-commit-hook))

(defun sqlite3-mode-safe-run-hooks (hook)
  (dolist (f (and (boundp hook)
                  (symbol-value hook)))
    (condition-case err
        (funcall f)
      (error
       (sqlite3-table-mode--message "Sqlite3 Error: %s" err)
       (sit-for 0.5)))))

;;TODO consider `local-write-file-hooks'
(defun sqlite3-mode-commit-changes ()
  "Commit changes to database file.
If changed data violate database constraint, transaction will be rollback.
"
  (interactive)
  (sqlite3-mode--before-transit-row)
  (unless (eq (sqlite3-mode-stream-status) 'transaction)
    (error "Hav not began a transaction"))
  (when (y-or-n-p "Commit all changes? ")
    (sqlite3-mode--transaction-commit)
    ;; sync with physical data.
    (sqlite3-mode-redraw-page)))

(defun sqlite3-mode-rollback ()
  "Discard all changes."
  (interactive)
  (unless (eq (sqlite3-mode-stream-status) 'transaction)
    (error "Have not began a transaction"))
  (unless (y-or-n-p "Really discard changes? ")
    (signal 'quit nil))
  (sqlite3-mode--transaction-rollback)
  (sqlite3-mode-redraw-page))

(defun sqlite3-mode-reset ()
  "Cause of unknown problem of editing buffer, reset sqlite3 command stream
if you want."
  (interactive)
  (unless (y-or-n-p "Restart sqlite3 process with discarding changes? ")
    (signal 'quit nil))
  (when (sqlite3-mode-ref :stream)
    (sqlite3-stream-close (sqlite3-mode-ref :stream)))
  (sqlite3-mode--check-stream)
  (sqlite3-mode-redraw-page))

(defun sqlite3-mode-send-query (sql-or-command)
  "Send SQL-OR-COMMAND to sqlite3 process.
Do not send compound statement or containing comment sql.
 This make unknown fatal result ;-)"
  (interactive "sSQL: ")
  (sqlite3-mode--before-transit-row)
  (let ((result (sqlite3-mode--send sql-or-command)))
    (cond
     ((stringp result)
      (message "%s" result))
     (t
      (sqlite3-mode-redraw-page)))))

(defun sqlite3-mode-open-schema-mode (&optional force)
  "Show schema view of current buffer file."
  (interactive)
  (sqlite3-mode--before-transit-row)
  (sqlite3-schema-mode)
  (sqlite3-mode--check-stream)
  (cond
   (force
    (sqlite3-schema-mode-draw-view))
   ((sqlite3-schema-mode--restore-from-evacuation))
   (t
    (sqlite3-schema-mode-draw-view)))
  (set-buffer-modified-p nil))

;;;
;;; sqlite3-table-mode
;;;

(defvar sqlite3-table-mode-map nil)

(unless sqlite3-table-mode-map

  (let ((map (make-sparse-keymap)))

    (set-keymap-parent map sqlite3-mode-map)

    (define-key map "\C-c\C-a" 'sqlite3-table-mode-add-row)
    (define-key map "\C-c\C-d" 'sqlite3-table-mode-delete-row)
    (define-key map "\C-c\C-k" 'sqlite3-table-mode-shrink-column)
    (define-key map "\C-c\C-l" 'sqlite3-table-mode-widen-column)
    (define-key map "\C-c\C-j" 'sqlite3-table-mode-jump-to-page)
    (define-key map "\C-c\ew" 'sqlite3-table-mode-copy-cell)
    (define-key map "\C-c\C-y" 'sqlite3-table-mode-paste-cell)
    (define-key map "\C-c>" 'sqlite3-table-mode-forward-page)
    (define-key map "\C-c<" 'sqlite3-table-mode-backward-page)
    ;;TODO
    ;; (define-key map "\C-c\e>" 'sqlite3-table-mode-last-page)
    ;; (define-key map "\C-c\e<" 'sqlite3-table-mode-first-page)
    (define-key map "F" 'sqlite3-table-mode-forward-page)
    (define-key map "B" 'sqlite3-table-mode-backward-page)
    (define-key map "\C-i" 'sqlite3-table-mode-forward-cell)
    (define-key map "\e\C-i" 'sqlite3-table-mode-backward-cell)
    (define-key map "\C-m" 'sqlite3-table-mode-view-cell)
    (define-key map "e" 'sqlite3-table-mode-start-edit)
    (define-key map "v" 'sqlite3-table-mode-view-cell)
    (define-key map "h" 'sqlite3-table-mode-previous-column)
    (define-key map "l" 'sqlite3-table-mode-next-column)
    (define-key map "k" 'sqlite3-table-mode-previous-row)
    (define-key map "j" 'sqlite3-table-mode-next-row)
    (define-key map "S" 'sqlite3-mode-open-schema-mode)

    (define-key map "\C-c\eC" 'sqlite3-table-mode-clone-row)

    ;; TODO keybinding `#' `%'
    (define-key map "#" (make-sparse-keymap))
    (define-key map "#s" 'sqlite3-table-mode-easy-sort)
    (define-key map "#c" 'sqlite3-table-mode-clear-sort)
    (define-key map "%" (make-sparse-keymap))
    (define-key map "%f" 'sqlite3-table-mode-easy-filter)
    (define-key map "%c" 'sqlite3-table-mode-clear-filter)
    (define-key map "%g" 'sqlite3-table-mode-helm-glob-search)
    (define-key map "%l" 'sqlite3-table-mode-helm-like-search)
    
    (setq sqlite3-table-mode-map map)))

(defconst sqlite3-table-mode-menu-spec
  `("Sqlite3"
    ,@sqlite3-mode-menu-spec
    ["Schema View" sqlite3-mode-open-schema-mode t]
    ["Cell View" sqlite3-table-mode-view-cell t]
    "---"
    ("Edit"
     ["Edit Cell" sqlite3-table-mode-start-edit t]
     ["Add Row" sqlite3-table-mode-add-row t]
     ["Delete Row" sqlite3-table-mode-delete-row t]
     ["Copy Value" sqlite3-table-mode-copy-cell t]
     ["Paste Value" sqlite3-table-mode-paste-cell t]
     )
    ("Paging"
     ["Forward Page" sqlite3-table-mode-forward-page t]
     ["Backward Page" sqlite3-table-mode-backward-page t]
     ["Jump to Page" sqlite3-table-mode-jump-to-page t]
     )
    ("Display"
     ["Shrink Column" sqlite3-table-mode-shrink-column t]
     ["Widen Column" sqlite3-table-mode-widen-column t]
     ["Sort" sqlite3-table-mode-easy-sort t]
     ["Clear Sort" sqlite3-table-mode-clear-sort t]
     ["Filter" sqlite3-table-mode-easy-filter t]
     ["Clear Filter" sqlite3-table-mode-clear-filter t]
     ["GLOB Search (helm)" sqlite3-table-mode-helm-glob-search t]
     ["LIKE Search (helm)" sqlite3-table-mode-helm-helm-search t]
     )
    ("Cursor"
     ["Forward Cell" sqlite3-table-mode-forward-cell t]
     ["Backward Cell" sqlite3-table-mode-backward-cell t]
     ["Previous Column" sqlite3-table-mode-previous-column t]
     ["Next Column" sqlite3-table-mode-next-column t]
     ["Previous Row" sqlite3-table-mode-previous-row t]
     ["Next Row" sqlite3-table-mode-next-row t]
     )))

(easy-menu-define sqlite3-table-mode-menu
  sqlite3-table-mode-map
  "Menu used in Sqlite3 table mode."
  sqlite3-table-mode-menu-spec)

(defvar sqlite3-table-mode--context nil)
(make-variable-buffer-local 'sqlite3-table-mode--context)

(defvar sqlite3-table-mode--popup-timer nil)

(defvar sqlite3-table-mode--highlight-overlay nil)
(make-variable-buffer-local 'sqlite3-table-mode--highlight-overlay)

(define-derived-mode sqlite3-table-mode sqlite3-mode "Sqlite3 Table"
  "Sqlite3 table view mode"
  ;;TODO clear when schema-view
  (set (make-local-variable 'revert-buffer-function)
       'sqlite3-table-mode-revert)
  (add-hook 'post-command-hook
            'sqlite3-table-mode--post-command nil t)
  (add-hook 'pre-command-hook
            'sqlite3-table-mode--pre-command nil t)
  (cond
   ((null sqlite3-use-highlight-line))
   ((require 'hl-line nil t)
    (hl-line-mode 1))
   (t
    ;; forcibly disable the customize variable
    (setq sqlite3-use-highlight-line nil)))
  (sqlite3-table-mode--new-context)
  (use-local-map sqlite3-table-mode-map)
  (unless sqlite3-table-mode--popup-timer
    (setq sqlite3-table-mode--popup-timer
          (run-with-idle-timer
           1 t 'sqlite3-table-mode--timer-popup-contents))))

(put 'sqlite3-table-mode 'mode-class 'special)

(defun sqlite3-table-mode-view-cell ()
  "View current cell with opening subwindow."
  (interactive)
  (let ((value (sqlite3-table-mode-current-value)))
    (unless value
      (error "No cell is here"))
    (let* ((type (sqlite3-table-mode-current-type))
           (buf (sqlite3-table-mode--create-cell-buffer value type)))
      (display-buffer buf))))

;;TODO
(defun sqlite3-table-mode-clone-row ()
  (interactive)
  (let ((source-row (get-text-property (point) 'sqlite3-mode-row)))
    (unless source-row
      (error "No row is here"))
    (forward-line 0)
    (let* ((row (cons nil (mapcar
                           (lambda (cell)
                             (sqlite3-table-mode--cell-value cell))
                           (plist-get source-row :cells))))
           (inhibit-read-only t))
      (insert "\n")
      (forward-line -1)
      ;;TODO sqlite3-table-mode--insert-row set :source-value non-nil
      (sqlite3-table-mode--insert-row row t)
      (forward-line 0))))

(defun sqlite3-table-mode-add-row ()
  "Add new row after the cursor."
  (interactive)
  ;;TODO need?
  ;; (sqlite3-mode--before-transit-row)
  (when sqlite3-table-mode--highlight-overlay
    (move-overlay sqlite3-table-mode--highlight-overlay
                  (point-max) (point-max)))
  (forward-line 0)
  (let* ((num (length (sqlite3-table-mode-ref :view :columns)))
         (row (cons nil (make-list num nil)))
         (inhibit-read-only t))
    (insert "\n")
    (forward-line -1)
    (sqlite3-table-mode--insert-row row)
    (forward-line 0)))

(defun sqlite3-table-mode-delete-row ()
  "Delete current row."
  (interactive)
  (unless (y-or-n-p "Really delete this row? ")
    (signal 'quit nil))
  (unless (eq (sqlite3-mode-stream-status) 'transaction)
    (sqlite3-mode--transaction-begin))
  (sqlite3-table-mode--update-with-handler
   (let* ((row (sqlite3-table-mode--convert-row
                (get-text-property (point) 'sqlite3-mode-row)))
          (sql (sqlite3-table-mode--delete-sql row)))
     (message "Deleting...")
     (sqlite3-mode--execute-sql sql)))
  (let ((inhibit-read-only t))
    (delete-region (point-at-bol)
                   (point-at-bol 2))))

(defvar sqlite3-table-mode--killed nil)
(defun sqlite3-table-mode-copy-cell ()
  "Copy cell."
  (interactive)
  (let ((cell (get-text-property (point) 'sqlite3-mode-cell)))
    (unless cell
      (user-error "No cell is here"))
    (let* ((value (sqlite3-table-mode--cell-value cell))
           (text (or (and (stringp value) value) "")))
      ;; To save the :null value use the elisp variable.
      (setq sqlite3-table-mode--killed value)
      (kill-new text)))
  (message "Current cell is saved."))

(defun sqlite3-table-mode-paste-cell ()
  "Insert kill-ring to current cell."
  (interactive)
  (sqlite3-table-mode--call/edit-cell
   (lambda (cell)
     (let ((text (current-kill 0)))
       (if (and (equal text "")
                (eq sqlite3-table-mode--killed :null))
           :null
         text)))))

(defun sqlite3-table-mode-start-edit ()
  "Edit current cell with opening subwindow."
  (interactive)
  (when (plusp (recursion-depth))
    (user-error
     "%s"
     (substitute-command-keys
      (concat
       "Other recursive edit is working. "
       "Type \\[abort-recursive-edit] to quit previous recursive edit"))))
  (sqlite3-table-mode--call/edit-cell
   (lambda (cell)
     (let ((value (sqlite3-table-mode--cell-value cell))
           (type (sqlite3-table-mode-current-type)))
       (sqlite3-table-mode-open-edit-window value type)))))

(defun sqlite3-table-mode-shrink-column (arg)
  "Shrink current column"
  (interactive "p")
  (sqlite3-table-mode--resize-column (* -1 arg)))

(defun sqlite3-table-mode-widen-column (arg)
  "Widen current column"
  (interactive "p")
  (sqlite3-table-mode--resize-column arg))

(defun sqlite3-table-mode-jump-to-page (page)
  "Jump to selected PAGE"
  (interactive
   (list (read-number "Page: ")))
  (sqlite3-mode--before-transit-row)
  (sqlite3-table-mode--move-page
   (1- page) "No such page"))

(defun sqlite3-table-mode-last-page (arg)
  "Jump to last page."
  (interactive "p")
  (sqlite3-mode--before-transit-row)
  ;;TODO
  (error "Not yet supported")
  ;; (sqlite3-table-mode--move-page
  ;;  (+ (sqlite3-table-mode-ref :source :view :page) arg)
  ;;  "No more next page")
  )

(defun sqlite3-table-mode-forward-page (arg)
  "Forward page."
  (interactive "p")
  (sqlite3-mode--before-transit-row)
  (sqlite3-table-mode--move-page
   (+ (sqlite3-table-mode-ref :source :view :page) arg)
   "No more next page"))

(defun sqlite3-table-mode-backward-page (arg)
  "Backward page."
  (interactive "p")
  (when (= (sqlite3-table-mode-ref :source :view :page) 0)
    (user-error "This is a first page"))
  (sqlite3-mode--before-transit-row)
  (sqlite3-table-mode--move-page
   (- (sqlite3-table-mode-ref :source :view :page) arg)
   "No more previous page"))

(defun sqlite3-table-mode-next-row (arg)
  "Goto next line of row."
  (interactive "p")
  (sqlite3-table-mode--move-line arg))

(defun sqlite3-table-mode-previous-row (arg)
  "Goto previous line of row."
  (interactive "p")
  (sqlite3-table-mode--move-line (- arg)))

(defun sqlite3-table-mode-next-column ()
  "Goto next column."
  (interactive)
  (let ((next (sqlite3-table-mode--next-cell t)))
    (when next
      (goto-char next))))

(defun sqlite3-table-mode-previous-column ()
  "Goto previous column."
  (interactive)
  (let ((prev (sqlite3-table-mode--previous-cell (point) t)))
    (when prev
      (goto-char prev))))

(defun sqlite3-table-mode-forward-cell ()
  "Forward cell over the row."
  (interactive)
  (let ((next (sqlite3-table-mode--next-cell)))
    ;;TODO paging
    (cond
     (next
      (goto-char next)))))

(defun sqlite3-table-mode-backward-cell ()
  "Backward cell over the row."
  (interactive)
  (let ((prev (sqlite3-table-mode--previous-cell (point))))
    ;;TODO paging
    (cond
     (prev
      (goto-char prev)))))

(defun sqlite3-table-mode-easy-sort (&optional desc)
  (interactive "P")
  (sqlite3-mode--before-transit-row)
  (let ((cell (get-text-property (point) 'sqlite3-mode-cell)))
    (unless cell
      (user-error "No column is here"))
    (let* ((src-view (copy-sequence (sqlite3-table-mode-ref :source :view)))
           (column (plist-get cell :column))
           (orders (plist-get src-view :orders))
           (name (plist-get column :name))
           (term (if desc :desc :asc))
           (rowid (sqlite3-table-mode-current-rowid)))
      ;;TODO show in header-format like tabulated-mode
      (let ((order (assoc name orders)))
        (unless order
          (setq order (list name))
          (setq orders (append orders (list order))))
        (setcdr order (list term)))
      (plist-put src-view :orders orders)
      (sqlite3-table-mode--draw-page src-view)
      (sqlite3-table-mode--goto-column name))))

(defun sqlite3-table-mode-clear-sort (&optional all)
  (interactive "P")
  (sqlite3-mode--before-transit-row)
  ;;TODO currently clear all
  (sqlite3-table-mode-set nil :source :view :orders)
  (sqlite3-table-mode-redraw-page))

;;TODO immitate excel autofill
;;TODO consider cell datatype
;;TODO compare > < <= >=
(defun sqlite3-table-mode-easy-filter ()
  ""
  (interactive)
  (sqlite3-mode--before-transit-row)
  (let ((cell (get-text-property (point) 'sqlite3-mode-cell)))
    (unless cell
      (user-error "No column is here"))
    (let* ((column (plist-get cell :column))
           (src-view (copy-sequence (sqlite3-table-mode-ref :source :view)))
           (where (plist-get src-view :where))
           (name (plist-get column :name))
           ;;TODO show current all where
           ;;TODO default value is cell value
           (str (read-string (format "%s " name) (format " = ")))
           (appended (format "%s %s" name str))
           (new-where (if where
                          (concat where " AND " appended)
                        appended)))
      (plist-put src-view :where new-where)
      (plist-put src-view :page 0)
      ;;TODO current editing row
      ;;TODO restore old where if redraw failed.
      (sqlite3-table-mode--draw-page src-view)
      (sqlite3-table-mode--goto-column name))))

;; TODO
(defun sqlite3-table-mode-clear-filter (&optional all)
  (interactive "P")
  ;;TODO currently clear all
  (let ((src-view (copy-sequence (sqlite3-table-mode-ref :source :view))))
    (plist-put src-view :where nil)
    (sqlite3-table-mode--draw-page src-view)))

(defun sqlite3-table-mode-helm-like-search ()
  ;;TODO doc
  (interactive)
  (sqlite3-mode--before-transit-row)
  (sqlite3-table-mode-helm--search t))

(defun sqlite3-table-mode-helm-glob-search ()
  ;;TODO doc
  (interactive)
  (sqlite3-mode--before-transit-row)
  (sqlite3-table-mode-helm--search nil))

(defun sqlite3-table-mode-helm--search (like)
  (let ((source
         (sqlite3-helm-define
          `((sqlite3-db . ,(sqlite3-mode-ref :stream))
            (sqlite3-composer
             .
             (lambda (pattern)
               (sqlite3-table-mode-helm-compose-query pattern ,like)))
            (requires-pattern . 1)
            (candidate-number-limit . ,sqlite3-table-mode--page-rows)
            ;;TODO action and quit?
            ;; or get LIKE pattern? 
            ;; goto selected row of page?
            (action . (lambda (xs)
                        (message "%s" (sqlite3-table-mode-helm-all-rowid)))))))
        (helm-display-function 'sqlite3-table-mode-helm-onewindow))
    (helm source)))

(defun sqlite3-table-mode-helm-onewindow (buffer)
  (set-window-buffer (selected-window) buffer))

(defun sqlite3-table-mode-helm-all-rowid ()
  (with-helm-buffer
    (save-excursion
      (let ((res '()))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((real (get-text-property (point) 'helm-realvalue)))
            (when real
              (setq res (cons (car real) res))))
          (forward-line 1))
        (nreverse res)))))

;;TODO move to top
(require 'sqlite3-helm)

(defun sqlite3-table-mode-helm-compose-query (pattern like)
  (with-helm-current-buffer
    (let* ((fuzpat (if like
                       (sqlite3-helm-glob-to-fuzzy-like pattern)
                     (sqlite3-helm-glob-to-fuzzy-glob pattern)))
           (schema (sqlite3-table-mode-ref :source :schema))
           (matcher (if like
                        (lambda (x)
                          (sqlite3-format "%o LIKE %T{fuzpat} ESCAPE '\\' "
                                          (nth 1 x)))
                      (lambda (x)
                        (sqlite3-format "%o GLOB %T{fuzpat} "
                                        (nth 1 x))))))
      (sqlite3-format
       '("SELECT %o, %O "
         " FROM %o "
         " WHERE %s "
         " LIMIT %s")
       (sqlite3-table-mode-ref :source :rowid)
       (mapcar (lambda (x) (nth 1 x)) schema)
       (sqlite3-table-mode-ref :source :name)
       (mapconcat matcher schema " OR ")
       sqlite3-table-mode--page-rows))))

(defvar sqlite3-table-mode-header-column-separator
  (let ((sep " "))
    (sqlite3-mode--propertize-background-header sep)
    (propertize sep 'display
                (list 'space :width 1)))
  "String used to separate tabs.")

(defun sqlite3-table-mode--put-error (msg)
  (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
    (overlay-put ov 'sqlite3-error-row-p t)
    (overlay-put ov 'sqlite3-error-message msg)
    (overlay-put ov 'face 'sqlite3-error-line-face)))

(defun sqlite3-table-mode--get-error ()
  (loop for o in (overlays-in (point-at-bol) (point-at-eol))
        if (overlay-get o 'sqlite3-error-row-p)
        return (overlay-get o 'sqlite3-error-message)))

(defun sqlite3-table-mode--clear-error ()
  (remove-overlays (point-at-bol) (point-at-eol)
                   'sqlite3-error-row-p t))

(defun sqlite3-table-mode-revert (&rest dummy)
  (sqlite3-table-mode-redraw-page))

(defun sqlite3-table-mode--new-context ()
  (unless sqlite3-table-mode--context
    (setq sqlite3-table-mode--context
          (list
           :source
           (list :name nil
                 :schema nil
                 :rowid nil
                 :view
                 (list :orders nil
                       :where nil
                       :page 0))
           :cache
           (list :max-page nil)
           :view
           (list :columns nil)))))

(defun sqlite3-table-mode-ref (&rest keys)
  (loop with context = sqlite3-table-mode--context
        for key in keys
        while context
        do (setq context (plist-get context key))
        finally return context))

(defun sqlite3-table-mode-set (value &rest keys)
  (let* ((rev (reverse keys))
         (last (car rev))
         (path (nreverse (cdr rev))))
    (plist-put (apply 'sqlite3-table-mode-ref path) last value)))

(defun sqlite3-table-mode--call/edit-cell (proc)
  ;;TODO unable begin edit if view
  (let ((cell (get-text-property (point) 'sqlite3-mode-cell)))
    (unless cell
      (user-error "No cell is here"))
    (let ((pos (point-marker))
          (value (sqlite3-table-mode--cell-value cell))
          ;; call external function.
          (new (funcall proc cell)))
      (unless (eq (get-text-property pos 'sqlite3-mode-cell) cell)
        (user-error "Table buffer was modified"))
      (unless (equal value new)
        (unless (eq (sqlite3-mode-stream-status) 'transaction)
          (sqlite3-mode--transaction-begin))
        (let ((inhibit-read-only t))
          (goto-char pos)
          (sqlite3-table-mode--replace-current-cell new))))))

(defun sqlite3-table-mode--cleanup-timer ()
  (when sqlite3-table-mode--popup-timer
    (loop for b in (buffer-list)
          if (eq (buffer-local-value 'major-mode b)
                 'sqlite3-mode)
          return t
          finally (progn
                    (cancel-timer sqlite3-table-mode--popup-timer)
                    (setq sqlite3-table-mode--popup-timer nil)))))

(defun sqlite3-table-mode--cell-value (cell)
  (or
   (plist-get cell :edit-value)
   (plist-get cell :source-value)))

(defun sqlite3-table-mode-current-rowid ()
  (let ((row (get-text-property (point) 'sqlite3-mode-row)))
    (plist-get row :rowid)))

(defun sqlite3-table-mode-current-value ()
  (let ((cell (get-text-property (point) 'sqlite3-mode-cell)))
    (sqlite3-table-mode--cell-value cell)))

(defun sqlite3-table-mode-current-type ()
  (let* ((cell (get-text-property (point) 'sqlite3-mode-cell))
         (column (plist-get cell :column))
         (column-nm (plist-get column :name))
         (schema (sqlite3-table-mode-ref :source :schema)))
    (loop for (_x name type . _xs) in schema
          if (equal name column-nm)
          return type)))

(defun sqlite3-table-mode-with-show-buffer (buffer proc)
  (save-window-excursion
    (pop-to-buffer buffer nil t)
    (let ((win (get-buffer-window buffer)))
      (fit-window-to-buffer win))
    (funcall proc)))

(defun sqlite3-table-mode--timer-popup-contents ()
  (save-match-data
    (when (and (eq major-mode 'sqlite3-table-mode)
               ;; suppress tooltip if last command were C-g
               (not (eq last-command 'keyboard-quit)))
      (let ((cell (get-text-property (point) 'sqlite3-mode-cell)))
        (when (plist-get cell :truncated)
          (let ((value (sqlite3-table-mode--cell-value cell)))
            (when value
              (sqlite3-table-mode-tooltip-show value))))))))

(defvar sqlite3-table-mode--cell-buffer " *Sqlite3 Cell* ")

(defun sqlite3-table-mode--create-cell-buffer (value type)
  (let ((buf (get-buffer-create sqlite3-table-mode--cell-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        ;; todo split other func `sqlite3-cell-mode-prepare' ?
        (erase-buffer)
        (cond
         ((stringp value)
          (setq sqlite3-cell-mode--null nil)
          (insert value))
         ((eq :null value)
          (setq sqlite3-cell-mode--null t))))
      (setq sqlite3-cell-mode--type type)
      (sqlite3-cell-mode-setup)
      (setq buffer-undo-list nil)
      (set-buffer-modified-p nil))
    buf))

(defun sqlite3-table-mode-open-edit-window (value type)
  (let* ((config (current-window-configuration))
         (buf (sqlite3-table-mode--create-cell-buffer value type)))
    (pop-to-buffer buf)
    (message "%s"
             (substitute-command-keys
              (concat "Type \\[exit-recursive-edit] to finish the edit."
                      "Type \\[abort-recursive-edit] to cancel the edit.")))
    (unwind-protect
        (progn
          (recursive-edit)
          (let ((new-value (sqlite3-cell-mode-value)))
            new-value))
      (set-window-configuration config))))

(defun sqlite3-table-mode-tooltip-show (text)
  ;; show tooltip at cursor point.
  ;; Unable calculate exactly absolute coord but almost case is ok.
  ;;TODO consider `x-max-tooltip-size'
  (let* ((xy (sqlite3-table-mode-tooltip-absolute-coordinate (point)))
         (y
          ;;TODO reconsider it like popup.el?
          (cond
           ((< (/ (ftruncate (cdr xy)) (x-display-pixel-height)) 0.2)
            (+ (cdr xy) (* (frame-char-height) 3)))
           (t
            (cdr xy)))))
    ;;TODO hack
    (setq text (truncate-string-to-width text 50))
    (x-show-tip
     text nil
     `((left . ,(car xy))
       (top . ,y))
     ;; huge timeout value
     100000)))

(defun sqlite3-table-mode-tooltip-absolute-coordinate (point)
  (let* ((posn (posn-at-point point))
         (xy (posn-x-y posn))
         (x (+ (sqlite3-table-mode-tooltip-frame-posn 'top)
               (car xy)))
         (y (truncate
             (+ (sqlite3-table-mode-tooltip-frame-posn 'left)
                (cdr xy)
                ;; FIXME calculate fringe of bar roughly..
                (* (or (and (boundp 'tool-bar-mode)
                            tool-bar-mode tool-bar-images-pixel-height) 0)
                   1.5)
                (* (or (and (boundp 'menu-bar-mode)
                            menu-bar-mode (frame-char-height)) 0)
                   1.5)))))
    (cons x y)))

(defun sqlite3-table-mode-tooltip-frame-posn (prop)
  (let ((res (cdr (assq prop (frame-parameters)))))
    (or
     (cond
      ((consp res)
       (loop for o in res
             if (numberp o)
             return o))
      (t
       res))
     0)))

;;
;; `sqlite3-table-mode' inner functions
;;

;;TODO generic ?
(defun sqlite3-table-mode--check-row ()
  (let ((errs (sqlite3--filter
               (lambda (o)
                 (overlay-get o 'sqlite3-error-row-p))
               (overlays-in (point-min) (point-max)))))
    (cond
     ((null errs) t)
     ((y-or-n-p "Non saved errors are exists. Really continue? ") t)
     (t (signal 'quit nil)))))

(defun sqlite3-mode--before-transit-row ()
  (when (eq major-mode 'sqlite3-table-mode)
    (ignore-errors
      (sqlite3-table-mode--maybe-apply-changes))
    (sqlite3-table-mode--check-row)))

(defun sqlite3-table-mode--resize-column (arg)
  (let* ((col (sqlite3-table-mode--column-index))
         (column (nth col (sqlite3-table-mode-ref :view :columns)))
         (size (+ (plist-get column :width) arg))
         (modified (buffer-modified-p)))
    (plist-put column :width (max size sqlite3-mode--cell-min-width))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (not (eobp))
          (loop repeat col
                do (sqlite3-table-mode-next-column))
          (sqlite3-table-mode--replace-current-cell
           (sqlite3-table-mode-current-value))
          (forward-line 1))))
    (set-buffer-modified-p modified)))

(defun sqlite3-table-mode-last-line-p ()
  (eq (point-at-bol 2) (point-max)))

(defun sqlite3-table-mode-first-line-p ()
  (eq (point-at-bol) (point-min)))

(defun sqlite3-table-mode--goto-column (column-name)
  (let ((pos (point-at-bol)))
    (catch 'done
      (while pos
        (let* ((cell (get-text-property pos 'sqlite3-mode-cell))
               (colnm (plist-get (plist-get cell :column) :name)))
          ;;TODO it's ok? should be remove column/row model
          (when (equal column-name colnm)
            (throw 'done t))
          (setq pos (sqlite3-table-mode--next-cell t pos)))))
    (when pos
      (goto-char pos))))

;; no need to make local
(defvar sqlite3-table-mode--moved-column nil)

(defun sqlite3-table-mode--move-line (arg)
  (let* ((cell (get-text-property (point) 'sqlite3-mode-cell))
         (column (plist-get cell :column))
         (colnm (plist-get column :name))
         (colpos (if (memq last-command
                           '(sqlite3-table-mode-previous-row
                             sqlite3-table-mode-next-row))
                     sqlite3-table-mode--moved-column
                   (current-column))))
    ;; eobp regard as last line
    (when (and (eobp) (plusp arg))
      (setq arg (1- arg)))
    (let ((rest (forward-line arg)))
      (when (or (not (zerop rest))
                ;; ignore eob as a valid sqlite3 data row
                (eobp))
        (let* ((pager (if (minusp rest)
                          (lambda (arg)
                            (sqlite3-table-mode-backward-page arg)
                            (goto-char (point-max)))
                        (lambda (arg)
                          (sqlite3-table-mode-forward-page arg))))
               (page-row sqlite3-table-mode--page-rows)
               (page (1+ (/ (abs rest) page-row)))
               (line (% rest page-row)))
          (let ((inhibit-redisplay t))
            (funcall pager page))
          (forward-line line))))
    (setq sqlite3-table-mode--moved-column colpos)
    (cond
     (colnm
      (sqlite3-table-mode--goto-column colnm))
     (colpos
      (move-to-column colpos)))))

(defun sqlite3-table-mode--pre-command ()
  (condition-case err
      (progn
        )
    (error
     (sqlite3-mode-log "%s" err))))

(defun sqlite3-table-mode--post-command ()
  (unwind-protect
      (condition-case err
          (let (model-err)
            ;; Update model
            (condition-case err2
                (sqlite3-table-mode--post-change-row)
              (error
               (setq model-err err2)))
            ;; Update view
            (sqlite3-table-mode--highlight-selected)
            (sqlite3-table-mode--show-cell-after-move)
            (sqlite3-table-mode--show-error-at-point)
            (sqlite3-table-mode--delayed-draw-header)
            (when model-err
              (signal (car model-err) (cdr model-err))))
        (error
         (sqlite3-mode-log "%s" err)
         (sqlite3-table-mode--message "%s" err)))
    (sqlite3-table-mode--finish-command)))

(defun sqlite3-table-mode--finish-command ()
  (ignore-errors
    (setq sqlite3-table-mode--processing-row
          (get-text-property (point) 'sqlite3-mode-row))))

(defun sqlite3-table-mode--message (fmt &rest args)
  (let ((msg (propertize (apply 'format fmt args) 'sqlite3-mode-message t))
        (dispmsg (current-message))
        logmsg)
    (when dispmsg
      (let ((start
             (if (get-text-property 0 'sqlite3-mode-message dispmsg)
                 0
               (next-single-property-change 0 'sqlite3-mode-message dispmsg)))
            end)
        (while start
          (setq end (next-single-property-change start 'sqlite3-mode-message dispmsg (length dispmsg)))
          (setq logmsg (concat logmsg (substring dispmsg start end) " "))
          (setq start (next-single-property-change end 'sqlite3-mode-message dispmsg)))))
    (if logmsg
        (message "%s" (format "%s %s" logmsg msg))
      (message "%s" msg))))

(defvar sqlite3-table-mode--processing-row nil)
(make-variable-buffer-local 'sqlite3-table-mode--processing-row)

;;TODO rename
(defun sqlite3-table-mode--pre-handle-rowid ()
  ;;TODO
  ;; (signal 'quit nil)
  )

(defun sqlite3-table-mode--post-change-row ()
  (when (and sqlite3-table-mode--processing-row
             (not (eq sqlite3-table-mode--processing-row
                      (get-text-property (point) 'sqlite3-mode-row))))
    (sqlite3-table-mode--maybe-apply-changes)))

(defmacro sqlite3-table-mode--update-with-handler (&rest form)
  "Update current row with FORM."
  `(condition-case err
       (progn
         ,@form
         (sqlite3-table-mode--clear-error))
     (error
      (sqlite3-table-mode--put-error (format "%s" (cdr err)))
      (signal (car err) (cdr err)))))

(defun sqlite3-table-mode--sync-row (point row)
  ;; FIXME when table have _rowid_, oid, rowid all meta columns.
  (let* ((new-rowid (sqlite3-table-mode--new-rowid row))
         (schema (sqlite3-table-mode-ref :source :schema))
         (query (sqlite3-format
                 "SELECT %o, %O FROM %o WHERE %o = %V"
                 (sqlite3-table-mode-ref :source :rowid)
                 (mapcar (lambda (x) (nth 1 x)) schema)
                 (sqlite3-table-mode-ref :source :name)
                 (sqlite3-table-mode-ref :source :rowid)
                 new-rowid))
         (data (sqlite3-mode-query query)))
    (save-excursion
      (goto-char point)
      (let ((inhibit-read-only t))
        (delete-region (point-at-bol) (point-at-eol))
        (sqlite3-table-mode--insert-row (car data))))))

(defun sqlite3-table-mode--new-rowid (row)
  (let* ((schema (sqlite3-table-mode-ref :source :schema))
         (keys (sqlite3--filter (lambda (x) (nth 5 x)) schema)))
    (cond
     ((null (car row))
      ;; rowid nil means INSERTed the ROW
      (let* ((last (sqlite3-mode-query "SELECT LAST_INSERT_ROWID()")))
        (caar last)))
     ((and (= (length keys) 1)
           (equal (nth 2 (car keys)) "INTEGER"))
      ;; http://www.sqlite.org/lang_createtable.html#rowid
      ;; "Single INTEGER PRIMARY KEY" is used as a ROWID.
      ;; When change that column, implicitly change ROWID value.
      (let ((pair (assoc (nth 1 (car keys)) row)))
        (or
         (and pair
              ;; check link ROWID and PRIMARY source
              (equal (car row) (nth 1 pair))
              ;; 2: edited primary key value is a next ROWID
              (nth 2 pair))
         ;; This should be "CREATE TABLE t(x INTEGER PRIMARY KEY DESC, y, z);" case
         ;; See above URL
         (car row))))
     (t
      ;; ROWID
      (car row)))))

(defun sqlite3-table-mode--row-is-modified (row)
  (loop for (name source edit) in (cdr row)
        unless (or (null edit) (equal source edit))
        return t))

(defun sqlite3-table-mode--show-error-at-point ()
  (when (and sqlite3-table-mode--processing-row
             (not (eq sqlite3-table-mode--processing-row
                      (get-text-property (point) 'sqlite3-mode-row))))
    (let ((errmsg (sqlite3-table-mode--get-error)))
      (cond
       ((null errmsg))
       (t
        (sqlite3-table-mode--message "%s" errmsg))))))

(defun sqlite3-table-mode--show-cell-after-move ()
  (when (get-buffer-window sqlite3-table-mode--cell-buffer)
    (let ((value (sqlite3-table-mode-current-value)))
      (when value
        (let ((type (sqlite3-table-mode-current-type)))
          (sqlite3-table-mode--create-cell-buffer value type))))))

(defun sqlite3-table-mode--highlight-selected ()
  (let ((ov (or sqlite3-table-mode--highlight-overlay
                (let ((tmp (make-overlay (point-max) (point-max))))
                  (overlay-put tmp 'face 'sqlite3-selected-face)
                  tmp))))
    (cond
     ((memq ov (overlays-at (point))))
     ((get-text-property (point) 'sqlite3-mode-cell (current-buffer))
      (let* ((region (sqlite3-table-mode--cell-region (point)))
             (start (car region))
             (end (cdr region)))
        (move-overlay ov start end)
        (setq sqlite3-table-mode--highlight-overlay ov)))
     (t
      (move-overlay ov (point-max) (point-max))))))

(defun sqlite3-table-mode--cell-region (point)
  (sqlite3-mode-property-region point 'sqlite3-mode-cell t))


(defun sqlite3-table-mode--previous-cell (point &optional current-line)
  (let ((region (sqlite3-table-mode--cell-region point)))
    (cond
     ((null region)
      (previous-single-property-change
       point 'sqlite3-mode-cell nil
       (if current-line (point-at-bol) (point-min))))
     ((eq (car region) (point-min))
      (point-min))
     (t
      (sqlite3-table-mode--previous-cell (1- (car region)) current-line)))))

(defun sqlite3-table-mode--next-cell (&optional current-line point)
  (let* ((pos (or point (point)))
         (region (sqlite3-table-mode--cell-region pos))
         (next (next-single-property-change
                (or (cdr region) pos)
                'sqlite3-mode-cell nil)))
    (cond
     ((null next) nil)
     ((and current-line
           (>= next (point-at-eol)))
      nil)
     (t next))))

(defun sqlite3-table-mode--truncate-text (width text)
  (truncate-string-to-width text width nil ?\s t))

(defun sqlite3-table-mode--maybe-apply-changes ()
  (save-excursion
    (let* ((row (sqlite3-table-mode--convert-row
                 sqlite3-table-mode--processing-row))
           (found (sqlite3-table-mode--goto-row
                   sqlite3-table-mode--processing-row)))
      (cond
       ((not found))
       ((null (car row))
        (sqlite3-table-mode--update-with-handler
         (let ((sql (sqlite3-table-mode--insert-sql row)))
           (sqlite3-table-mode--message "Inserting...")
           (sqlite3-mode--execute-sql sql)
           (sqlite3-table-mode--sync-row found row)
           (sqlite3-table-mode--message "Done.")))
        (setq sqlite3-table-mode--processing-row nil))
       ((sqlite3-table-mode--row-is-modified row)
        (sqlite3-table-mode--update-with-handler
         (let ((sql (sqlite3-table-mode--update-sql row)))
           (sqlite3-table-mode--message "Updating...")
           (sqlite3-mode--execute-sql sql)
           (sqlite3-table-mode--sync-row found row)
           (sqlite3-table-mode--message "Done.")))
        (setq sqlite3-table-mode--processing-row nil))
       (t
        (sqlite3-table-mode--clear-error))))))

(defun sqlite3-table-mode--find-row (pred)
  (let ((first (point))
        (col (current-column)))
    (goto-char (point-min))
    (loop while (not (eobp))
          if (let ((row (get-text-property (point) 'sqlite3-mode-row)))
               (funcall pred row))
          return (progn (move-to-column col) (point))
          do (forward-line 1)
          finally (progn (goto-char first) nil))))

(defun sqlite3-table-mode--goto-rowid (rowid)
  (sqlite3-table-mode--find-row
   (lambda (row)
     (equal rowid (car-safe row)))))

(defun sqlite3-table-mode--goto-row (search)
  (sqlite3-table-mode--find-row
   (lambda (row)
     (eq search row))))

(defun sqlite3-table-mode--convert-row (row)
  (cons
   (plist-get row :rowid)
   (loop for cell in (plist-get row :cells)
         collect (let ((col (plist-get cell :column)))
                   (list
                    (plist-get col :name)
                    (plist-get cell :source-value)
                    (plist-get cell :edit-value))))))

(defun sqlite3-table-mode--delayed-draw-header ()
  ;; In the `post-command-hook', `window-hscroll' function still return
  ;; previous value. Although after calling `scroll-right' return correct value.
  (let ((buffer (current-buffer)))
    (run-with-timer 0.1 nil 'sqlite3-table-mode--draw-header buffer)))

(defun sqlite3-table-mode--draw-header (buffer)
  (with-local-quit
    (cancel-function-timers 'sqlite3-table-mode--draw-header)
    (with-current-buffer buffer
      (let* ((disp-headers
              (loop with hscroll = (window-hscroll)
                    ;;  set t after first displaying column in window
                    with flag
                    for col in (sqlite3-table-mode-ref :view :columns)
                    ;; add `sqlite3-table-mode-header-column-separator' width
                    sum (1+ (plist-get col :width)) into right
                    if (< hscroll right)
                    collect (let ((name (plist-get col :name))
                                  (wid (plist-get col :width)))
                              (unless flag
                                (setq wid (- right hscroll 1)))
                              (setq flag t)
                              (cond
                               ((< wid sqlite3-mode--cell-min-width)
                                ;; Beginning of line header may have too short
                                ;;  length of name.
                                (make-string wid ?\s))
                               (t
                                (propertize
                                 (sqlite3-table-mode--truncate-text
                                  wid name)
                                 'face 'sqlite3-mode-column-face))))))
             (filler (make-string (frame-width) ?\s))
             (tail (sqlite3-mode--propertize-background-header filler))
             (separator sqlite3-table-mode-header-column-separator)
             (left (car (window-inside-edges))))
        (setq header-line-format
              (and disp-headers
                   (list
                    ;;FIXME after change examples
                    ;; `scroll-bar-mode' or `linum-mode'.
                    (make-list left separator)
                    (mapconcat 'identity disp-headers separator)
                    tail)))
        (force-mode-line-update)))))

(defun sqlite3-table-mode--insert-row (row &optional clone)
  ;; (car row) is ROWID
  (let ((rowobj (list :rowid (car row) :cells nil))
        (cells nil))
    (loop for v in (cdr row)
          for i from 0
          do (progn
               (when (> i 0)
                 (insert " "))
               (let ((cell (sqlite3-table-mode--insert-cell v i nil)))
                 (setq cells (cons cell cells))
                 (if clone
                     (plist-put cell :edit-value v)
                   (plist-put cell :source-value v)))))
    (plist-put rowobj :cells (nreverse cells))
    (put-text-property
     (point-at-bol) (point-at-eol)
     'sqlite3-mode-row rowobj)))

(defun sqlite3-table-mode--insert-cell (value index &optional cell)
  (let* ((start (point))
         (column (nth index (sqlite3-table-mode-ref :view :columns)))
         (wid (plist-get column :width))
         (truncated (sqlite3-table-mode--truncate-insert value wid))
         (cell (or cell
                   (list :edit-value nil :truncated truncated
                         :column column))))
    (let ((end (point)))
      (put-text-property start end 'sqlite3-mode-cell cell)
      cell)))

(defun sqlite3-table-mode--truncate-insert (value width)
  "Insert VALUE after point restricted by WIDTH."
  (let* ((pos-beg (point))
         (text (sqlite3-mode--to-text value))
         (first-column (current-column))
         (next-column (+ first-column width)))
    (insert text)
    (let ((pos-end (point)))
      (move-to-column next-column t)
      (let ((col-end (point)))
        (while (< next-column (current-column))
          (backward-char))
        (let* ((end (max pos-end col-end))
               ;;TODO describe why don't use truncate-string-to-width
               (truncated
                (cond
                 ((> end (point))
                  (let ((start (max pos-beg
                                    (- (point) sqlite3-mode--cell-min-width))))
                    (delete-region start end))
                  (insert (make-string (- next-column (current-column)) ?\.))
                  t)
                 ((not (equal text value)) t)
                 (t nil))))
          ;; visualize NULL value
          (when (eq value :null)
            (put-text-property pos-beg (point) 'face 'sqlite3-null-face))
          truncated)))))

(defun sqlite3-table-mode--column-index ()
  (let* ((curr (get-text-property (point) 'sqlite3-mode-cell))
         (prev (sqlite3-table-mode--previous-cell (point) t))
         (cell (get-text-property
                (max prev (point-at-bol))
                'sqlite3-mode-cell))
         (column (plist-get cell :column))
         (index (plist-get column :index)))
    (if (eq curr cell)
        0
      (1+ index))))

(defun sqlite3-table-mode--replace-current-cell (value)
  (let* ((pos (point))                  ;save position
         (region (sqlite3-table-mode--cell-region pos)))
    (unless region
      (user-error "Not a cell"))
    (let* ((cell (get-text-property pos 'sqlite3-mode-cell))
           (column (plist-get cell :column))
           (index (plist-get column :index))
           (row (get-text-property pos 'sqlite3-mode-row))
           ;; save rest of line with properties
           (rest (buffer-substring (cdr region) (point-at-eol))))
      (delete-region (car region) (point-at-eol))
      (sqlite3-table-mode--insert-cell value index cell)
      (plist-put cell :edit-value value)
      (put-text-property (point-at-bol) (point-at-eol)
                         'sqlite3-mode-row row)
      (insert rest))
    ;; restore previous position
    (goto-char pos)))

(defun sqlite3-table-mode-redraw-page ()
  (let ((start (window-start))
        (pos (point)))
    (sqlite3-table-mode--draw-page)
    (goto-char pos)
    (set-window-start (selected-window) start)))

(defun sqlite3-table-mode--delay-max-page (buffer)
  (with-local-quit
    (cancel-function-timers 'sqlite3-table-mode--delay-max-page)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((count (sqlite3-table-mode-max-page)))
          (sqlite3-table-mode-set count :cache :max-page))))))

(defun sqlite3-table-mode-max-page ()
  (let* ((query (sqlite3-format
                 '("SELECT ROUND((COUNT(*) / %s) + 0.5)"
                   " FROM %o "
                   " WHERE %s")
                 sqlite3-table-mode--page-rows
                 (sqlite3-table-mode-ref :source :name)
                 (or (sqlite3-table-mode-ref :source :view :where) "1 = 1")))
         (data (sqlite3-mode-query query))
         (max (caar data)))
    (and (string-match "^\\([0-9]+\\)" max)
         (string-to-number (match-string 1 max)))))

(defun sqlite3-table-mode--schema-rowid (schema)
  (loop for r in sqlite3--rowid-columns
        unless (loop for x in schema
                     if (equal r (upcase (nth 1 x)))
                     return x)
        return r
        finally (error "Unable take valid ROWID column")))

(defun sqlite3-table-mode--clear-page ()
  ;;TODO completely remove
  ;; (sqlite3-table-mode-set nil :cache :max-page)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays (point-min) (point-max)))

(defun sqlite3-table-mode-jump-to-rowid (rowid)
  ;;TODO sample query
  ;; "SELECT ROWID,* FROM %o{table} ORDER BY ROWID LIMIT 100 OFFSET 100 * %s{page}"
  (let ((src-view (copy-sequence (sqlite3-table-mode-ref :source :view)))
        (page (sqlite3-table-mode--get-rowid-page rowid)))
    (plist-put src-view :page page)
    (sqlite3-table-mode--draw-page src-view)))

(defun sqlite3-table-mode--move-page (page error-message)
  (let ((src-view (copy-sequence (sqlite3-table-mode-ref :source :view))))
    ;;TODO may exceed max page
    (plist-put src-view :page page)
    (sqlite3-table-mode--draw-page src-view)
    (cond
     ((equal page (sqlite3-table-mode-ref :source :view :page))
      ;; succeeded
      ;; redraw header forcibly
      (sqlite3-table-mode--delayed-draw-header))
     (t
      ;; TODO no ding?
      (ding)
      (message "%s" error-message)))))

(defun sqlite3-table-mode--load-schema (name)
  (let* ((schema (sqlite3-table-mode--schema name))
         (rowid-name (sqlite3-table-mode--schema-rowid schema))
         (source (sqlite3-table-mode-ref :source)))
    (plist-put source :name name)
    (plist-put source :schema schema)
    (plist-put source :rowid rowid-name)))

(defun sqlite3-table-mode--schema (table)
  (or
   (and (equal (sqlite3-table-mode-ref :source :name) table)
        (sqlite3-table-mode-ref :source :schema))
   (sqlite3-read-table-schema (sqlite3-mode-ref :stream) table)))

;;TODO bad!! fuckoff my head
(defun sqlite3-table-mode--get-rowid-page (rowid &optional source-view)
  (let* ((query (sqlite3-table-mode--build-getting-page rowid source-view))
         (data (sqlite3-mode-query query)))
    (string-to-number (caar data))))

(defun sqlite3-table-mode--build-getting-page (rowid &optional source-view)
  (let* ((source (sqlite3-table-mode-ref :source))
         (src-view (or source-view (plist-get source :view)))
         (tablename (plist-get source :name))
         (rowid-name (plist-get source :rowid))
         (where (or (plist-get src-view :where) "1 = 1"))
         (orders (plist-get src-view :orders))
         (order-by (sqlite3-table-mode--compile-orders orders)))
    (sqlite3-format
     `("SELECT ((COUNT(*) - 1)/ 100) "
       " FROM %o{tablename} "
       " WHERE %o{rowid-name} <= %V{rowid}"
       "  AND %s{where} "
       ,@(and orders
              `(" ORDER BY %s{order-by}"))))))

;;TODO
(defun sqlite3-table-mode--compile-preceeding-where (orders)
  (loop for order in orders
        do (let ((column (car order))
                 (term (cadr order))))))

(defun sqlite3-table-mode--build-paging-query (source-view)
  (let* ((source (sqlite3-table-mode-ref :source))
         (tablename (plist-get source :name))
         (schema (plist-get source :schema))
         (columns (mapcar (lambda (x) (nth 1 x)) schema))
         (rowid-name (plist-get source :rowid))
         (page-row sqlite3-table-mode--page-rows)
         (page (or (plist-get source-view :page) 0))
         (where (or (plist-get source-view :where) "1 = 1"))
         (orders (plist-get source-view :orders))
         (order-by (sqlite3-table-mode--compile-orders orders)))
    (sqlite3-format
     `("SELECT %o{rowid-name}, %O{columns}"
       " FROM %o{tablename} "
       " WHERE %s{where}"
       ,@(and orders
              `(" ORDER BY %s{order-by}"))
       " LIMIT %s{page-row} OFFSET %s{page-row} * %s{page}"))))

;;TODO refactor
;;todo remove force
(defun sqlite3-table-mode--draw-page (&optional source-view)
  (unless source-view
    (setq source-view (sqlite3-table-mode-ref :source :view)))
  (save-excursion
    (let* ((schema (sqlite3-table-mode-ref :source :schema))
           (query (sqlite3-table-mode--build-paging-query source-view))
           (data (sqlite3-mode-query query)))
      (cond
       (data
        (sqlite3-table-mode--clear-page)
        (sqlite3-table-mode--set-header schema data)
        (let ((inhibit-read-only t))
          ;; ignore header row
          (dolist (row data)
            (sqlite3-table-mode--insert-row row)
            (insert "\n"))
          (unless(memq (sqlite3-mode-stream-status) '(transaction))
            (set-buffer-modified-p nil)))
        (setq buffer-read-only t)
        ;;TODO only check first time? when move forward non exists page?
        (unless (sqlite3-table-mode-ref :cache :max-page)
          (run-with-idle-timer
           1 nil 'sqlite3-table-mode--delay-max-page
           (current-buffer))))
       ((> (plist-get source-view :page) 0)
        ;; TODO last page have no data.
        (plist-put source-view :page (1- (plist-get source-view :page))))
       (t
        (sqlite3-table-mode--clear-page)
        (sqlite3-table-mode--set-header schema)
        (sqlite3-table-mode-set 0 :cache :max-page)
        (set-buffer-modified-p nil)))
      (sqlite3-table-mode-set source-view :source :view))))

(defun sqlite3-table-mode--set-header (schema &optional data)
  (let* ((lis (cons
               (cons
                "ROWID"                 ;dummy
                (loop for def in schema
                      collect (nth 1 def)))
               data))
         (width-def (sqlite3-mode--calculate-width lis))
         (headers (car lis))
         (columns (loop for max in (cdr width-def)
                        for hdr in (cdr headers)
                        for idx from 0
                        collect (list :name hdr
                                      :index idx
                                      :initial-max max
                                      :width (min max 30)))))
    (sqlite3-table-mode-set columns :view :columns)))

;;TODO
(defun sqlite3-table-mode-isearch ()
  (let ((timer (run-with-idle-timer
                1 t 'sqlite3-table-mode--delayed-search
                (current-buffer))))
    (unwind-protect
        (read-from-minibuffer "Search: ")
      (cancel-timer timer))))

(defun sqlite3-table-mode--delayed-search (buffer)
  ;; TODO cancel last timer func
  (let ((contents (save-window-excursion
                    (let* ((win (active-minibuffer-window))
                           (minibuf (window-buffer win)))
                      (with-current-buffer minibuf
                        (minibuffer-contents))))))
    (with-current-buffer "*scratch*"
      (save-excursion
        (goto-char (point-min))
        (insert contents "\n")))))


;; http://www.sqlite.org/syntaxdiagrams.html#expr
(defconst sqlite3-table-mode--filters-operators
  '(
    (and filter)
    (or filter)))

(defconst sqlite3-table-mode--filter-operators
  '(
    (is-null "IS NULL")
    (is-not-null "IS NOT NULL")
    (= "=" exp)
    (<= "<=" exp)
    (< "<" exp)
    (>= ">=" exp)
    (> ">" exp)
    (like "LIKE" exp "ESCAPE '\\'")
    (glob "GLOB" exp)
    (between "BETWEEN" exp "AND" exp)
    ;;TODO not 
    ))

(defun sqlite3-table-mode--read-filters (&optional column)
  (completing-read "" sqlite3-table-mode--filters-operators nil t)
  (loop with filter
        while (setq filter (sqlite3-table-mode--read-filter))
        collect filter into res
        finally return res))

(defun sqlite3-table-mode--read-filter ()
  (let* ((filter-defs sqlite3-table-mode--filter-operators)
         (operator (intern-soft (completing-read
                                 "Filter operator: " filter-defs
                                 nil t)))
         (filter-def (assq operator sqlite3-table-mode--filter-operators)))
    (loop for def in (cdr filter-def)
          collect (cond
                   ((stringp def) def)
                   ((eq def 'exp)
                    ;; number | string
                    ;; FIXME: only accept emacs sexp style number.
                    ;;    otherwise accept as string.
                    (let* ((s (read-string "Value: "))
                           (parse-res (ignore-errors (read-from-string s))))
                      (cond
                       ((and (= (cdr-safe parse-res) (length s))
                             (numberp (car-safe parse-res)))
                        (car-safe parse-res))
                       (t
                        s))))
                   (t (error "Not supported"))))))

;; COLUMN ::= string of column name

;; ORDERS ::= ORDER
;; ORDER ::= (COLUMN ORDERING-TERM)

(defun sqlite3-table-mode--compile-orders (orders)
  (mapconcat
   'sqlite3-table-mode--compile-order
   orders
   ", "))

(defun sqlite3-table-mode--compile-order (order)
  (let* ((column (car order))
         (term (cadr order))
         (ordering-term (if (eq term :desc) "DESC" "ASC")))
    (sqlite3-format "%o{column} %s{ordering-term}")))

;;TODO reconsider it
;; FILTERS ::= (AND FILTER ...) | (OR FILTER ...)
;; FILTER ::= (COLUMN OPERATOR) | (COLUMN OPERATOR EXP ...) | FILTERS

;; OPERATOR ::= car of `sqlite3-table-mode--filter-operators'
;; EXP    ::= string | number

;;TODO number and string (quote)

(defun sqlite3-table-mode--compile-filters (filters)
  (concat
   "("
   (mapconcat
    'sqlite3-table-mode--compile-filter
    (cdr filters)
    (cond
     ((eq (car filters) 'or)  " OR ")
     ((eq (car filters) 'and) " AND ")
     (t (error "Invalid filters %s" filters))))
   ")"))

(defun sqlite3-table-mode--compile-filter (filter)
  (cond
   ((memq (car filter) '(or and))
    (sqlite3-table-mode--compile-filters filter))
   ((and (listp filter) (>= (length filter) 2))
    (let* ((column (car filter))
           (operator (cadr filter))
           (exps (cddr filter))
           (filter-def (assq operator sqlite3-table-mode--filter-operators)))
      (unless filter-def
        (error "Invalid operator %s" operator))
      (concat
       (sqlite3-format-object column)
       " "
       (loop for def in (cdr filter-def)
             collect (cond
                      ((eq def 'exp)
                       (let ((exp (pop exps)))
                         (unless (or (stringp exp)
                                     (numberp exp))
                           (error "Invalid EXP %s" exp))
                         (sqlite3-format-value exp)))
                      ((stringp def) def)
                      (t (error "Invalid definition %s" def)))
             into res
             finally return (mapconcat 'identity res " ")))))
   (t
    (error "Invalid filter %s" filter))))

;;
;; Automated DML
;;

(defun sqlite3-table-mode--update-sql (row)
  (sqlite3-format
   "UPDATE %o SET %s WHERE %o = %V;"
   (sqlite3-table-mode-ref :source :name)
   (mapconcat
    'identity
    (loop for (name source edit) in (cdr row)
          if edit
          collect (sqlite3-format "%o = %V" name edit))
    ", ")
   (sqlite3-table-mode-ref :source :rowid) (car row)))

(defun sqlite3-table-mode--insert-sql (row)
  (let (columns values)
    (loop for (name source edit) in (cdr row)
          do (setq columns (cons name columns)
                   values (cons (or edit "") values)))
    (sqlite3-format
     "INSERT INTO %o (%O) VALUES (%V);"
     (sqlite3-table-mode-ref :source :name)
     columns values)))

(defun sqlite3-table-mode--delete-sql (row)
  (sqlite3-format
   "DELETE FROM %o WHERE %o = %V;"
   (sqlite3-table-mode-ref :source :name)
   (sqlite3-table-mode-ref :source :rowid) (car row)))

;;TODO consider key exchange
(defun sqlite3-table-mode--key-cond (keys row)
  (loop for k in keys
        collect (destructuring-bind (_ source _) (assoc k (cdr row))
                  (sqlite3-format "%o = %V" k source))
        into res
        finally return (mapconcat 'identity res " AND ")))

(defun sqlite3-table-mode--schema-have-key ()
  (let ((schema (sqlite3-table-mode-ref :source :schema)))
    (loop for (_ name _ notnull _ primaryp) in schema
          if (and primaryp (not notnull))
          return nil
          if primaryp
          collect name)))

;;;
;;; sqlite3-schema-mode
;;;

(defconst sqlite3-schema-mode--close-icon "[+]")
(defconst sqlite3-schema-mode--open-icon "[-]")

(defvar sqlite3-schema-mode-map nil)

(unless sqlite3-schema-mode-map

  (let ((map (make-sparse-keymap)))

    (set-keymap-parent map sqlite3-mode-map)

    (define-key map "\C-m" 'sqlite3-schema-mode-toggle-item)
    (define-key map " " 'sqlite3-schema-mode-toggle-item)
    (define-key map "C" 'sqlite3-schema-mode-create-definition)
    (define-key map "V" 'sqlite3-schema-mode-open-table)

    (define-key map "k" 'sqlite3-schema-mode-previous-line)
    (define-key map "j" 'sqlite3-schema-mode-next-line)
    (define-key map "p" 'sqlite3-schema-mode-previous-line)
    (define-key map "n" 'sqlite3-schema-mode-next-line)

    (setq sqlite3-schema-mode-map map)))

(defconst sqlite3-schema-mode-menu-spec
  `("Sqlite3"
    ,@sqlite3-mode-menu-spec
    ["View table" sqlite3-schema-mode-open-table t]
    ["Open/Close item" sqlite3-schema-mode-toggle-item t]
    ["Copy DDL" sqlite3-schema-mode-create-definition t]
    ))

(easy-menu-define sqlite3-schema-mode-menu
  sqlite3-schema-mode-map
  "Menu used in Sqlite3 schema mode."
  sqlite3-schema-mode-menu-spec)

(define-derived-mode sqlite3-schema-mode sqlite3-mode "Sqlite3 Schema"
  "Sqlite3 schema view mode"
  (use-local-map sqlite3-schema-mode-map)
  (set (make-local-variable 'revert-buffer-function)
       'sqlite3-schema-mode-revert))

(put 'sqlite3-schema-mode 'mode-class 'special)

(defun sqlite3-schema-mode-open-table ()
  (interactive)
  (let ((item (get-text-property (point) 'sqlite3-schema-item)))
    (unless item
      (user-error "No item is here"))
    (cond
     ((memq (plist-get item :type) '(table view))
      (sqlite3-schema-mode--evacuate-buffer)
      (let ((name (plist-get item :name)))
        (sqlite3-mode-open-table name)))
     (t
      (error "Cannot open %s as table" (plist-get item :type))))))

;;TODO test
;; TODO rename?
;;TODO read QUERY
;; TODO cleanup when switch to schema view
;;    DROP VIEW IF EXISTS %o{view-name};
(defun sqlite3-mode-open-query (query)
  (interactive)
  (let* ((stream (sqlite3-mode-ref :stream))
         (view-name (sqlite3--unique-name stream "tempview"))
         ;; create TEMP VIEW to read QUERY like a table
         ;; http://www.sqlite.org/lang_createview.html
         (query (sqlite3-format
                 '("CREATE TEMP VIEW %o{view-name}"
                   " AS %s{query}"))))
    (sqlite3-mode-query query)
    (sqlite3-schema-mode--evacuate-buffer)
    (sqlite3-mode-open-table view-name)))

(defun sqlite3-schema-mode-toggle-item ()
  (interactive)
  (let ((item (get-text-property (point) 'sqlite3-schema-item)))
    (unless item
      (user-error "No item is here"))
    (let ((region (sqlite3-mode-property-region
                   (point) 'sqlite3-schema-item))
          (child (plist-get item :child))
          (type (plist-get item :type))
          (inhibit-read-only t)
          (modifiedp (buffer-modified-p)))
      (cond
       ((and child (not (overlay-get child 'invisible)))
        (overlay-put child 'invisible t)
        (sqlite3-schema-mode--set-close region))
       ((overlayp child)
        (overlay-put child 'invisible nil)
        (sqlite3-schema-mode--set-open region))
       (t
        (sqlite3-mode--check-stream)
        (let ((ov (cond
                   ((eq type 'table-heading)
                    (sqlite3-schema-mode--draw-objects
                     'sqlite3-read-tables 'table))
                   ((eq type 'view-heading)
                    (sqlite3-schema-mode--draw-objects
                     'sqlite3-read-views 'view))
                   ((eq type 'index-heading)
                    (sqlite3-schema-mode--draw-objects
                     'sqlite3-read-indexes 'index))
                   ((eq type 'trigger-heading)
                    (sqlite3-schema-mode--draw-objects
                     'sqlite3-read-triggers 'trigger))
                   ((memq type '(table view))
                    (sqlite3-schema-mode--draw-table item))
                   ;; TODO
                   (t (error "Not a supported type `%s'" type)))))
          ;;TODO
          ;; (overlay-put ov 'isearch-open-invisible (lambda (x)))
          (plist-put item :child ov))
        (sqlite3-schema-mode--set-open region)))
      (set-buffer-modified-p modifiedp))))

(defun sqlite3-schema-mode-next-line ()
  (interactive)
  (forward-line 1))

(defun sqlite3-schema-mode-previous-line ()
  (interactive)
  (forward-line -1))

(defun sqlite3-schema-mode-create-definition ()
  (interactive)
  (let* ((item (get-text-property (point) 'sqlite3-schema-item))
         (name (plist-get item :name))
         (type (plist-get item :type))
         (results
          (cond
           ((memq type '(table view index trigger))
            (let ((typename (symbol-name type)))
              (sqlite3-mode-query
               (sqlite3-format
                '("SELECT sql "
                  " FROM sqlite_master"
                  " WHERE type = %T{typename}"
                  " AND name = %T{name}")))))
           (t
            (error "Not a supported type `%s'" type))))
         (ddl (caar results)))
    (unless (stringp ddl)
      (error "No definitions here"))
    (kill-new ddl)
    (message "%s" ddl)))

(defun sqlite3-schema-mode--restore-from-evacuation ()
  (let ((to-buf (current-buffer))
        (from-buf (sqlite3-mode-ref :schemaview))
        pos)
    (setq buffer-read-only nil)
    (unwind-protect
        (cond
         ((or (null from-buf)
              (not (buffer-live-p from-buf)))
          (message "There is no evacuation buffer")
          nil)
         (t
          (with-current-buffer from-buf
            (setq pos (point))
            (append-to-buffer to-buf (point-min) (point-max))
            (dolist (ov (overlays-in (point-min) (point-max)))
              (move-overlay ov (overlay-start ov) (overlay-end ov) to-buf)))
          (goto-char pos)
          t))
      (setq buffer-read-only t))))

(defun sqlite3-schema-mode--evacuate-buffer ()
  (let ((to-buf (sqlite3-mode-ref :schemaview))
        (pos (point)))
    (when to-buf
      (kill-buffer to-buf))
    (setq to-buf (generate-new-buffer
                  (format " *sqlite3 schema %s* " buffer-file-name)))
    (sqlite3-mode-set :schemaview to-buf)
    (append-to-buffer to-buf (point-min) (point-max))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (move-overlay ov (overlay-start ov) (overlay-end ov) to-buf))
    (with-current-buffer to-buf
      (goto-char pos))))

(defun sqlite3-schema-mode-draw-view ()
  (save-excursion
    (remove-overlays (point-min) (point-max))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (loop for (name type) in '(("Table" table-heading)
                                 ("View" view-heading)
                                 ("Index" index-heading)
                                 ("Trigger" trigger-heading))
            do (let ((start (point)))
                 (insert (format "%s %s\n"
                                 sqlite3-schema-mode--close-icon
                                 (propertize name
                                             'face 'sqlite3-mode-table-face)))
                 (put-text-property
                  start (point)
                  'sqlite3-schema-item (list :type type))
                 (insert "\n"))))))

(defun sqlite3-schema-mode-revert (&rest dummy)
  (sqlite3-mode-open-schema-mode t))

(defun sqlite3-schema-mode--switch-icon (start icon)
  (let ((first (point))
        save-point)
    (save-excursion
      (goto-char start)
      (unless (looking-at "[ \t]*\\(\\[[-+]\\]\\)")
        (user-error "No icon is here"))
      (let* ((start (match-beginning 1))
             (end (match-end 1))
             (props (text-properties-at start)))
        (replace-match icon nil nil nil 1)
        ;; copy properties to icon text
        (set-text-properties start end props)
        (when (and (<= start first) (<= first end))
          (setq save-point t))))
    (when save-point
      (goto-char first))))

(defun sqlite3-schema-mode--set-open (region)
  (sqlite3-schema-mode--switch-icon
   (car region) sqlite3-schema-mode--open-icon))

(defun sqlite3-schema-mode--set-close (region)
  (sqlite3-schema-mode--switch-icon
   (car region) sqlite3-schema-mode--close-icon))

(defun sqlite3-schema-mode--draw-objects (gatherer type)
  (save-excursion
    (let ((objects (funcall gatherer (sqlite3-mode-ref :stream))))
      (forward-line 2)
      (loop with start = (point)
            for obj in objects
            do (let ((start (point))
                     (table (list :type type :name obj)))
                 (sqlite3-schema-mode--draw-line
                  1
                  (format "%s %s\n"
                          sqlite3-schema-mode--close-icon
                          ;;TODO change face-name
                          (propertize obj 'face 'sqlite3-mode-object-face)))
                 (put-text-property
                  start (point)
                  'sqlite3-schema-item table))
            finally return (make-overlay start (point))))))

(defun sqlite3-schema-mode--draw-table (parent)
  (save-excursion
    (let* ((name (plist-get parent :name))
           (schema (or (plist-get parent :schema)
                       (let ((s (sqlite3-schema-mode--table-schema name)))
                         (plist-put parent :schema s)
                         s))))
      (forward-line 1)
      (let* ((headers '("NAME" "TYPE" "NULL" "DEFAULT" "KEY"))
             (start (point))
             (data (cons headers schema))
             (width-def (sqlite3-mode--calculate-width data))
             (hdrs (loop for h in headers
                         for w in width-def
                         collect (let ((name
                                        (truncate-string-to-width h w nil ?\s)))
                                   (propertize
                                    name
                                    'face 'sqlite3-header-background)))))
        (insert "\n")
        (sqlite3-schema-mode--draw-line
         2
         (mapconcat 'identity hdrs " "))
        (loop for (name type notnull default primaryp) in schema
              do (progn
                   (sqlite3-schema-mode--draw-line
                    2
                    (sqlite3-schema-mode--table-column
                     width-def name type notnull default primaryp))))
        ;; column items are branch of the view.
        (put-text-property start (point) 'sqlite3-schema-item parent)
        (make-overlay start (point))))))

(defun sqlite3-schema-mode--table-column (def &rest values)
  (let ((texts (loop for w in def
                     for v in values
                     collect (truncate-string-to-width
                              (sqlite3-mode--to-text v) w nil ?\s))))
    (mapconcat
     'identity
     (cons
      (propertize (car texts) 'face 'sqlite3-mode-column-face)
      (cdr texts)) " ")))

(defconst sqlite3-schema-mode-indent-level 2)

(defun sqlite3-schema-mode--draw-line (level line)
  (insert (make-string (* level sqlite3-schema-mode-indent-level) ?\s))
  (insert line)
  (insert "\n"))

;; TODO rename sqlite3-schema-view (?)
(defun sqlite3-schema-mode--table-schema (name)
  (loop for (idx name type notnull default primaryp)
        in (sqlite3-read-table-schema
            (sqlite3-mode-ref :stream) name)
        collect (list name type
                      (if notnull "no" "yes")
                      (or default "no")
                      (or (and primaryp "*") ""))))

;;;
;;; Sqlite3 binary mode (TODO no need?)
;;;

(defvar sqlite3-binary-mode-map nil)
(unless sqlite3-binary-mode-map
  (let ((map (make-sparse-keymap)))

    (suppress-keymap map)

    (define-key map "\C-c\C-c" 'sqlite3-mode-toggle-view)

    (setq sqlite3-binary-mode-map map)))

(define-minor-mode sqlite3-binary-mode
  ""
  nil nil sqlite3-binary-mode-map)

(defun sqlite3-mode-toggle-view ()
  "Toggle sqlite3 view <-> binary view"
  (interactive)
  (cond
   (sqlite3-binary-mode
    (sqlite3-view))
   (t
    (sqlite3-binary-view))))

(defun sqlite3-binary-view ()
  (let ((magic-mode-alist
         (delq nil (mapcar
                    (lambda (elt)
                      (unless (eq (cdr elt) 'sqlite3-view)
                        elt))
                    magic-mode-alist)))
        (read-only buffer-read-only))
    ;; warn if file is too huge and may exit.
    (find-alternate-file buffer-file-name)
    (when read-only
      (setq buffer-read-only t))
    (sqlite3-binary-mode 1)))

;;;
;;; Sqlite3 cell mode
;;;

;;TODO display schema TYPE. Do Not prohibit input TYPE.

(defvar sqlite3-cell-mode-map nil)
(unless sqlite3-cell-mode-map

  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-t" 'sqlite3-cell-mode-toggle-null)
    (define-key map "\C-c\C-c" 'exit-recursive-edit)
    (define-key map "\C-x\C-s" 'exit-recursive-edit)
    (define-key map "\C-c\C-k" 'abort-recursive-edit)

    (setq sqlite3-cell-mode-map map)))

(defun sqlite3-cell-mode-setup ()
  (setq mode-line-process
        `((:eval
           (concat
            " "
            (if (and sqlite3-cell-mode--type
                     (not (string= sqlite3-cell-mode--type "")))
                (propertize
                 sqlite3-cell-mode--type
                 'face 'sqlite3-mode-object-face)
              (propertize "unknown" 'face 'sqlite3-null-face))))
          (:eval
           (and (eq (sqlite3-cell-mode-value) :null)
                (concat " " (propertize "null" 'face 'sqlite3-null-face))))))
  (setq mode-name "Sqlite3 Cell Edit")
  (use-local-map sqlite3-cell-mode-map))

(defvar sqlite3-cell-mode--null nil)
(defvar sqlite3-cell-mode--type nil)

(defun sqlite3-cell-mode-toggle-null ()
  "Toggle input value between NULL and some value."
  (interactive)
  (cond
   ((null sqlite3-cell-mode--null)
    (erase-buffer)
    (setq sqlite3-cell-mode--null t)
    (message "Now cell value is NULL"))
   (t
    (setq sqlite3-cell-mode--null nil)))
  (set-buffer-modified-p t))

(defun sqlite3-cell-mode-value ()
  (cond
   ((> (buffer-size) 0)
    (buffer-string))
   (sqlite3-cell-mode--null :null)
   (t "")))


;;;
;;; Sqlite3 database manager interface
;;;

;;;###autoload
(defun sqlite3-find-file (file)
  "Open FILE as Sqlite3 database.

FYI Normally, sqlite3 database open automatically `sqlite3-view' but
should not open huge file. This function support to open such file."
  (interactive "FSqlite3 File: ")
  (cond
   ((file-exists-p file)
    (unless (sqlite3-file-guessed-database-p file)
      (error "Not a valid database file")))
   ((not (y-or-n-p "File not exists. Create a Sqlite3 database? "))
    (signal 'quit nil)))
  (let ((buf (get-file-buffer file)))
    (unless buf
      (setq buf (create-file-buffer (file-name-nondirectory file)))
      (with-current-buffer buf
        (set-visited-file-name file)
        (sqlite3-view)))
    (switch-to-buffer buf)))

;;;###autoload
(defun sqlite3-view ()
  "View current buffer as a sqlite3 database."
  (interactive)
  (set-buffer-modified-p nil)
  (sqlite3-mode-open-schema-mode t))

;;;
;;; Package load/unload
;;;

;;;###autoload
(setq magic-mode-alist
      `((,sqlite3-file-header-regexp . sqlite3-view)))

(defun sqlite3-mode-unload-function ()
  (let ((pair (rassq 'sqlite3-view magic-mode-alist)))
    (when pair
      (setq magic-mode-alist (delq pair magic-mode-alist)))))


;;;; TODO TESTING

(defun sqlite3-create-alternate-table (stream create-sql)
  "Execute CREATE-SQL in STREAM. This function not begin transaction.
If you need transaction, begin transaction by your own before calling this function."
  (unless (let ((case-fold-search t))
            (string-match "^[ \t\n]*create[ \t\n]+table[ \t\n]+\\([^ \t\n]+\\)" create-sql))
    (user-error "Invalid create sql `%s'" create-sql))
  (let* ((table (match-string 1 create-sql))
         (temp-table (sqlite3--unique-name stream table))
         (src-columns (mapcar
                       (lambda (x) (nth 1 x))
                       (sqlite3-read-table-schema stream table))))
    (unless src-columns
      (error "Unable to get `%s' table columns" table))
    (let ((temp-create (sqlite3-format
                        "CREATE TEMPORARY TABLE %o (%O)"
                        temp-table src-columns)))
      (sqlite3-stream-execute stream temp-create))
    (let ((temp-insert (sqlite3-format
                        "INSERT INTO %o SELECT %O FROM %o"
                        temp-table src-columns table)))
      (sqlite3-stream-execute stream temp-insert))
    (let ((drop-object (sqlite3-format
                        "DROP TABLE %o"
                        table)))
      (sqlite3-stream-execute stream drop-object))
    (sqlite3-stream-execute stream create-sql)
    (let* ((new-columns (mapcar
                         (lambda (x) (nth 1 x))
                         (sqlite3-read-table-schema stream table)))
           (share-columns (delq nil
                                (mapcar
                                 (lambda (col)
                                   (and (member col src-columns)
                                        col))
                                 new-columns)))
           (insert-object (sqlite3-format
                           "INSERT INTO %o (%O) SELECT %O FROM %o"
                           table share-columns share-columns
                           temp-table)))
      (sqlite3-stream-execute stream insert-object))
    (let ((drop-temp (sqlite3-format
                      "DROP TABLE %o"
                      temp-table)))
      (sqlite3-stream-execute stream drop-temp))))

;;TODO non used
(defun sqlite3-mode--faced-insert (face &rest args)
  (let ((start (point)))
    (apply 'insert args)
    (put-text-property start (point) 'face face)))

;;TODO non used
(defun sqlite3-mode--clone-cond ()
  (copy-sequence sqlite3-table-mode--context))

;; TODO :table :page :order :where


(provide 'sqlite3-mode)

;;; sqlite3-mode.el ends here
