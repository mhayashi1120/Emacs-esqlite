;;; esqlite-mode.el --- esqlite file editing mode

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: data
;; URL: https://github.com/mhayashi1120/Emacs-sqlite/raw/master/esqlite-mode.el
;; Emacs: GNU Emacs 24 or later
;; Version: 0.0.1
;; Package-Requires: ((esqlite "0.1.0") (esqlite-helm "0.1.0"))

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

;;  This module provides view/edit/search esqlite database file like
;;  a normal file.

;;; Install:

;; (autoload 'esqlite-find-file "esqlite"
;;   "Open FILE as a Esqlite database" t)

;;; TODO:
;; * incremental search all data in table (by helm)
;; * to right side if number column.
;; * blob
;; * create hook (ex: commit, update delete...)
;; * clone row
;; * easy filter
;; * order by
;; * sqlite_temp_master

;; * esqlite-mode
;;     table is valid -> schema -> read data
;; * esqlite-mode
;;   tool menu, easy menu

;; * how to edit exclusively. DO NOT USE visited file time. that is nasty.
;; * multiple stream will be created too many revert. why??
;; * search query by SELECT\|UPDATE\|DELETE\|INSERT\|CREATE\|ALTER

;; * esqlite-mode
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
;;    esqlite -stats file "select * from table"
;;    esqlite file "explain select * from table"

;; * SELECT ifnull((SELECT 1 WHERE changes() = 1), abs(-9223372036854775808));
;;   more refine way.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'esqlite)
(require 'esqlite-helm)
(require 'easymenu)

(defgroup esqlite-mode ()
  "Manipulate esqlite database."
  :prefix "esqlite-"
  :group 'esqlite)

;;;
;;; esqlite-mode
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

(defcustom esqlite-use-highlight-line t
  "Use `hl-line-mode' or not."
  :type 'boolean
  :group 'esqlite-mode)

(defface esqlite-header-background
  '((((type x w32 mac ns) (class color))
     :background "LightSteelBlue" :foreground "black")
    (((class color))
     (:background "white" :foreground "black")))
  "Face to fontify background of header line."
  :group 'esqlite-mode)

(defface esqlite-selected-face
  '((t (:inherit mode-line-highlight)))
  "Face for highlighting current cell."
  :group 'esqlite-mode)

(defface esqlite-error-line-face
  '((t (:inherit isearch-fail)))
  "Face for highlighting failed part in changing row."
  :group 'esqlite-mode)

(defface esqlite-null-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for any NULL."
  :group 'esqlite-mode)

(defface esqlite-mode-object-face
  '((t (:inherit font-lock-function-name-face)))
  "Face to fontify esqlite object."
  :group 'esqlite-mode)

(defface esqlite-mode-table-face
  '((t (:inherit esqlite-mode-object-face)))
  "Face to fontify esqlite table."
  :group 'esqlite-mode)

(defface esqlite-mode-column-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face to fontify esqlite table column."
  :group 'esqlite-mode)

(defvar esqlite-mode-map nil)

(unless esqlite-mode-map
  (let ((map (make-sparse-keymap)))

    (suppress-keymap map)

    (define-key map "\C-c!" 'esqlite-mode-send-query)
    (define-key map "\C-c\C-q" 'esqlite-mode-send-query)
    (define-key map "\C-x\C-s" 'esqlite-mode-commit-changes)
    (define-key map "\C-c\C-r" 'esqlite-mode-rollback)
    (define-key map "\C-c\C-c" 'esqlite-mode-toggle-view)
    (define-key map "g" 'revert-buffer)

    (setq esqlite-mode-map map)))

(defconst esqlite-mode-menu-spec
  '(
    ["Toggle View Binary/Esqlite" esqlite-mode-toggle-view t]
    ["Discard Changes" esqlite-mode-rollback t]
    ["Save Changes" esqlite-mode-commit-changes t]
    ["Send Query" esqlite-mode-send-query t]
    ["Revert Buffer" revert-buffer t]
    ))

(defcustom esqlite-mode-hook nil
  "Hook called enter the `esqlite-mode'."
  :group 'esqlite-mode
  :type 'hook)

(defcustom esqlite-mode-before-transaction-hook nil
  "Run before transaction is started."
  :group 'esqlite-mode
  :type 'hook)

(defcustom esqlite-mode-after-commit-hook nil
  "Run after transaction is commit. "
  :group 'esqlite-mode
  :type 'hook)

(defcustom esqlite-mode-after-rollback-hook nil
  "Run after transaction is rollback.
TODO safe-hook"
  :group 'esqlite-mode
  :type 'hook)

(defcustom esqlite-mode-after-transaction-hook nil
  "Run after transaction is finished. (commit / rollback)
TODO safe-hook"
  :group 'esqlite-mode
  :type 'hook)

(defvar esqlite-mode--context nil)
(make-variable-buffer-local 'esqlite-mode--context)
(put 'esqlite-mode--context 'permanent-local t)

(defvar esqlite-table-mode--page-rows 100)

(defconst esqlite-mode--cell-min-width 3)

(define-derived-mode esqlite-mode fundamental-mode "Esqlite"
  "Basic mode to manipulate esqlite database."
  (unless buffer-file-name
    (error "Not a file buffer"))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays (point-min) (point-max))
  (esqlite-mode--new-context)
  (setq buffer-undo-list t)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq header-line-format nil)
  ;; to insert non tab space
  (setq indent-tabs-mode nil)
  (add-hook 'kill-buffer-hook
            'esqlite-mode--after-kill-buffer nil t)
  (add-hook 'local-write-file-hooks
            'esqlite-mode--write-buffer nil t)
  (when (fboundp 'hl-line-mode)
    (hl-line-mode -1))
  ;; disable creating #hoge.sqlite# file
  (set (make-local-variable 'backup-inhibited) t)
  (auto-save-mode -1)
  (esqlite-mode-setup-mode-line)
  (run-mode-hooks 'esqlite-mode-hook))

(defvar esqlite-mode-read-query-history nil)

(defun esqlite-mode-read-query ()
  (read-from-minibuffer "Query: " nil nil nil 'esqlite-mode-read-query-history))

(defvar esqlite-mode-read-table-history nil)

(defun esqlite-mode--read-table-name ()
  (esqlite-mode--check-stream)
  (let ((completion-ignore-case t))
    (completing-read
     "Table: "
     ;;TODO view
     (esqlite-read-tables (esqlite-mode-ref :stream))
     nil t nil 'esqlite-mode-read-table-history)))

(defun esqlite-mode--new-context ()
  (unless esqlite-mode--context
    (setq esqlite-mode--context
          (list
           :stream nil
           :transaction nil
           :schemaview nil))))

(defvar esqlite-mode-debug nil)
(defvar esqlite-mode--debug-buffer "*Esqlite Debug*")

(defun esqlite-mode-log (fmt &rest args)
  (when esqlite-mode-debug
    (with-current-buffer (get-buffer-create esqlite-mode--debug-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (apply 'format fmt args) "\n")))))

(defun esqlite-mode-ref (key)
  (plist-get esqlite-mode--context key))

(defun esqlite-mode-set (key value)
  (plist-put esqlite-mode--context key value))

;;TODO
(defun esqlite-mode--write-buffer ()
  (message "Not saved (Not yet implemented)")
  t)

(defun esqlite-mode--after-kill-buffer ()
  (when (esqlite-mode-ref :stream)
    (esqlite-stream-close (esqlite-mode-ref :stream)))
  (when (esqlite-mode-ref :schemaview)
    (kill-buffer (esqlite-mode-ref :schemaview)))
  (esqlite-table-mode--cleanup-timer))

;;TODO separate mode-line-format each mode
(defun esqlite-mode-setup-mode-line ()
  (setq mode-line-process
        '((:eval
           (when (and (eq major-mode 'esqlite-table-mode)
                      (esqlite-table-mode-ref :source :name))
             (concat " ["
                     (propertize (esqlite-table-mode-ref :source :name)
                                 'face 'esqlite-mode-table-face)
                     ":"
                     ;;TODO consider this. too many execution.
                     (let ((max (esqlite-table-mode-ref :cache :max-page)))
                       (if (and (numberp max) (zerop max))
                           "(No Data)"
                         (format "%d/%s"
                                 (1+ (esqlite-table-mode-ref :source :view :page))
                                 (or max "Unknown"))))
                     "]")))
          (:eval
           (concat
            " "
            (let ((status (esqlite-mode-stream-status)))
              (cond
               ((eq status 'prompt)
                (propertize "Run" 'face 'minibuffer-prompt))
               ((eq status 'transaction)
                (propertize "Transaction" 'face 'compilation-info))
               ((eq status 'querying)
                (propertize "Querying" 'face 'compilation-warning))
               (t
                (propertize "No Process" 'face 'shadow)))))))))

(defun esqlite-mode--to-text (datum)
  (cond
   ((stringp datum)
    (let ((oneline (or
                    (and (string-match "^\\([^\n]+\\)" datum)
                         (match-string 1 datum))
                    datum)))
      (replace-regexp-in-string "\t" "\\\\t" oneline)))
   ((eq datum :null) "null")
   (t "")))

(defun esqlite-mode--calculate-width (data)
  ;; decide list length by header line
  (loop with all-width = (make-list (length (car data))
                                    esqlite-mode--cell-min-width)
        for row in data
        do (loop for datum in row
                 for pair on all-width
                 for idx from 0
                 do (let* ((text (esqlite-mode--to-text datum))
                           (wid (string-width text)))
                      (setcar pair (max (car pair) wid))))
        finally return all-width))

(defun esqlite-mode-property-region (point property &optional restrict-line)
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

(defun esqlite-mode--propertize-background-header (string)
  (let ((end (length string)))
    (add-text-properties 0 end
                         `(
                           face esqlite-header-background
                                tab-separator t)
                         string)
    string))

(defun esqlite-mode-open-table (table)
  (esqlite-mode--check-stream)
  (esqlite-table-mode)
  (esqlite-table-mode--load-schema table)
  (let ((src-view (copy-sequence (esqlite-table-mode-ref :source :view))))
    (esqlite-table-mode--draw-page src-view)
    ;; redraw header forcibly
    (esqlite-table-mode--delayed-draw-header)))

(defun esqlite-mode--send (query)
  (esqlite-mode--check-stream)
  (let ((stream (esqlite-mode-ref :stream)))
    (esqlite-stream-send stream query)))

(defun esqlite-mode--execute-sql (sql)
  (esqlite-mode--check-stream)
  (let ((stream (esqlite-mode-ref :stream)))
    (esqlite-stream-execute stream sql)))

(defun esqlite-mode--check-stream ()
  "Check current buffer's database file is opend by sqlite."
  (let ((stream (esqlite-mode-ref :stream)))
    (unless (and stream
                 (esqlite-stream-alive-p stream))
      (when stream
        (esqlite-stream-close stream))
      (setq stream (esqlite-stream-open buffer-file-name))
      (esqlite-mode-set :stream stream))))

(defun esqlite-mode-redraw-page ()
  (cond
   ((eq major-mode 'esqlite-table-mode)
    (esqlite-table-mode-redraw-page))
   ((eq major-mode 'esqlite-schema-mode)
    (esqlite-mode-open-schema-mode t))
   (t (error "Not supported in this mode"))))

(defun esqlite-mode-query (query)
  (esqlite-mode--check-stream)
  (let* ((stream (esqlite-mode-ref :stream))
         (data (esqlite-stream-read stream query)))
    data))

(defun esqlite-mode-stream-status ()
  (let ((stream (esqlite-mode-ref :stream)))
    (cond
     ((null stream) 'exit)
     ((not (esqlite-stream-alive-p stream)) 'exit)
     ((esqlite-stream-prompt-p stream)
      (cond
       ((esqlite-mode-ref :transaction)
        'transaction)
       (t
        'prompt)))
     ;;TODO continue prompt
     (t 'querying))))

(defun esqlite-mode--transaction-begin ()
  (run-hooks 'esqlite-mode-before-transaction-hook)
  (esqlite-mode--execute-sql "BEGIN")
  (esqlite-mode-set :transaction t))

(defun esqlite-mode--transaction-rollback ()
  (esqlite-mode--execute-sql "ROLLBACK")
  (esqlite-mode-set :transaction nil)
  (esqlite-mode-safe-run-hooks 'esqlite-mode-after-transaction-hook)
  (esqlite-mode-safe-run-hooks 'esqlite-mode-after-rollback-hook))

(defun esqlite-mode--transaction-commit ()
  (esqlite-mode--execute-sql "COMMIT")
  (esqlite-mode-set :transaction nil)
  (esqlite-mode-safe-run-hooks 'esqlite-mode-after-transaction-hook)
  (esqlite-mode-safe-run-hooks 'esqlite-mode-after-commit-hook))

(defun esqlite-mode-safe-run-hooks (hook)
  (dolist (f (and (boundp hook)
                  (symbol-value hook)))
    (condition-case err
        (funcall f)
      (error
       (esqlite-table-mode--message "Esqlite Error: %s" err)
       (sit-for 0.5)))))

;;TODO consider `local-write-file-hooks'
(defun esqlite-mode-commit-changes ()
  "Commit changes to database file.
If changed data violate database constraint, transaction will be rollback.
"
  (interactive)
  (esqlite-mode--before-transit-row)
  (unless (eq (esqlite-mode-stream-status) 'transaction)
    (error "Hav not began a transaction"))
  (when (y-or-n-p "Commit all changes? ")
    (esqlite-mode--transaction-commit)
    ;; sync with physical data.
    (esqlite-mode-redraw-page)))

(defun esqlite-mode-rollback ()
  "Discard all changes."
  (interactive)
  (unless (eq (esqlite-mode-stream-status) 'transaction)
    (error "Have not began a transaction"))
  (unless (y-or-n-p "Really discard changes? ")
    (signal 'quit nil))
  (esqlite-mode--transaction-rollback)
  (esqlite-mode-redraw-page))

(defun esqlite-mode-reset ()
  "Cause of unknown problem of editing buffer, reset esqlite command stream
if you want."
  (interactive)
  (unless (y-or-n-p "Restart esqlite process with discarding changes? ")
    (signal 'quit nil))
  (when (esqlite-mode-ref :stream)
    (esqlite-stream-close (esqlite-mode-ref :stream)))
  (esqlite-mode--check-stream)
  (esqlite-mode-redraw-page))

(defun esqlite-mode-send-query (sql-or-command)
  "Send SQL-OR-COMMAND to esqlite process.
Do not send compound statement or containing comment sql.
 This make unknown fatal result ;-)"
  (interactive "sSQL: ")
  (esqlite-mode--before-transit-row)
  (let ((result (esqlite-mode--send sql-or-command)))
    (cond
     ((stringp result)
      (message "%s" result))
     (t
      (esqlite-mode-redraw-page)))))

(defun esqlite-mode-open-schema-mode (&optional force)
  "Show schema view of current buffer file."
  (interactive)
  (esqlite-mode--before-transit-row)
  (esqlite-schema-mode)
  (esqlite-mode--check-stream)
  (cond
   (force
    (esqlite-schema-mode-draw-view))
   ((esqlite-schema-mode--restore-from-evacuation))
   (t
    (esqlite-schema-mode-draw-view)))
  (set-buffer-modified-p nil))

;;;
;;; esqlite-table-mode
;;;

(defvar esqlite-table-mode-map nil)

(unless esqlite-table-mode-map

  (let ((map (make-sparse-keymap)))

    (set-keymap-parent map esqlite-mode-map)

    (define-key map "\C-c\C-a" 'esqlite-table-mode-add-row)
    (define-key map "\C-c\C-d" 'esqlite-table-mode-delete-row)
    (define-key map "\C-c\C-k" 'esqlite-table-mode-shrink-column)
    (define-key map "\C-c\C-l" 'esqlite-table-mode-widen-column)
    (define-key map "\C-c\C-j" 'esqlite-table-mode-jump-to-page)
    (define-key map "\C-c\ew" 'esqlite-table-mode-copy-cell)
    (define-key map "\C-c\C-y" 'esqlite-table-mode-paste-cell)
    (define-key map "\C-c>" 'esqlite-table-mode-forward-page)
    (define-key map "\C-c<" 'esqlite-table-mode-backward-page)
    ;;TODO
    ;; (define-key map "\C-c\e>" 'esqlite-table-mode-last-page)
    ;; (define-key map "\C-c\e<" 'esqlite-table-mode-first-page)
    (define-key map "F" 'esqlite-table-mode-forward-page)
    (define-key map "B" 'esqlite-table-mode-backward-page)
    (define-key map "\C-i" 'esqlite-table-mode-forward-cell)
    (define-key map "\e\C-i" 'esqlite-table-mode-backward-cell)
    (define-key map "\C-m" 'esqlite-table-mode-view-cell)
    (define-key map "e" 'esqlite-table-mode-start-edit)
    (define-key map "v" 'esqlite-table-mode-view-cell)
    (define-key map "h" 'esqlite-table-mode-previous-column)
    (define-key map "l" 'esqlite-table-mode-next-column)
    (define-key map "k" 'esqlite-table-mode-previous-row)
    (define-key map "j" 'esqlite-table-mode-next-row)
    (define-key map "S" 'esqlite-mode-open-schema-mode)

    (define-key map "\C-c\eC" 'esqlite-table-mode-clone-row)

    ;; TODO keybinding `#' `%'
    (define-key map "#" (make-sparse-keymap))
    (define-key map "#s" 'esqlite-table-mode-easy-sort)
    (define-key map "#c" 'esqlite-table-mode-clear-sort)
    (define-key map "%" (make-sparse-keymap))
    (define-key map "%f" 'esqlite-table-mode-easy-filter)
    (define-key map "%c" 'esqlite-table-mode-clear-filter)
    (define-key map "%g" 'esqlite-table-mode-helm-glob-search)
    (define-key map "%l" 'esqlite-table-mode-helm-like-search)
    
    (setq esqlite-table-mode-map map)))

(defconst esqlite-table-mode-menu-spec
  `("Esqlite"
    ,@esqlite-mode-menu-spec
    ["Schema View" esqlite-mode-open-schema-mode t]
    ["Cell View" esqlite-table-mode-view-cell t]
    "---"
    ("Edit"
     ["Edit Cell" esqlite-table-mode-start-edit t]
     ["Add Row" esqlite-table-mode-add-row t]
     ["Delete Row" esqlite-table-mode-delete-row t]
     ["Copy Value" esqlite-table-mode-copy-cell t]
     ["Paste Value" esqlite-table-mode-paste-cell t]
     )
    ("Paging"
     ["Forward Page" esqlite-table-mode-forward-page t]
     ["Backward Page" esqlite-table-mode-backward-page t]
     ["Jump to Page" esqlite-table-mode-jump-to-page t]
     )
    ("Display"
     ["Shrink Column" esqlite-table-mode-shrink-column t]
     ["Widen Column" esqlite-table-mode-widen-column t]
     ["Sort" esqlite-table-mode-easy-sort t]
     ["Clear Sort" esqlite-table-mode-clear-sort t]
     ["Filter" esqlite-table-mode-easy-filter t]
     ["Clear Filter" esqlite-table-mode-clear-filter t]
     ["GLOB Search (helm)" esqlite-table-mode-helm-glob-search t]
     ["LIKE Search (helm)" esqlite-table-mode-helm-helm-search t]
     )
    ("Cursor"
     ["Forward Cell" esqlite-table-mode-forward-cell t]
     ["Backward Cell" esqlite-table-mode-backward-cell t]
     ["Previous Column" esqlite-table-mode-previous-column t]
     ["Next Column" esqlite-table-mode-next-column t]
     ["Previous Row" esqlite-table-mode-previous-row t]
     ["Next Row" esqlite-table-mode-next-row t]
     )))

(easy-menu-define esqlite-table-mode-menu
  esqlite-table-mode-map
  "Menu used in Esqlite table mode."
  esqlite-table-mode-menu-spec)

(defvar esqlite-table-mode--context nil)
(make-variable-buffer-local 'esqlite-table-mode--context)

(defvar esqlite-table-mode--popup-timer nil)

(defvar esqlite-table-mode--highlight-overlay nil)
(make-variable-buffer-local 'esqlite-table-mode--highlight-overlay)

(define-derived-mode esqlite-table-mode esqlite-mode "Esqlite Table"
  "Esqlite table view mode"
  ;;TODO clear when schema-view
  (set (make-local-variable 'revert-buffer-function)
       'esqlite-table-mode-revert)
  (add-hook 'post-command-hook
            'esqlite-table-mode--post-command nil t)
  (add-hook 'pre-command-hook
            'esqlite-table-mode--pre-command nil t)
  (cond
   ((null esqlite-use-highlight-line))
   ((require 'hl-line nil t)
    (hl-line-mode 1))
   (t
    ;; forcibly disable the customize variable
    (setq esqlite-use-highlight-line nil)))
  (esqlite-table-mode--new-context)
  (use-local-map esqlite-table-mode-map)
  (unless esqlite-table-mode--popup-timer
    (setq esqlite-table-mode--popup-timer
          (run-with-idle-timer
           1 t 'esqlite-table-mode--timer-popup-contents))))

(put 'esqlite-table-mode 'mode-class 'special)

(defun esqlite-table-mode-view-cell ()
  "View current cell with opening subwindow."
  (interactive)
  (let ((value (esqlite-table-mode-current-value)))
    (unless value
      (error "No cell is here"))
    (let* ((type (esqlite-table-mode-current-type))
           (buf (esqlite-table-mode--create-cell-buffer value type)))
      (display-buffer buf))))

;;TODO
(defun esqlite-table-mode-clone-row ()
  (interactive)
  (let ((source-row (get-text-property (point) 'esqlite-mode-row)))
    (unless source-row
      (error "No row is here"))
    (forward-line 0)
    (let* ((row (cons nil (mapcar
                           (lambda (cell)
                             (esqlite-table-mode--cell-value cell))
                           (plist-get source-row :cells))))
           (inhibit-read-only t))
      (insert "\n")
      (forward-line -1)
      ;;TODO esqlite-table-mode--insert-row set :source-value non-nil
      (esqlite-table-mode--insert-row row t)
      (forward-line 0))))

(defun esqlite-table-mode-add-row ()
  "Add new row after the cursor."
  (interactive)
  ;;TODO need?
  ;; (esqlite-mode--before-transit-row)
  (when esqlite-table-mode--highlight-overlay
    (move-overlay esqlite-table-mode--highlight-overlay
                  (point-max) (point-max)))
  (forward-line 0)
  (let* ((num (length (esqlite-table-mode-ref :view :columns)))
         (row (cons nil (make-list num nil)))
         (inhibit-read-only t))
    (insert "\n")
    (forward-line -1)
    (esqlite-table-mode--insert-row row)
    (forward-line 0)))

(defun esqlite-table-mode-delete-row ()
  "Delete current row."
  (interactive)
  (unless (y-or-n-p "Really delete this row? ")
    (signal 'quit nil))
  (unless (eq (esqlite-mode-stream-status) 'transaction)
    (esqlite-mode--transaction-begin))
  (esqlite-table-mode--update-with-handler
   (let* ((row (esqlite-table-mode--convert-row
                (get-text-property (point) 'esqlite-mode-row)))
          (sql (esqlite-table-mode--delete-sql row)))
     (message "Deleting...")
     (esqlite-mode--execute-sql sql)))
  (let ((inhibit-read-only t))
    (delete-region (point-at-bol)
                   (point-at-bol 2))))

(defvar esqlite-table-mode--killed nil)
(defun esqlite-table-mode-copy-cell ()
  "Copy cell."
  (interactive)
  (let ((cell (get-text-property (point) 'esqlite-mode-cell)))
    (unless cell
      (user-error "No cell is here"))
    (let* ((value (esqlite-table-mode--cell-value cell))
           (text (or (and (stringp value) value) "")))
      ;; To save the :null value use the elisp variable.
      (setq esqlite-table-mode--killed value)
      (kill-new text)))
  (message "Current cell is saved."))

(defun esqlite-table-mode-paste-cell ()
  "Insert kill-ring to current cell."
  (interactive)
  (esqlite-table-mode--call/edit-cell
   (lambda (cell)
     (let ((text (current-kill 0)))
       (if (and (equal text "")
                (eq esqlite-table-mode--killed :null))
           :null
         text)))))

(defun esqlite-table-mode-start-edit ()
  "Edit current cell with opening subwindow."
  (interactive)
  (when (plusp (recursion-depth))
    (user-error
     "%s"
     (substitute-command-keys
      (concat
       "Other recursive edit is working. "
       "Type \\[abort-recursive-edit] to quit previous recursive edit"))))
  (esqlite-table-mode--call/edit-cell
   (lambda (cell)
     (let ((value (esqlite-table-mode--cell-value cell))
           (type (esqlite-table-mode-current-type)))
       (esqlite-table-mode-open-edit-window value type)))))

(defun esqlite-table-mode-shrink-column (arg)
  "Shrink current column"
  (interactive "p")
  (esqlite-table-mode--resize-column (* -1 arg)))

(defun esqlite-table-mode-widen-column (arg)
  "Widen current column"
  (interactive "p")
  (esqlite-table-mode--resize-column arg))

(defun esqlite-table-mode-jump-to-page (page)
  "Jump to selected PAGE"
  (interactive
   (list (read-number "Page: ")))
  (esqlite-mode--before-transit-row)
  (esqlite-table-mode--move-page
   (1- page) "No such page"))

(defun esqlite-table-mode-last-page (arg)
  "Jump to last page."
  (interactive "p")
  (esqlite-mode--before-transit-row)
  ;;TODO
  (error "Not yet supported")
  ;; (esqlite-table-mode--move-page
  ;;  (+ (esqlite-table-mode-ref :source :view :page) arg)
  ;;  "No more next page")
  )

(defun esqlite-table-mode-forward-page (arg)
  "Forward page."
  (interactive "p")
  (esqlite-mode--before-transit-row)
  (esqlite-table-mode--move-page
   (+ (esqlite-table-mode-ref :source :view :page) arg)
   "No more next page"))

(defun esqlite-table-mode-backward-page (arg)
  "Backward page."
  (interactive "p")
  (when (= (esqlite-table-mode-ref :source :view :page) 0)
    (user-error "This is a first page"))
  (esqlite-mode--before-transit-row)
  (esqlite-table-mode--move-page
   (- (esqlite-table-mode-ref :source :view :page) arg)
   "No more previous page"))

(defun esqlite-table-mode-next-row (arg)
  "Goto next line of row."
  (interactive "p")
  (esqlite-table-mode--move-line arg))

(defun esqlite-table-mode-previous-row (arg)
  "Goto previous line of row."
  (interactive "p")
  (esqlite-table-mode--move-line (- arg)))

(defun esqlite-table-mode-next-column ()
  "Goto next column."
  (interactive)
  (let ((next (esqlite-table-mode--next-cell t)))
    (when next
      (goto-char next))))

(defun esqlite-table-mode-previous-column ()
  "Goto previous column."
  (interactive)
  (let ((prev (esqlite-table-mode--previous-cell (point) t)))
    (when prev
      (goto-char prev))))

(defun esqlite-table-mode-forward-cell ()
  "Forward cell over the row."
  (interactive)
  (let ((next (esqlite-table-mode--next-cell)))
    ;;TODO paging
    (cond
     (next
      (goto-char next)))))

(defun esqlite-table-mode-backward-cell ()
  "Backward cell over the row."
  (interactive)
  (let ((prev (esqlite-table-mode--previous-cell (point))))
    ;;TODO paging
    (cond
     (prev
      (goto-char prev)))))

(defun esqlite-table-mode-easy-sort (&optional desc)
  (interactive "P")
  (esqlite-mode--before-transit-row)
  (let ((cell (get-text-property (point) 'esqlite-mode-cell)))
    (unless cell
      (user-error "No column is here"))
    (let* ((src-view (copy-sequence (esqlite-table-mode-ref :source :view)))
           (column (plist-get cell :column))
           (orders (plist-get src-view :orders))
           (name (plist-get column :name))
           (term (if desc :desc :asc))
           (rowid (esqlite-table-mode-current-rowid)))
      ;;TODO show in header-format like tabulated-mode
      (let ((order (assoc name orders)))
        (unless order
          (setq order (list name))
          (setq orders (append orders (list order))))
        (setcdr order (list term)))
      (plist-put src-view :orders orders)
      (esqlite-table-mode--draw-page src-view)
      (esqlite-table-mode--goto-column name))))

(defun esqlite-table-mode-clear-sort (&optional all)
  (interactive "P")
  (esqlite-mode--before-transit-row)
  ;;TODO currently clear all
  (esqlite-table-mode-set nil :source :view :orders)
  (esqlite-table-mode-redraw-page))

;;TODO immitate excel autofill
;;TODO consider cell datatype
;;TODO compare > < <= >=
(defun esqlite-table-mode-easy-filter ()
  ""
  (interactive)
  (esqlite-mode--before-transit-row)
  (let ((cell (get-text-property (point) 'esqlite-mode-cell)))
    (unless cell
      (user-error "No column is here"))
    (let* ((column (plist-get cell :column))
           (src-view (copy-sequence (esqlite-table-mode-ref :source :view)))
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
      (esqlite-table-mode--draw-page src-view)
      (esqlite-table-mode--goto-column name))))

;; TODO
(defun esqlite-table-mode-clear-filter (&optional all)
  (interactive "P")
  ;;TODO currently clear all
  (let ((src-view (copy-sequence (esqlite-table-mode-ref :source :view))))
    (plist-put src-view :where nil)
    (esqlite-table-mode--draw-page src-view)))

(defun esqlite-table-mode-helm-like-search ()
  ;;TODO doc
  (interactive)
  (esqlite-mode--before-transit-row)
  (esqlite-table-mode-helm--search t))

(defun esqlite-table-mode-helm-glob-search ()
  ;;TODO doc
  (interactive)
  (esqlite-mode--before-transit-row)
  (esqlite-table-mode-helm--search nil))

(defun esqlite-table-mode-helm--search (like)
  (let ((source
         (esqlite-helm-define
          `((esqlite-db . ,(esqlite-mode-ref :stream))
            (esqlite-composer
             .
             (lambda (pattern)
               (esqlite-table-mode-helm-compose-query pattern ,like)))
            (requires-pattern . 1)
            (candidate-number-limit . ,esqlite-table-mode--page-rows)
            ;;TODO action and quit?
            ;; or get LIKE pattern? 
            ;; goto selected row of page?
            (action . (lambda (xs)
                        (message "%s" (esqlite-table-mode-helm-all-rowid)))))))
        (helm-display-function 'esqlite-table-mode-helm-onewindow))
    (helm source)))

(defun esqlite-table-mode-helm-onewindow (buffer)
  (set-window-buffer (selected-window) buffer))

(defun esqlite-table-mode-helm-all-rowid ()
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

(defun esqlite-table-mode-helm-compose-query (pattern like)
  (with-helm-current-buffer
    (let* ((fuzpat (if like
                       (esqlite-helm-glob-to-fuzzy-like pattern)
                     (esqlite-helm-glob-to-fuzzy-glob pattern)))
           (schema (esqlite-table-mode-ref :source :schema))
           (matcher (if like
                        (lambda (x)
                          (esqlite-format "%o LIKE %T{fuzpat} ESCAPE '\\' "
                                          (nth 1 x)))
                      (lambda (x)
                        (esqlite-format "%o GLOB %T{fuzpat} "
                                        (nth 1 x))))))
      (esqlite-format
       '("SELECT %o, %O "
         " FROM %o "
         " WHERE %s "
         " LIMIT %s")
       (esqlite-table-mode-ref :source :rowid)
       (mapcar (lambda (x) (nth 1 x)) schema)
       (esqlite-table-mode-ref :source :name)
       (mapconcat matcher schema " OR ")
       esqlite-table-mode--page-rows))))

(defvar esqlite-table-mode-header-column-separator
  (let ((sep " "))
    (esqlite-mode--propertize-background-header sep)
    (propertize sep 'display
                (list 'space :width 1)))
  "String used to separate tabs.")

(defun esqlite-table-mode--put-error (msg)
  (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
    (overlay-put ov 'esqlite-error-row-p t)
    (overlay-put ov 'esqlite-error-message msg)
    (overlay-put ov 'face 'esqlite-error-line-face)))

(defun esqlite-table-mode--get-error ()
  (loop for o in (overlays-in (point-at-bol) (point-at-eol))
        if (overlay-get o 'esqlite-error-row-p)
        return (overlay-get o 'esqlite-error-message)))

(defun esqlite-table-mode--clear-error ()
  (remove-overlays (point-at-bol) (point-at-eol)
                   'esqlite-error-row-p t))

(defun esqlite-table-mode-revert (&rest dummy)
  (esqlite-table-mode-redraw-page))

(defun esqlite-table-mode--new-context ()
  (unless esqlite-table-mode--context
    (setq esqlite-table-mode--context
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

(defun esqlite-table-mode-ref (&rest keys)
  (loop with context = esqlite-table-mode--context
        for key in keys
        while context
        do (setq context (plist-get context key))
        finally return context))

(defun esqlite-table-mode-set (value &rest keys)
  (let* ((rev (reverse keys))
         (last (car rev))
         (path (nreverse (cdr rev))))
    (plist-put (apply 'esqlite-table-mode-ref path) last value)))

(defun esqlite-table-mode--call/edit-cell (proc)
  ;;TODO unable begin edit if view
  (let ((cell (get-text-property (point) 'esqlite-mode-cell)))
    (unless cell
      (user-error "No cell is here"))
    (let ((pos (point-marker))
          (value (esqlite-table-mode--cell-value cell))
          ;; call external function.
          (new (funcall proc cell)))
      (unless (eq (get-text-property pos 'esqlite-mode-cell) cell)
        (user-error "Table buffer was modified"))
      (unless (equal value new)
        (unless (eq (esqlite-mode-stream-status) 'transaction)
          (esqlite-mode--transaction-begin))
        (let ((inhibit-read-only t))
          (goto-char pos)
          (esqlite-table-mode--replace-current-cell new))))))

(defun esqlite-table-mode--cleanup-timer ()
  (when esqlite-table-mode--popup-timer
    (loop for b in (buffer-list)
          if (eq (buffer-local-value 'major-mode b)
                 'esqlite-mode)
          return t
          finally (progn
                    (cancel-timer esqlite-table-mode--popup-timer)
                    (setq esqlite-table-mode--popup-timer nil)))))

(defun esqlite-table-mode--cell-value (cell)
  (or
   (plist-get cell :edit-value)
   (plist-get cell :source-value)))

(defun esqlite-table-mode-current-rowid ()
  (let ((row (get-text-property (point) 'esqlite-mode-row)))
    (plist-get row :rowid)))

(defun esqlite-table-mode-current-value ()
  (let ((cell (get-text-property (point) 'esqlite-mode-cell)))
    (esqlite-table-mode--cell-value cell)))

(defun esqlite-table-mode-current-type ()
  (let* ((cell (get-text-property (point) 'esqlite-mode-cell))
         (column (plist-get cell :column))
         (column-nm (plist-get column :name))
         (schema (esqlite-table-mode-ref :source :schema)))
    (loop for (_x name type . _xs) in schema
          if (equal name column-nm)
          return type)))

(defun esqlite-table-mode-with-show-buffer (buffer proc)
  (save-window-excursion
    (pop-to-buffer buffer nil t)
    (let ((win (get-buffer-window buffer)))
      (fit-window-to-buffer win))
    (funcall proc)))

(defun esqlite-table-mode--timer-popup-contents ()
  (save-match-data
    (when (and (eq major-mode 'esqlite-table-mode)
               ;; suppress tooltip if last command were C-g
               (not (eq last-command 'keyboard-quit)))
      (let ((cell (get-text-property (point) 'esqlite-mode-cell)))
        (when (plist-get cell :truncated)
          (let ((value (esqlite-table-mode--cell-value cell)))
            (when value
              (esqlite-table-mode-tooltip-show value))))))))

(defvar esqlite-table-mode--cell-buffer " *Esqlite Cell* ")

(defun esqlite-table-mode--create-cell-buffer (value type)
  (let ((buf (get-buffer-create esqlite-table-mode--cell-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        ;; todo split other func `esqlite-cell-mode-prepare' ?
        (erase-buffer)
        (cond
         ((stringp value)
          (setq esqlite-cell-mode--null nil)
          (insert value))
         ((eq :null value)
          (setq esqlite-cell-mode--null t))))
      (setq esqlite-cell-mode--type type)
      (esqlite-cell-mode-setup)
      (setq buffer-undo-list nil)
      (set-buffer-modified-p nil))
    buf))

(defun esqlite-table-mode-open-edit-window (value type)
  (let* ((config (current-window-configuration))
         (buf (esqlite-table-mode--create-cell-buffer value type)))
    (pop-to-buffer buf)
    (message "%s"
             (substitute-command-keys
              (concat "Type \\[exit-recursive-edit] to finish the edit."
                      "Type \\[abort-recursive-edit] to cancel the edit.")))
    (unwind-protect
        (progn
          (recursive-edit)
          (let ((new-value (esqlite-cell-mode-value)))
            new-value))
      (set-window-configuration config))))

(defun esqlite-table-mode-tooltip-show (text)
  ;; show tooltip at cursor point.
  ;; Unable calculate exactly absolute coord but almost case is ok.
  ;;TODO consider `x-max-tooltip-size'
  (let* ((xy (esqlite-table-mode-tooltip-absolute-coordinate (point)))
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

(defun esqlite-table-mode-tooltip-absolute-coordinate (point)
  (let* ((posn (posn-at-point point))
         (xy (posn-x-y posn))
         (x (+ (esqlite-table-mode-tooltip-frame-posn 'top)
               (car xy)))
         (y (truncate
             (+ (esqlite-table-mode-tooltip-frame-posn 'left)
                (cdr xy)
                ;; FIXME calculate fringe of bar roughly..
                (* (or (and (boundp 'tool-bar-mode)
                            tool-bar-mode tool-bar-images-pixel-height) 0)
                   1.5)
                (* (or (and (boundp 'menu-bar-mode)
                            menu-bar-mode (frame-char-height)) 0)
                   1.5)))))
    (cons x y)))

(defun esqlite-table-mode-tooltip-frame-posn (prop)
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
;; `esqlite-table-mode' inner functions
;;

;;TODO generic ?
(defun esqlite-table-mode--check-row ()
  (let ((errs (esqlite--filter
               (lambda (o)
                 (overlay-get o 'esqlite-error-row-p))
               (overlays-in (point-min) (point-max)))))
    (cond
     ((null errs) t)
     ((y-or-n-p "Non saved errors are exists. Really continue? ") t)
     (t (signal 'quit nil)))))

(defun esqlite-mode--before-transit-row ()
  (when (eq major-mode 'esqlite-table-mode)
    (ignore-errors
      (esqlite-table-mode--maybe-apply-changes))
    (esqlite-table-mode--check-row)))

(defun esqlite-table-mode--resize-column (arg)
  (let* ((col (esqlite-table-mode--column-index))
         (column (nth col (esqlite-table-mode-ref :view :columns)))
         (size (+ (plist-get column :width) arg))
         (modified (buffer-modified-p)))
    (plist-put column :width (max size esqlite-mode--cell-min-width))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (not (eobp))
          (loop repeat col
                do (esqlite-table-mode-next-column))
          (esqlite-table-mode--replace-current-cell
           (esqlite-table-mode-current-value))
          (forward-line 1))))
    (set-buffer-modified-p modified)))

(defun esqlite-table-mode-last-line-p ()
  (eq (point-at-bol 2) (point-max)))

(defun esqlite-table-mode-first-line-p ()
  (eq (point-at-bol) (point-min)))

(defun esqlite-table-mode--goto-column (column-name)
  (let ((pos (point-at-bol)))
    (catch 'done
      (while pos
        (let* ((cell (get-text-property pos 'esqlite-mode-cell))
               (colnm (plist-get (plist-get cell :column) :name)))
          ;;TODO it's ok? should be remove column/row model
          (when (equal column-name colnm)
            (throw 'done t))
          (setq pos (esqlite-table-mode--next-cell t pos)))))
    (when pos
      (goto-char pos))))

;; no need to make local
(defvar esqlite-table-mode--moved-column nil)

(defun esqlite-table-mode--move-line (arg)
  (let* ((cell (get-text-property (point) 'esqlite-mode-cell))
         (column (plist-get cell :column))
         (colnm (plist-get column :name))
         (colpos (if (memq last-command
                           '(esqlite-table-mode-previous-row
                             esqlite-table-mode-next-row))
                     esqlite-table-mode--moved-column
                   (current-column))))
    ;; eobp regard as last line
    (when (and (eobp) (plusp arg))
      (setq arg (1- arg)))
    (let ((rest (forward-line arg)))
      (when (or (not (zerop rest))
                ;; ignore eob as a valid esqlite data row
                (eobp))
        (let* ((pager (if (minusp rest)
                          (lambda (arg)
                            (esqlite-table-mode-backward-page arg)
                            (goto-char (point-max)))
                        (lambda (arg)
                          (esqlite-table-mode-forward-page arg))))
               (page-row esqlite-table-mode--page-rows)
               (page (1+ (/ (abs rest) page-row)))
               (line (% rest page-row)))
          (let ((inhibit-redisplay t))
            (funcall pager page))
          (forward-line line))))
    (setq esqlite-table-mode--moved-column colpos)
    (cond
     (colnm
      (esqlite-table-mode--goto-column colnm))
     (colpos
      (move-to-column colpos)))))

(defun esqlite-table-mode--pre-command ()
  (condition-case err
      (progn
        )
    (error
     (esqlite-mode-log "%s" err))))

(defun esqlite-table-mode--post-command ()
  (unwind-protect
      (condition-case err
          (let (model-err)
            ;; Update model
            (condition-case err2
                (esqlite-table-mode--post-change-row)
              (error
               (setq model-err err2)))
            ;; Update view
            (esqlite-table-mode--highlight-selected)
            (esqlite-table-mode--show-cell-after-move)
            (esqlite-table-mode--show-error-at-point)
            (esqlite-table-mode--delayed-draw-header)
            (when model-err
              (signal (car model-err) (cdr model-err))))
        (error
         (esqlite-mode-log "%s" err)
         (esqlite-table-mode--message "%s" err)))
    (esqlite-table-mode--finish-command)))

(defun esqlite-table-mode--finish-command ()
  (ignore-errors
    (setq esqlite-table-mode--processing-row
          (get-text-property (point) 'esqlite-mode-row))))

(defun esqlite-table-mode--message (fmt &rest args)
  (let ((msg (propertize (apply 'format fmt args) 'esqlite-mode-message t))
        (dispmsg (current-message))
        logmsg)
    (when dispmsg
      (let ((start
             (if (get-text-property 0 'esqlite-mode-message dispmsg)
                 0
               (next-single-property-change 0 'esqlite-mode-message dispmsg)))
            end)
        (while start
          (setq end (next-single-property-change start 'esqlite-mode-message dispmsg (length dispmsg)))
          (setq logmsg (concat logmsg (substring dispmsg start end) " "))
          (setq start (next-single-property-change end 'esqlite-mode-message dispmsg)))))
    (if logmsg
        (message "%s" (format "%s %s" logmsg msg))
      (message "%s" msg))))

(defvar esqlite-table-mode--processing-row nil)
(make-variable-buffer-local 'esqlite-table-mode--processing-row)

;;TODO rename
(defun esqlite-table-mode--pre-handle-rowid ()
  ;;TODO
  ;; (signal 'quit nil)
  )

(defun esqlite-table-mode--post-change-row ()
  (when (and esqlite-table-mode--processing-row
             (not (eq esqlite-table-mode--processing-row
                      (get-text-property (point) 'esqlite-mode-row))))
    (esqlite-table-mode--maybe-apply-changes)))

(defmacro esqlite-table-mode--update-with-handler (&rest form)
  "Update current row with FORM."
  `(condition-case err
       (progn
         ,@form
         (esqlite-table-mode--clear-error))
     (error
      (esqlite-table-mode--put-error (format "%s" (cdr err)))
      (signal (car err) (cdr err)))))

(defun esqlite-table-mode--sync-row (point row)
  ;; FIXME when table have _rowid_, oid, rowid all meta columns.
  (let* ((new-rowid (esqlite-table-mode--new-rowid row))
         (schema (esqlite-table-mode-ref :source :schema))
         (query (esqlite-format
                 "SELECT %o, %O FROM %o WHERE %o = %V"
                 (esqlite-table-mode-ref :source :rowid)
                 (mapcar (lambda (x) (nth 1 x)) schema)
                 (esqlite-table-mode-ref :source :name)
                 (esqlite-table-mode-ref :source :rowid)
                 new-rowid))
         (data (esqlite-mode-query query)))
    (save-excursion
      (goto-char point)
      (let ((inhibit-read-only t))
        (delete-region (point-at-bol) (point-at-eol))
        (esqlite-table-mode--insert-row (car data))))))

(defun esqlite-table-mode--new-rowid (row)
  (let* ((schema (esqlite-table-mode-ref :source :schema))
         (keys (esqlite--filter (lambda (x) (nth 5 x)) schema)))
    (cond
     ((null (car row))
      ;; rowid nil means INSERTed the ROW
      (let* ((last (esqlite-mode-query "SELECT LAST_INSERT_ROWID()")))
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

(defun esqlite-table-mode--row-is-modified (row)
  (loop for (name source edit) in (cdr row)
        unless (or (null edit) (equal source edit))
        return t))

(defun esqlite-table-mode--show-error-at-point ()
  (when (and esqlite-table-mode--processing-row
             (not (eq esqlite-table-mode--processing-row
                      (get-text-property (point) 'esqlite-mode-row))))
    (let ((errmsg (esqlite-table-mode--get-error)))
      (cond
       ((null errmsg))
       (t
        (esqlite-table-mode--message "%s" errmsg))))))

(defun esqlite-table-mode--show-cell-after-move ()
  (when (get-buffer-window esqlite-table-mode--cell-buffer)
    (let ((value (esqlite-table-mode-current-value)))
      (when value
        (let ((type (esqlite-table-mode-current-type)))
          (esqlite-table-mode--create-cell-buffer value type))))))

(defun esqlite-table-mode--highlight-selected ()
  (let ((ov (or esqlite-table-mode--highlight-overlay
                (let ((tmp (make-overlay (point-max) (point-max))))
                  (overlay-put tmp 'face 'esqlite-selected-face)
                  tmp))))
    (cond
     ((memq ov (overlays-at (point))))
     ((get-text-property (point) 'esqlite-mode-cell (current-buffer))
      (let* ((region (esqlite-table-mode--cell-region (point)))
             (start (car region))
             (end (cdr region)))
        (move-overlay ov start end)
        (setq esqlite-table-mode--highlight-overlay ov)))
     (t
      (move-overlay ov (point-max) (point-max))))))

(defun esqlite-table-mode--cell-region (point)
  (esqlite-mode-property-region point 'esqlite-mode-cell t))


(defun esqlite-table-mode--previous-cell (point &optional current-line)
  (let ((region (esqlite-table-mode--cell-region point)))
    (cond
     ((null region)
      (previous-single-property-change
       point 'esqlite-mode-cell nil
       (if current-line (point-at-bol) (point-min))))
     ((eq (car region) (point-min))
      (point-min))
     (t
      (esqlite-table-mode--previous-cell (1- (car region)) current-line)))))

(defun esqlite-table-mode--next-cell (&optional current-line point)
  (let* ((pos (or point (point)))
         (region (esqlite-table-mode--cell-region pos))
         (next (next-single-property-change
                (or (cdr region) pos)
                'esqlite-mode-cell nil)))
    (cond
     ((null next) nil)
     ((and current-line
           (>= next (point-at-eol)))
      nil)
     (t next))))

(defun esqlite-table-mode--truncate-text (width text)
  (truncate-string-to-width text width nil ?\s t))

(defun esqlite-table-mode--maybe-apply-changes ()
  (save-excursion
    (let* ((row (esqlite-table-mode--convert-row
                 esqlite-table-mode--processing-row))
           (found (esqlite-table-mode--goto-row
                   esqlite-table-mode--processing-row)))
      (cond
       ((not found))
       ((null (car row))
        (esqlite-table-mode--update-with-handler
         (let ((sql (esqlite-table-mode--insert-sql row)))
           (esqlite-table-mode--message "Inserting...")
           (esqlite-mode--execute-sql sql)
           (esqlite-table-mode--sync-row found row)
           (esqlite-table-mode--message "Done.")))
        (setq esqlite-table-mode--processing-row nil))
       ((esqlite-table-mode--row-is-modified row)
        (esqlite-table-mode--update-with-handler
         (let ((sql (esqlite-table-mode--update-sql row)))
           (esqlite-table-mode--message "Updating...")
           (esqlite-mode--execute-sql sql)
           (esqlite-table-mode--sync-row found row)
           (esqlite-table-mode--message "Done.")))
        (setq esqlite-table-mode--processing-row nil))
       (t
        (esqlite-table-mode--clear-error))))))

(defun esqlite-table-mode--find-row (pred)
  (let ((first (point))
        (col (current-column)))
    (goto-char (point-min))
    (loop while (not (eobp))
          if (let ((row (get-text-property (point) 'esqlite-mode-row)))
               (funcall pred row))
          return (progn (move-to-column col) (point))
          do (forward-line 1)
          finally (progn (goto-char first) nil))))

(defun esqlite-table-mode--goto-rowid (rowid)
  (esqlite-table-mode--find-row
   (lambda (row)
     (equal rowid (car-safe row)))))

(defun esqlite-table-mode--goto-row (search)
  (esqlite-table-mode--find-row
   (lambda (row)
     (eq search row))))

(defun esqlite-table-mode--convert-row (row)
  (cons
   (plist-get row :rowid)
   (loop for cell in (plist-get row :cells)
         collect (let ((col (plist-get cell :column)))
                   (list
                    (plist-get col :name)
                    (plist-get cell :source-value)
                    (plist-get cell :edit-value))))))

(defun esqlite-table-mode--delayed-draw-header ()
  ;; In the `post-command-hook', `window-hscroll' function still return
  ;; previous value. Although after calling `scroll-right' return correct value.
  (let ((buffer (current-buffer)))
    (run-with-timer 0.1 nil 'esqlite-table-mode--draw-header buffer)))

(defun esqlite-table-mode--draw-header (buffer)
  (with-local-quit
    (cancel-function-timers 'esqlite-table-mode--draw-header)
    (with-current-buffer buffer
      (let* ((disp-headers
              (loop with hscroll = (window-hscroll)
                    ;;  set t after first displaying column in window
                    with flag
                    for col in (esqlite-table-mode-ref :view :columns)
                    ;; add `esqlite-table-mode-header-column-separator' width
                    sum (1+ (plist-get col :width)) into right
                    if (< hscroll right)
                    collect (let ((name (plist-get col :name))
                                  (wid (plist-get col :width)))
                              (unless flag
                                (setq wid (- right hscroll 1)))
                              (setq flag t)
                              (cond
                               ((< wid esqlite-mode--cell-min-width)
                                ;; Beginning of line header may have too short
                                ;;  length of name.
                                (make-string wid ?\s))
                               (t
                                (propertize
                                 (esqlite-table-mode--truncate-text
                                  wid name)
                                 'face 'esqlite-mode-column-face))))))
             (filler (make-string (frame-width) ?\s))
             (tail (esqlite-mode--propertize-background-header filler))
             (separator esqlite-table-mode-header-column-separator)
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

(defun esqlite-table-mode--insert-row (row &optional clone)
  ;; (car row) is ROWID
  (let ((rowobj (list :rowid (car row) :cells nil))
        (cells nil))
    (loop for v in (cdr row)
          for i from 0
          do (progn
               (when (> i 0)
                 (insert " "))
               (let ((cell (esqlite-table-mode--insert-cell v i nil)))
                 (setq cells (cons cell cells))
                 (if clone
                     (plist-put cell :edit-value v)
                   (plist-put cell :source-value v)))))
    (plist-put rowobj :cells (nreverse cells))
    (put-text-property
     (point-at-bol) (point-at-eol)
     'esqlite-mode-row rowobj)))

(defun esqlite-table-mode--insert-cell (value index &optional cell)
  (let* ((start (point))
         (column (nth index (esqlite-table-mode-ref :view :columns)))
         (wid (plist-get column :width))
         (truncated (esqlite-table-mode--truncate-insert value wid))
         (cell (or cell
                   (list :edit-value nil :truncated truncated
                         :column column))))
    (let ((end (point)))
      (put-text-property start end 'esqlite-mode-cell cell)
      cell)))

(defun esqlite-table-mode--truncate-insert (value width)
  "Insert VALUE after point restricted by WIDTH."
  (let* ((pos-beg (point))
         (text (esqlite-mode--to-text value))
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
                                    (- (point) esqlite-mode--cell-min-width))))
                    (delete-region start end))
                  (insert (make-string (- next-column (current-column)) ?\.))
                  t)
                 ((not (equal text value)) t)
                 (t nil))))
          ;; visualize NULL value
          (when (eq value :null)
            (put-text-property pos-beg (point) 'face 'esqlite-null-face))
          truncated)))))

(defun esqlite-table-mode--column-index ()
  (let* ((curr (get-text-property (point) 'esqlite-mode-cell))
         (prev (esqlite-table-mode--previous-cell (point) t))
         (cell (get-text-property
                (max prev (point-at-bol))
                'esqlite-mode-cell))
         (column (plist-get cell :column))
         (index (plist-get column :index)))
    (if (eq curr cell)
        0
      (1+ index))))

(defun esqlite-table-mode--replace-current-cell (value)
  (let* ((pos (point))                  ;save position
         (region (esqlite-table-mode--cell-region pos)))
    (unless region
      (user-error "Not a cell"))
    (let* ((cell (get-text-property pos 'esqlite-mode-cell))
           (column (plist-get cell :column))
           (index (plist-get column :index))
           (row (get-text-property pos 'esqlite-mode-row))
           ;; save rest of line with properties
           (rest (buffer-substring (cdr region) (point-at-eol))))
      (delete-region (car region) (point-at-eol))
      (esqlite-table-mode--insert-cell value index cell)
      (plist-put cell :edit-value value)
      (put-text-property (point-at-bol) (point-at-eol)
                         'esqlite-mode-row row)
      (insert rest))
    ;; restore previous position
    (goto-char pos)))

(defun esqlite-table-mode-redraw-page ()
  (let ((start (window-start))
        (pos (point)))
    (esqlite-table-mode--draw-page)
    (goto-char pos)
    (set-window-start (selected-window) start)))

(defun esqlite-table-mode--delay-max-page (buffer)
  (with-local-quit
    (cancel-function-timers 'esqlite-table-mode--delay-max-page)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((count (esqlite-table-mode-max-page)))
          (esqlite-table-mode-set count :cache :max-page))))))

(defun esqlite-table-mode-max-page ()
  (let* ((query (esqlite-format
                 '("SELECT ROUND((COUNT(*) / %s) + 0.5)"
                   " FROM %o "
                   " WHERE %s")
                 esqlite-table-mode--page-rows
                 (esqlite-table-mode-ref :source :name)
                 (or (esqlite-table-mode-ref :source :view :where) "1 = 1")))
         (data (esqlite-mode-query query))
         (max (caar data)))
    (and (string-match "^\\([0-9]+\\)" max)
         (string-to-number (match-string 1 max)))))

(defun esqlite-table-mode--schema-rowid (schema)
  (loop for r in esqlite--rowid-columns
        unless (loop for x in schema
                     if (equal r (upcase (nth 1 x)))
                     return x)
        return r
        finally (error "Unable take valid ROWID column")))

(defun esqlite-table-mode--clear-page ()
  ;;TODO completely remove
  ;; (esqlite-table-mode-set nil :cache :max-page)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays (point-min) (point-max)))

(defun esqlite-table-mode-jump-to-rowid (rowid)
  ;;TODO sample query
  ;; "SELECT ROWID,* FROM %o{table} ORDER BY ROWID LIMIT 100 OFFSET 100 * %s{page}"
  (let ((src-view (copy-sequence (esqlite-table-mode-ref :source :view)))
        (page (esqlite-table-mode--get-rowid-page rowid)))
    (plist-put src-view :page page)
    (esqlite-table-mode--draw-page src-view)))

(defun esqlite-table-mode--move-page (page error-message)
  (let ((src-view (copy-sequence (esqlite-table-mode-ref :source :view))))
    ;;TODO may exceed max page
    (plist-put src-view :page page)
    (esqlite-table-mode--draw-page src-view)
    (cond
     ((equal page (esqlite-table-mode-ref :source :view :page))
      ;; succeeded
      ;; redraw header forcibly
      (esqlite-table-mode--delayed-draw-header))
     (t
      ;; TODO no ding?
      (ding)
      (message "%s" error-message)))))

(defun esqlite-table-mode--load-schema (name)
  (let* ((schema (esqlite-table-mode--schema name))
         (rowid-name (esqlite-table-mode--schema-rowid schema))
         (source (esqlite-table-mode-ref :source)))
    (plist-put source :name name)
    (plist-put source :schema schema)
    (plist-put source :rowid rowid-name)))

(defun esqlite-table-mode--schema (table)
  (or
   (and (equal (esqlite-table-mode-ref :source :name) table)
        (esqlite-table-mode-ref :source :schema))
   (esqlite-read-table-schema (esqlite-mode-ref :stream) table)))

;;TODO bad!! fuckoff my head
(defun esqlite-table-mode--get-rowid-page (rowid &optional source-view)
  (let* ((query (esqlite-table-mode--build-getting-page rowid source-view))
         (data (esqlite-mode-query query)))
    (string-to-number (caar data))))

(defun esqlite-table-mode--build-getting-page (rowid &optional source-view)
  (let* ((source (esqlite-table-mode-ref :source))
         (src-view (or source-view (plist-get source :view)))
         (tablename (plist-get source :name))
         (rowid-name (plist-get source :rowid))
         (where (or (plist-get src-view :where) "1 = 1"))
         (orders (plist-get src-view :orders))
         (order-by (esqlite-table-mode--compile-orders orders)))
    (esqlite-format
     `("SELECT ((COUNT(*) - 1)/ 100) "
       " FROM %o{tablename} "
       " WHERE %o{rowid-name} <= %V{rowid}"
       "  AND %s{where} "
       ,@(and orders
              `(" ORDER BY %s{order-by}"))))))

;;TODO
(defun esqlite-table-mode--compile-preceeding-where (orders)
  (loop for order in orders
        do (let ((column (car order))
                 (term (cadr order))))))

(defun esqlite-table-mode--build-paging-query (source-view)
  (let* ((source (esqlite-table-mode-ref :source))
         (tablename (plist-get source :name))
         (schema (plist-get source :schema))
         (columns (mapcar (lambda (x) (nth 1 x)) schema))
         (rowid-name (plist-get source :rowid))
         (page-row esqlite-table-mode--page-rows)
         (page (or (plist-get source-view :page) 0))
         (where (or (plist-get source-view :where) "1 = 1"))
         (orders (plist-get source-view :orders))
         (order-by (esqlite-table-mode--compile-orders orders)))
    (esqlite-format
     `("SELECT %o{rowid-name}, %O{columns}"
       " FROM %o{tablename} "
       " WHERE %s{where}"
       ,@(and orders
              `(" ORDER BY %s{order-by}"))
       " LIMIT %s{page-row} OFFSET %s{page-row} * %s{page}"))))

;;TODO refactor
;;todo remove force
(defun esqlite-table-mode--draw-page (&optional source-view)
  (unless source-view
    (setq source-view (esqlite-table-mode-ref :source :view)))
  (save-excursion
    (let* ((schema (esqlite-table-mode-ref :source :schema))
           (query (esqlite-table-mode--build-paging-query source-view))
           (data (esqlite-mode-query query)))
      (cond
       (data
        (esqlite-table-mode--clear-page)
        (esqlite-table-mode--set-header schema data)
        (let ((inhibit-read-only t))
          ;; ignore header row
          (dolist (row data)
            (esqlite-table-mode--insert-row row)
            (insert "\n"))
          (unless(memq (esqlite-mode-stream-status) '(transaction))
            (set-buffer-modified-p nil)))
        (setq buffer-read-only t)
        ;;TODO only check first time? when move forward non exists page?
        (unless (esqlite-table-mode-ref :cache :max-page)
          (run-with-idle-timer
           1 nil 'esqlite-table-mode--delay-max-page
           (current-buffer))))
       ((> (plist-get source-view :page) 0)
        ;; TODO last page have no data.
        (plist-put source-view :page (1- (plist-get source-view :page))))
       (t
        (esqlite-table-mode--clear-page)
        (esqlite-table-mode--set-header schema)
        (esqlite-table-mode-set 0 :cache :max-page)
        (set-buffer-modified-p nil)))
      (esqlite-table-mode-set source-view :source :view))))

(defun esqlite-table-mode--set-header (schema &optional data)
  (let* ((lis (cons
               (cons
                "ROWID"                 ;dummy
                (loop for def in schema
                      collect (nth 1 def)))
               data))
         (width-def (esqlite-mode--calculate-width lis))
         (headers (car lis))
         (columns (loop for max in (cdr width-def)
                        for hdr in (cdr headers)
                        for idx from 0
                        collect (list :name hdr
                                      :index idx
                                      :initial-max max
                                      :width (min max 30)))))
    (esqlite-table-mode-set columns :view :columns)))

;;TODO
(defun esqlite-table-mode-isearch ()
  (let ((timer (run-with-idle-timer
                1 t 'esqlite-table-mode--delayed-search
                (current-buffer))))
    (unwind-protect
        (read-from-minibuffer "Search: ")
      (cancel-timer timer))))

(defun esqlite-table-mode--delayed-search (buffer)
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
(defconst esqlite-table-mode--filters-operators
  '(
    (and filter)
    (or filter)))

(defconst esqlite-table-mode--filter-operators
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

(defun esqlite-table-mode--read-filters (&optional column)
  (completing-read "" esqlite-table-mode--filters-operators nil t)
  (loop with filter
        while (setq filter (esqlite-table-mode--read-filter))
        collect filter into res
        finally return res))

(defun esqlite-table-mode--read-filter ()
  (let* ((filter-defs esqlite-table-mode--filter-operators)
         (operator (intern-soft (completing-read
                                 "Filter operator: " filter-defs
                                 nil t)))
         (filter-def (assq operator esqlite-table-mode--filter-operators)))
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

(defun esqlite-table-mode--compile-orders (orders)
  (mapconcat
   'esqlite-table-mode--compile-order
   orders
   ", "))

(defun esqlite-table-mode--compile-order (order)
  (let* ((column (car order))
         (term (cadr order))
         (ordering-term (if (eq term :desc) "DESC" "ASC")))
    (esqlite-format "%o{column} %s{ordering-term}")))

;;TODO reconsider it
;; FILTERS ::= (AND FILTER ...) | (OR FILTER ...)
;; FILTER ::= (COLUMN OPERATOR) | (COLUMN OPERATOR EXP ...) | FILTERS

;; OPERATOR ::= car of `esqlite-table-mode--filter-operators'
;; EXP    ::= string | number

;;TODO number and string (quote)

(defun esqlite-table-mode--compile-filters (filters)
  (concat
   "("
   (mapconcat
    'esqlite-table-mode--compile-filter
    (cdr filters)
    (cond
     ((eq (car filters) 'or)  " OR ")
     ((eq (car filters) 'and) " AND ")
     (t (error "Invalid filters %s" filters))))
   ")"))

(defun esqlite-table-mode--compile-filter (filter)
  (cond
   ((memq (car filter) '(or and))
    (esqlite-table-mode--compile-filters filter))
   ((and (listp filter) (>= (length filter) 2))
    (let* ((column (car filter))
           (operator (cadr filter))
           (exps (cddr filter))
           (filter-def (assq operator esqlite-table-mode--filter-operators)))
      (unless filter-def
        (error "Invalid operator %s" operator))
      (concat
       (esqlite-format-object column)
       " "
       (loop for def in (cdr filter-def)
             collect (cond
                      ((eq def 'exp)
                       (let ((exp (pop exps)))
                         (unless (or (stringp exp)
                                     (numberp exp))
                           (error "Invalid EXP %s" exp))
                         (esqlite-format-value exp)))
                      ((stringp def) def)
                      (t (error "Invalid definition %s" def)))
             into res
             finally return (mapconcat 'identity res " ")))))
   (t
    (error "Invalid filter %s" filter))))

;;
;; Automated DML
;;

(defun esqlite-table-mode--update-sql (row)
  (esqlite-format
   "UPDATE %o SET %s WHERE %o = %V;"
   (esqlite-table-mode-ref :source :name)
   (mapconcat
    'identity
    (loop for (name source edit) in (cdr row)
          if edit
          collect (esqlite-format "%o = %V" name edit))
    ", ")
   (esqlite-table-mode-ref :source :rowid) (car row)))

(defun esqlite-table-mode--insert-sql (row)
  (let (columns values)
    (loop for (name source edit) in (cdr row)
          do (setq columns (cons name columns)
                   values (cons (or edit "") values)))
    (esqlite-format
     "INSERT INTO %o (%O) VALUES (%V);"
     (esqlite-table-mode-ref :source :name)
     columns values)))

(defun esqlite-table-mode--delete-sql (row)
  (esqlite-format
   "DELETE FROM %o WHERE %o = %V;"
   (esqlite-table-mode-ref :source :name)
   (esqlite-table-mode-ref :source :rowid) (car row)))

;;TODO consider key exchange
(defun esqlite-table-mode--key-cond (keys row)
  (loop for k in keys
        collect (destructuring-bind (_ source _) (assoc k (cdr row))
                  (esqlite-format "%o = %V" k source))
        into res
        finally return (mapconcat 'identity res " AND ")))

(defun esqlite-table-mode--schema-have-key ()
  (let ((schema (esqlite-table-mode-ref :source :schema)))
    (loop for (_ name _ notnull _ primaryp) in schema
          if (and primaryp (not notnull))
          return nil
          if primaryp
          collect name)))

;;;
;;; esqlite-schema-mode
;;;

(defconst esqlite-schema-mode--close-icon "[+]")
(defconst esqlite-schema-mode--open-icon "[-]")

(defvar esqlite-schema-mode-map nil)

(unless esqlite-schema-mode-map

  (let ((map (make-sparse-keymap)))

    (set-keymap-parent map esqlite-mode-map)

    (define-key map "\C-m" 'esqlite-schema-mode-toggle-item)
    (define-key map " " 'esqlite-schema-mode-toggle-item)
    (define-key map "C" 'esqlite-schema-mode-create-definition)
    (define-key map "V" 'esqlite-schema-mode-open-table)

    (define-key map "k" 'esqlite-schema-mode-previous-line)
    (define-key map "j" 'esqlite-schema-mode-next-line)
    (define-key map "p" 'esqlite-schema-mode-previous-line)
    (define-key map "n" 'esqlite-schema-mode-next-line)

    (setq esqlite-schema-mode-map map)))

(defconst esqlite-schema-mode-menu-spec
  `("Esqlite"
    ,@esqlite-mode-menu-spec
    ["View table" esqlite-schema-mode-open-table t]
    ["Open/Close item" esqlite-schema-mode-toggle-item t]
    ["Copy DDL" esqlite-schema-mode-create-definition t]
    ))

(easy-menu-define esqlite-schema-mode-menu
  esqlite-schema-mode-map
  "Menu used in Esqlite schema mode."
  esqlite-schema-mode-menu-spec)

(define-derived-mode esqlite-schema-mode esqlite-mode "Esqlite Schema"
  "Esqlite schema view mode"
  (use-local-map esqlite-schema-mode-map)
  (set (make-local-variable 'revert-buffer-function)
       'esqlite-schema-mode-revert))

(put 'esqlite-schema-mode 'mode-class 'special)

(defun esqlite-schema-mode-open-table ()
  (interactive)
  (let ((item (get-text-property (point) 'esqlite-schema-item)))
    (unless item
      (user-error "No item is here"))
    (cond
     ((memq (plist-get item :type) '(table view))
      (esqlite-schema-mode--evacuate-buffer)
      (let ((name (plist-get item :name)))
        (esqlite-mode-open-table name)))
     (t
      (error "Cannot open %s as table" (plist-get item :type))))))

;;TODO test
;; TODO rename?
;;TODO read QUERY
;; TODO cleanup when switch to schema view
;;    DROP VIEW IF EXISTS %o{view-name};
(defun esqlite-mode-open-query (query)
  (interactive)
  (let* ((stream (esqlite-mode-ref :stream))
         (view-name (esqlite--unique-name stream "tempview"))
         ;; create TEMP VIEW to read QUERY like a table
         ;; http://www.sqlite.org/lang_createview.html
         (query (esqlite-format
                 '("CREATE TEMP VIEW %o{view-name}"
                   " AS %s{query}"))))
    (esqlite-mode-query query)
    (esqlite-schema-mode--evacuate-buffer)
    (esqlite-mode-open-table view-name)))

(defun esqlite-schema-mode-toggle-item ()
  (interactive)
  (let ((item (get-text-property (point) 'esqlite-schema-item)))
    (unless item
      (user-error "No item is here"))
    (let ((region (esqlite-mode-property-region
                   (point) 'esqlite-schema-item))
          (child (plist-get item :child))
          (type (plist-get item :type))
          (inhibit-read-only t)
          (modifiedp (buffer-modified-p)))
      (cond
       ((and child (not (overlay-get child 'invisible)))
        (overlay-put child 'invisible t)
        (esqlite-schema-mode--set-close region))
       ((overlayp child)
        (overlay-put child 'invisible nil)
        (esqlite-schema-mode--set-open region))
       (t
        (esqlite-mode--check-stream)
        (let ((ov (cond
                   ((eq type 'table-heading)
                    (esqlite-schema-mode--draw-objects
                     'esqlite-read-tables 'table))
                   ((eq type 'view-heading)
                    (esqlite-schema-mode--draw-objects
                     'esqlite-read-views 'view))
                   ((eq type 'index-heading)
                    (esqlite-schema-mode--draw-objects
                     'esqlite-read-indexes 'index))
                   ((eq type 'trigger-heading)
                    (esqlite-schema-mode--draw-objects
                     'esqlite-read-triggers 'trigger))
                   ((memq type '(table view))
                    (esqlite-schema-mode--draw-table item))
                   ;; TODO
                   (t (error "Not a supported type `%s'" type)))))
          ;;TODO
          ;; (overlay-put ov 'isearch-open-invisible (lambda (x)))
          (plist-put item :child ov))
        (esqlite-schema-mode--set-open region)))
      (set-buffer-modified-p modifiedp))))

(defun esqlite-schema-mode-next-line ()
  (interactive)
  (forward-line 1))

(defun esqlite-schema-mode-previous-line ()
  (interactive)
  (forward-line -1))

(defun esqlite-schema-mode-create-definition ()
  (interactive)
  (let* ((item (get-text-property (point) 'esqlite-schema-item))
         (name (plist-get item :name))
         (type (plist-get item :type))
         (results
          (cond
           ((memq type '(table view index trigger))
            (let ((typename (symbol-name type)))
              (esqlite-mode-query
               (esqlite-format
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

(defun esqlite-schema-mode--restore-from-evacuation ()
  (let ((to-buf (current-buffer))
        (from-buf (esqlite-mode-ref :schemaview))
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

(defun esqlite-schema-mode--evacuate-buffer ()
  (let ((to-buf (esqlite-mode-ref :schemaview))
        (pos (point)))
    (when to-buf
      (kill-buffer to-buf))
    (setq to-buf (generate-new-buffer
                  (format " *esqlite schema %s* " buffer-file-name)))
    (esqlite-mode-set :schemaview to-buf)
    (append-to-buffer to-buf (point-min) (point-max))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (move-overlay ov (overlay-start ov) (overlay-end ov) to-buf))
    (with-current-buffer to-buf
      (goto-char pos))))

(defun esqlite-schema-mode-draw-view ()
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
                                 esqlite-schema-mode--close-icon
                                 (propertize name
                                             'face 'esqlite-mode-table-face)))
                 (put-text-property
                  start (point)
                  'esqlite-schema-item (list :type type))
                 (insert "\n"))))))

(defun esqlite-schema-mode-revert (&rest dummy)
  (esqlite-mode-open-schema-mode t))

(defun esqlite-schema-mode--switch-icon (start icon)
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

(defun esqlite-schema-mode--set-open (region)
  (esqlite-schema-mode--switch-icon
   (car region) esqlite-schema-mode--open-icon))

(defun esqlite-schema-mode--set-close (region)
  (esqlite-schema-mode--switch-icon
   (car region) esqlite-schema-mode--close-icon))

(defun esqlite-schema-mode--draw-objects (gatherer type)
  (save-excursion
    (let ((objects (funcall gatherer (esqlite-mode-ref :stream))))
      (forward-line 2)
      (loop with start = (point)
            for obj in objects
            do (let ((start (point))
                     (table (list :type type :name obj)))
                 (esqlite-schema-mode--draw-line
                  1
                  (format "%s %s\n"
                          esqlite-schema-mode--close-icon
                          ;;TODO change face-name
                          (propertize obj 'face 'esqlite-mode-object-face)))
                 (put-text-property
                  start (point)
                  'esqlite-schema-item table))
            finally return (make-overlay start (point))))))

(defun esqlite-schema-mode--draw-table (parent)
  (save-excursion
    (let* ((name (plist-get parent :name))
           (schema (or (plist-get parent :schema)
                       (let ((s (esqlite-schema-mode--table-schema name)))
                         (plist-put parent :schema s)
                         s))))
      (forward-line 1)
      (let* ((headers '("NAME" "TYPE" "NULL" "DEFAULT" "KEY"))
             (start (point))
             (data (cons headers schema))
             (width-def (esqlite-mode--calculate-width data))
             (hdrs (loop for h in headers
                         for w in width-def
                         collect (let ((name
                                        (truncate-string-to-width h w nil ?\s)))
                                   (propertize
                                    name
                                    'face 'esqlite-header-background)))))
        (insert "\n")
        (esqlite-schema-mode--draw-line
         2
         (mapconcat 'identity hdrs " "))
        (loop for (name type notnull default primaryp) in schema
              do (progn
                   (esqlite-schema-mode--draw-line
                    2
                    (esqlite-schema-mode--table-column
                     width-def name type notnull default primaryp))))
        ;; column items are branch of the view.
        (put-text-property start (point) 'esqlite-schema-item parent)
        (make-overlay start (point))))))

(defun esqlite-schema-mode--table-column (def &rest values)
  (let ((texts (loop for w in def
                     for v in values
                     collect (truncate-string-to-width
                              (esqlite-mode--to-text v) w nil ?\s))))
    (mapconcat
     'identity
     (cons
      (propertize (car texts) 'face 'esqlite-mode-column-face)
      (cdr texts)) " ")))

(defconst esqlite-schema-mode-indent-level 2)

(defun esqlite-schema-mode--draw-line (level line)
  (insert (make-string (* level esqlite-schema-mode-indent-level) ?\s))
  (insert line)
  (insert "\n"))

;; TODO rename esqlite-schema-view (?)
(defun esqlite-schema-mode--table-schema (name)
  (loop for (idx name type notnull default primaryp)
        in (esqlite-read-table-schema
            (esqlite-mode-ref :stream) name)
        collect (list name type
                      (if notnull "no" "yes")
                      (or default "no")
                      (or (and primaryp "*") ""))))

;;;
;;; Esqlite binary mode (TODO no need?)
;;;

(defvar esqlite-binary-mode-map nil)
(unless esqlite-binary-mode-map
  (let ((map (make-sparse-keymap)))

    (suppress-keymap map)

    (define-key map "\C-c\C-c" 'esqlite-mode-toggle-view)

    (setq esqlite-binary-mode-map map)))

(define-minor-mode esqlite-binary-mode
  ""
  nil nil esqlite-binary-mode-map)

(defun esqlite-mode-toggle-view ()
  "Toggle esqlite view <-> binary view"
  (interactive)
  (cond
   (esqlite-binary-mode
    (esqlite-view))
   (t
    (esqlite-binary-view))))

(defun esqlite-binary-view ()
  (let ((magic-mode-alist
         (delq nil (mapcar
                    (lambda (elt)
                      (unless (eq (cdr elt) 'esqlite-view)
                        elt))
                    magic-mode-alist)))
        (read-only buffer-read-only))
    ;; warn if file is too huge and may exit.
    (find-alternate-file buffer-file-name)
    (when read-only
      (setq buffer-read-only t))
    (esqlite-binary-mode 1)))

;;;
;;; Esqlite cell mode
;;;

;;TODO display schema TYPE. Do Not prohibit input TYPE.

(defvar esqlite-cell-mode-map nil)
(unless esqlite-cell-mode-map

  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-t" 'esqlite-cell-mode-toggle-null)
    (define-key map "\C-c\C-c" 'exit-recursive-edit)
    (define-key map "\C-x\C-s" 'exit-recursive-edit)
    (define-key map "\C-c\C-k" 'abort-recursive-edit)

    (setq esqlite-cell-mode-map map)))

(defun esqlite-cell-mode-setup ()
  (setq mode-line-process
        `((:eval
           (concat
            " "
            (if (and esqlite-cell-mode--type
                     (not (string= esqlite-cell-mode--type "")))
                (propertize
                 esqlite-cell-mode--type
                 'face 'esqlite-mode-object-face)
              (propertize "unknown" 'face 'esqlite-null-face))))
          (:eval
           (and (eq (esqlite-cell-mode-value) :null)
                (concat " " (propertize "null" 'face 'esqlite-null-face))))))
  (setq mode-name "Esqlite Cell Edit")
  (use-local-map esqlite-cell-mode-map))

(defvar esqlite-cell-mode--null nil)
(defvar esqlite-cell-mode--type nil)

(defun esqlite-cell-mode-toggle-null ()
  "Toggle input value between NULL and some value."
  (interactive)
  (cond
   ((null esqlite-cell-mode--null)
    (erase-buffer)
    (setq esqlite-cell-mode--null t)
    (message "Now cell value is NULL"))
   (t
    (setq esqlite-cell-mode--null nil)))
  (set-buffer-modified-p t))

(defun esqlite-cell-mode-value ()
  (cond
   ((> (buffer-size) 0)
    (buffer-string))
   (esqlite-cell-mode--null :null)
   (t "")))


;;;
;;; Esqlite database manager interface
;;;

;;;###autoload
(defun esqlite-find-file (file)
  "Open FILE as Esqlite database.

FYI Normally, esqlite database open automatically `esqlite-view' but
should not open huge file. This function support to open such file."
  (interactive "FEsqlite File: ")
  (cond
   ((file-exists-p file)
    (unless (esqlite-file-guessed-database-p file)
      (error "Not a valid database file")))
   ((not (y-or-n-p "File not exists. Create a Esqlite database? "))
    (signal 'quit nil)))
  (let ((buf (get-file-buffer file)))
    (unless buf
      (setq buf (create-file-buffer (file-name-nondirectory file)))
      (with-current-buffer buf
        (set-visited-file-name file)
        (esqlite-view)))
    (switch-to-buffer buf)))

;;;###autoload
(defun esqlite-view ()
  "View current buffer as a esqlite database."
  (interactive)
  (set-buffer-modified-p nil)
  (esqlite-mode-open-schema-mode t))

;;;
;;; Package load/unload
;;;

;;;###autoload
(setq magic-mode-alist
      `((,esqlite-file-header-regexp . esqlite-view)))

(defun esqlite-mode-unload-function ()
  (let ((pair (rassq 'esqlite-view magic-mode-alist)))
    (when pair
      (setq magic-mode-alist (delq pair magic-mode-alist)))))


;;;; TODO TESTING

(defun esqlite-create-alternate-table (stream create-sql)
  "Execute CREATE-SQL in STREAM. This function not begin transaction.
If you need transaction, begin transaction by your own before calling this function."
  (unless (let ((case-fold-search t))
            (string-match "^[ \t\n]*create[ \t\n]+table[ \t\n]+\\([^ \t\n]+\\)" create-sql))
    (user-error "Invalid create sql `%s'" create-sql))
  (let* ((table (match-string 1 create-sql))
         (temp-table (esqlite--unique-name stream table))
         (src-columns (mapcar
                       (lambda (x) (nth 1 x))
                       (esqlite-read-table-schema stream table))))
    (unless src-columns
      (error "Unable to get `%s' table columns" table))
    (let ((temp-create (esqlite-format
                        "CREATE TEMPORARY TABLE %o (%O)"
                        temp-table src-columns)))
      (esqlite-stream-execute stream temp-create))
    (let ((temp-insert (esqlite-format
                        "INSERT INTO %o SELECT %O FROM %o"
                        temp-table src-columns table)))
      (esqlite-stream-execute stream temp-insert))
    (let ((drop-object (esqlite-format
                        "DROP TABLE %o"
                        table)))
      (esqlite-stream-execute stream drop-object))
    (esqlite-stream-execute stream create-sql)
    (let* ((new-columns (mapcar
                         (lambda (x) (nth 1 x))
                         (esqlite-read-table-schema stream table)))
           (share-columns (delq nil
                                (mapcar
                                 (lambda (col)
                                   (and (member col src-columns)
                                        col))
                                 new-columns)))
           (insert-object (esqlite-format
                           "INSERT INTO %o (%O) SELECT %O FROM %o"
                           table share-columns share-columns
                           temp-table)))
      (esqlite-stream-execute stream insert-object))
    (let ((drop-temp (esqlite-format
                      "DROP TABLE %o"
                      temp-table)))
      (esqlite-stream-execute stream drop-temp))))

;;TODO non used
(defun esqlite-mode--faced-insert (face &rest args)
  (let ((start (point)))
    (apply 'insert args)
    (put-text-property start (point) 'face face)))

;;TODO non used
(defun esqlite-mode--clone-cond ()
  (copy-sequence esqlite-table-mode--context))

;; TODO :table :page :order :where


(provide 'esqlite-mode)

;;; esqlite-mode.el ends here
