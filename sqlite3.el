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

;; * Specification
;; allow invalid character in data.
;; disallow invalid character in column and table.

;;; Install:

;;TODO

;;; TODO:
;; * incremental search
;; * number to right.
;; * blob
;; * check sqlite3 command is exists.
;; * windows

;;; Code:

;;TODO when table is locked

(require 'pcsv)

(defgroup sqlite3 ()
  "Manipulate Sqlite3 Database."
  :group 'applications)

(defcustom sqlite3-program "sqlite3"
  ""
  :type 'file
  :group 'sqlite3)

(defvar sqlite3-mode-map nil)

(unless sqlite3-mode-map
  ;;TODO or is testing
  (let ((map (or sqlite3-mode-map (make-sparse-keymap))))

    (suppress-keymap map)

    (define-key map "\C-c\C-c" 'sqlite3-mode-toggle-display)
    (define-key map "\C-c\C-q" 'sqlite3-mode-send-query)
    (define-key map "\C-x\C-s" 'sqlite3-mode-commit-changes)
    (define-key map "\C-c\C-j" 'sqlite3-mode-jump-to-page)
    (define-key map "\C-c\C-n" 'sqlite3-mode-new-row)
    (define-key map "\C-c>" 'sqlite3-mode-forward-page)
    (define-key map "\C-c<" 'sqlite3-mode-backward-page)
    (define-key map "\C-i" 'sqlite3-mode-forward-cell)
    (define-key map "\e\C-i" 'sqlite3-mode-backward-cell)
    (define-key map "\C-m" 'sqlite3-mode-start-edit)
    (define-key map "g" 'revert-buffer)
    (define-key map "h" 'sqlite3-mode-previous-column)
    (define-key map "l" 'sqlite3-mode-next-column)
    (define-key map "k" 'sqlite3-mode-previous-row)
    (define-key map "j" 'sqlite3-mode-next-row)
    ;TODO
    (define-key map "\C-c\C-f" 'sqlite3-mode-narrow-down)
    (define-key map "\C-c\C-o" 'sqlite3-mode-open-table)

    (setq sqlite3-mode-map map)))

(defvar sqlite3-mode--stream nil)
(make-variable-buffer-local 'sqlite3-mode--stream)

(defvar sqlite3-mode-hook nil)

(defun sqlite3-mode ()
  (interactive)
  (unless buffer-file-name
    (error "Not a file buffer"))
  (kill-all-local-variables)
  (setq major-mode 'sqlite3-mode)
  (setq mode-name "Sqlite3")
  (setq buffer-undo-list t)
  (setq truncate-lines t)
  ;;TODO
  (set (make-local-variable 'backup-inhibited) t)
  (setq revert-buffer-function
        'sqlite3-mode-revert-buffer)
  (setq sqlite3-mode--current-page 0)
  (setq sqlite3-mode--current-table nil)
  (set-visited-file-modtime
   (nth 5 (file-attributes buffer-file-name)))
  (add-hook 'post-command-hook
            'sqlite3-mode--post-command nil t)
  (add-hook 'kill-buffer-hook
            'sqlite3-after-kill-buffer nil t)
  (use-local-map sqlite3-mode-map)
  (sqlite3-mode-setup-mode-line)
  (unless sqlite3-mode--popup-timer
    (setq sqlite3-mode--popup-timer
          (run-with-idle-timer
           1 t 'sqlite3-mode-popup-contents)))
  (run-mode-hooks 'sqlite3-mode-hook))

(defun sqlite3-mode-start-edit ()
  "Edit current cell with opening subwindow."
  (interactive)
  (let ((value (sqlite3-mode-current-value)))
    (unless value
      (error "No cell is here"))
    (unless (eq (sqlite3-mode-stream-status) 'transaction)
      (sqlite3-mode--transaction-begin))
    (let* ((pos (point))
           (region (or (sqlite3-text-property-region 
                        pos 'sqlite3-edit-value)
                       (sqlite3-text-property-region
                        pos 'sqlite3-source-value)))
           (new (sqlite3-mode-open-edit-window value)))
      (let ((inhibit-read-only t))
        (unless (equal value new)
          (goto-char pos)
          (let ((width (get-text-property pos 'sqlite3-mode-cell-width)))
            (sqlite3-mode--replace-current-cell new width))
          (put-text-property 
           (car region) (cdr region) 'sqlite3-edit-value
           new)
          (sqlite3-mode--highlight-selected))))))

;;TODO
(defun sqlite3-mode-commit-changes ()
  (interactive)
  (condition-case nil
      (sqlite3-mode--transaction-commit)
    (sqlite3-mode--transaction-rollback)))

;;TODO
(defun sqlite3-mode-reset ()
  (interactive)
  (when sqlite3-mode--stream
    (sqlite3-stream-close sqlite3-mode--stream)
    (setq sqlite3-mode--stream
          (sqlite3-stream-open buffer-file-name)))
  (sqlite3-mode-redraw-page))

;;TODO
;; raw-data <-> table-view
(defun sqlite3-mode-toggle-display ()
  (interactive)
  )

;;TODO
(defun sqlite3-mode-send-query (query)
  (interactive "sSQL: ")
  (sqlite3-mode--send-query query))

(defun sqlite3-mode-jump-to-page (page)
  "TODO"
  (interactive
   (list (read-number "Page: ")))
  (unless (sqlite3-mode-draw-page
           sqlite3-mode--current-table page)
    (error "No more next page")))

(defun sqlite3-mode-forward-page (&optional arg)
  "TODO"
  (interactive "p")
  (unless (sqlite3-mode-draw-page
           sqlite3-mode--current-table
           (+ sqlite3-mode--current-page arg))
    (error "No more next page")))

(defun sqlite3-mode-backward-page (&optional arg)
  "TODO"
  (interactive "p")
  (when (= sqlite3-mode--current-page 0)
    (error "This is a first page"))
  (unless (sqlite3-mode-draw-page 
           sqlite3-mode--current-table
           (- sqlite3-mode--current-page arg))
    (error "No more previous page")))

(defun sqlite3-mode-next-row (&optional arg)
  "TODO"
  (interactive "p")
  (sqlite3-mode--move-line arg)
  ;;TODO next page
  )

(defun sqlite3-mode-previous-row (&optional arg)
  "TODO"
  (interactive "p")
  (sqlite3-mode--move-line (- arg))
  ;;TODO prev page
  )

(defun sqlite3-mode-next-column ()
  "Goto next column."
  (interactive)
  (let ((next (sqlite3-mode--next-cell (point) t)))
    (when next
      (goto-char next))))

(defun sqlite3-mode-previous-column ()
  "Goto previous column."
  (interactive)
  (let ((prev (sqlite3-mode--previous-cell (point) t)))
    (when prev
      (goto-char prev))))

(defun sqlite3-mode-forward-cell ()
  "Forward cell over the row."
  (interactive)
  (let ((next (sqlite3-mode--next-cell (point))))
    (when next
      (goto-char next))))

(defun sqlite3-mode-backward-cell ()
  "Backward cell over the row."
  (interactive)
  (let ((prev (sqlite3-mode--previous-cell (point))))
    (when prev
      (goto-char prev))))

;;TODO name
(defun sqlite3-mode-narrow-down ()
  (interactive)
  ;;
  )

;;;
;;; `sqlite3-mode' functions
;;;

(defvar sqlite3-mode--popup-timer nil)

(defun sqlite3-mode--cleanup-timer ()
  (when sqlite3-mode--popup-timer
    (loop for b in (buffer-list)
          if (and (eq (buffer-local-value 'major-mode b) 'sqlite3-mode)
                  (buffer-live-p b))
          return t
          finally (progn 
                    (cancel-timer sqlite3-mode--popup-timer)
                    (setq sqlite3-mode--popup-timer nil)))))

(defun sqlite3-mode-current-value ()
  (or 
   (get-text-property (point) 'sqlite3-edit-value)
   (get-text-property (point) 'sqlite3-source-value)))

(defun sqlite3-mode-popup-contents ()
  (save-match-data
    ;;TODO only show current cell exceed window.
    (when (and (eq major-mode 'sqlite3-mode)
               ;; suppress tooltip if last command were C-g
               (not (eq last-command 'keyboard-quit)))
      (sqlite3-tooltip-show (sqlite3-mode-current-value)))))

(defconst sqlite3-mode-line-format
  '(
    (sqlite3-mode--current-table
     (:eval
      (concat " [" 
              (propertize sqlite3-mode--current-table 
                          'face 'mode-line-emphasis)
              "]")))
    (:eval
     (let ((status (sqlite3-mode-stream-status)))
       (cond
        ((eq status 'prompt)
         (propertize " Run" 'face 'compilation-info))
        ((eq status 'transaction)
         (propertize " Transaction" 'face 'compilation-info))
        ((eq status 'querying)
         (propertize " Querying" 'face 'compilation-warning))
        (t
         (propertize " No Process" 'face 'shadow)))))))

(defun sqlite3-mode-setup-mode-line ()
  (setq mode-line-process sqlite3-mode-line-format))

(defconst sqlite3-mode-edit-buffer " *Sqlite3 Edit*")

(defun sqlite3-mode-open-edit-window (value)
  (let ((buf (get-buffer-create sqlite3-mode-edit-buffer))
        (config (current-window-configuration)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (insert value)
      (setq buffer-undo-list nil)
      (set-buffer-modified-p nil))
    (pop-to-buffer buf)
    (message "%s"
             (substitute-command-keys 
              "Type \\[exit-recursive-edit] to finish the edit."))
    ;+TODO check (recursion-depth)
    (recursive-edit)
    (let ((new-value (buffer-string)))
      (set-window-configuration config)
      new-value)))

(defun sqlite3-tooltip-show (text)
  ;; show tooltip at cursor point.
  ;; Unable calculate exactly absolute coord but almost case is ok.
  ;;TODO consider `x-max-tooltip-size'
  (let* ((xy (sqlite3-tooltip-absolute-coordinate (point)))
         (y
          (cond
           ((< (/ (ftruncate (cdr xy)) (x-display-pixel-height)) 0.2)
            (+ (cdr xy) (* (frame-char-height) 3)))
           (t
            (cdr xy)))))
    (x-show-tip 
     text nil 
     `((left . ,(car xy))
       (top . ,y))
     ;; huge timeout value
     100000)))

(defun sqlite3-tooltip-absolute-coordinate (point)
  (let* ((posn (posn-at-point point))
         (xy (posn-x-y posn))
         (x (+ (sqlite3-tooltip-frame-posn 'top) 
               (car xy)))
         (y (truncate
             (+ (sqlite3-tooltip-frame-posn 'left)
                (cdr xy)
                ;; FIXME calculate fringe of bar roughly..
                (* (or (and tool-bar-mode tool-bar-images-pixel-height) 0)
                   1.5)
                (* (or (and menu-bar-mode (frame-char-height)) 0)
                   1.5)))))
    (cons x y)))

(defun sqlite3-tooltip-frame-posn (prop)
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

(defun sqlite3-after-kill-buffer ()
  (when sqlite3-mode--stream
    (sqlite3-stream-close sqlite3-mode--stream))
  (sqlite3-mode--cleanup-timer))

(defvar sqlite3-mode--moved-column nil)
(defun sqlite3-mode--move-line (arg)
  (let ((col (if (memq last-command 
                       '(sqlite3-mode-previous-row sqlite3-mode-next-row))
                 sqlite3-mode--moved-column
               (current-column))))
    (setq sqlite3-mode--moved-column col)
    (forward-line arg)
    (move-to-column col)))

(defun sqlite3-mode--post-command ()
  (condition-case err
      (progn
        (sqlite3-mode--highlight-selected)
        (sqlite3-mode--delayed-draw-header))
    (error
     (message "%s" err))))

(defvar sqlite3-mode--highlight-overlay nil)
(make-variable-buffer-local 'sqlite3-mode--highlight-overlay)

(defun sqlite3-mode--highlight-selected ()
  (let ((ov (or sqlite3-mode--highlight-overlay
                (let ((tmp (make-overlay (point-max) (point-max))))
                  ;;TODo face
                  (overlay-put tmp 'face 'match)
                  tmp))))
    (cond
     ((memq ov (overlays-in (point) (1+ (point)))))
     ((get-text-property (point) 'sqlite3-source-value (current-buffer))
      (let* ((region (sqlite3-text-property-region (point) 'sqlite3-source-value))
             (start (car region))
             (end (cdr region)))
        (move-overlay ov start end)
        (setq sqlite3-mode--highlight-overlay ov)))
     (t
      (move-overlay ov (point-max) (point-max))))))

(defun sqlite3-text-property-region (point property)
  (let ((val (get-text-property point property)))
    (and val
         (let* ((start (previous-single-property-change 
                        point property
                        nil (line-beginning-position)))
                (end (next-single-property-change
                      point property 
                      nil (line-end-position))))
           (unless (eq (get-text-property start property) val)
             (setq start (point)))
           (unless (eq (get-text-property (1- end) property) val)
             (setq end (point)))
           (cons start end)))))

(defun sqlite3-mode--previous-cell (point &optional current-line)
  (let ((region (sqlite3-text-property-region point 'sqlite3-source-value)))
    (cond
     ((null region)
      (previous-single-property-change
       point 'sqlite3-source-value nil
       (if current-line (line-beginning-position) (point-min))))
     ((eq (car region) (point-min))
      (point-min))
     (t
      (sqlite3-mode--previous-cell (1- (car region)) current-line)))))

(defun sqlite3-mode--next-cell (point &optional current-line)
  (let ((region (sqlite3-text-property-region point 'sqlite3-source-value)))
    (next-single-property-change 
     (or (cdr region) point) 
     'sqlite3-source-value nil
     (and current-line (line-end-position)))))

(defun sqlite3-mode--data-to-text (datum)
  (let ((oneline (or
                  (and (string-match "^\\([^\n]+\\)" datum)
                       (match-string 1 datum))
                  datum)))
    (replace-regexp-in-string "\t" "\\\\t" oneline)))

(defun sqlite3-mode--format-text (width datum)
  (let* ((text (sqlite3-mode--data-to-text datum))
         (res (sqlite3-mode--truncate-text width text)))
    (cons (equal datum res) res)))

(defun sqlite3-mode--truncate-text (width text)
  (truncate-string-to-width text width nil ?\s t))

(defface sqlite3-header-background
  '((((type x w32 mac ns) (class color))
     :background "LightSteelBlue" :foreground "black")
    (((class color))
     (:background "white" :foreground "black")))
  "*Face to fontify background of header line."
  :group 'faces)

(defun sqlite3-header-background-propertize (string)
  (let ((end (length string)))
    (add-text-properties 0 end 
                         (list
                          'face (list 'sqlite3-header-background)
                          'tab-separator t)
                         string)
    string))

(defvar sqlite3-header-column-separator
  (let ((sep " "))
    (sqlite3-header-background-propertize sep)
    (propertize sep 'display 
                '(space :width 1)))
  "String used to separate tabs.")

(defun sqlite3-mode--set-header (width-def header)
  (setq sqlite3-mode--header
        (loop for w in (cdr width-def)
              for h in (cdr header)
              collect (list h w))))

(defvar sqlite3-mode--header nil)
(make-variable-buffer-local 'sqlite3-mode--header)

(defvar sqlite3-mode--previous-hscroll nil)
(make-variable-buffer-local 'sqlite3-mode--previous-hscroll)

(defun sqlite3-mode--delayed-draw-header (&optional force)
  (when force
    (setq sqlite3-mode--previous-hscroll nil))
  (cancel-function-timers 'sqlite3-mode--draw-header)
  ;; in the `post-command-hook', `window-hscroll' still return previous value.
  ;; although after calling `scroll-right' return correct value.
  (run-with-timer 0.1 nil 'sqlite3-mode--draw-header))

(defun sqlite3-mode--draw-header ()
  (unless (eq sqlite3-mode--previous-hscroll (window-hscroll))
    (setq sqlite3-mode--previous-hscroll (window-hscroll))
    (let* ((disp-headers 
            (loop with hscroll = (window-hscroll)
                  with flag
                  for (h w) in sqlite3-mode--header
                  sum (1+ w) into right
                  if (<= hscroll right)
                  collect (let ((wid w))
                            (unless flag
                              (setq wid (- right hscroll 1)))
                            (setq flag t)
                            (if (< wid 3)
                                (sqlite3-mode--truncate-text wid "")
                              (sqlite3-mode--truncate-text wid h)))))
           (filler (make-string (frame-width) ?\s))
           (tail (sqlite3-header-background-propertize filler)))
      (setq header-line-format
            (and disp-headers
                 (list 
                  sqlite3-header-column-separator
                  (mapconcat
                   'identity
                   disp-headers
                   sqlite3-header-column-separator)
                  tail))))))

(defun sqlite3-mode--insert-row (width-def row)
  ;; (car row) is ROWID
  (loop for v in (cdr row)
        for w in (cdr width-def)
        for i from 0
        do (progn
             (when (> i 0)
               (insert " "))
             (let ((region (sqlite3-mode--insert-cell v w)))
               (put-text-property (car region) (cdr region) 
                                  'sqlite3-source-value v))))
  (put-text-property
   (line-beginning-position) (line-end-position)
   'sqlite3-rowid (car row))
  (insert "\n"))

(defun sqlite3-mode--insert-cell (value width)
  (let ((start (point))
        (fmt (sqlite3-mode--format-text width value)))
    (insert (cdr fmt))
    (let ((end (point)))
      (put-text-property start end 'sqlite3-mode-cell-width width)
      (when (car fmt)
        (put-text-property start end 'sqlite3-mode-truncated t))
      (cons start end))))

(defun sqlite3-mode--replace-current-cell (value width)
  (let* ((pos (point))
         (region (sqlite3-text-property-region pos 'sqlite3-source-value)))
    (unless region
      (error "Not a cell"))
    (let ((source (get-text-property pos 'sqlite3-source-value)))
      (delete-region (car region) (cdr region))
      (let ((new (sqlite3-mode--insert-cell value width)))
        (put-text-property (car new) (cdr new) 'sqlite3-source-value source)))
    (goto-char pos)))

;;TODO hack function make obsolete later
(defun sqlite3-mode-open-table (table)
  (interactive 
   (let ((table (sqlite3-mode--read-table)))
     (list table)))
  (unless (sqlite3-mode-draw-page table 0)
    (error "No data")))

(defvar sqlite3-mode-read-table-history nil)
(defun sqlite3-mode--read-table ()
  (completing-read
   "Table: "
   (mapcar
    (lambda (x) (car x))
    (sqlite3-mode-read-data sqlite3-select-table-query t))
   nil t nil 'sqlite3-mode-read-table-history))

(defun sqlite3-mode--send-query (query)
  (sqlite3-mode--check-stream)
  (sqlite3-stream--send-query sqlite3-mode--stream query))

(defun sqlite3-mode--check-stream ()
  "Check current buffer's database file is opend by sqlite."
  (unless (and sqlite3-mode--stream
               (eq (process-status sqlite3-mode--stream) 'run))
    (setq sqlite3-mode--stream
          (sqlite3-stream-open buffer-file-name))))

(defun sqlite3-mode-read-data (query &optional ommit-header)
  (sqlite3-mode--check-stream)
  (let ((data (sqlite3-stream--read-result sqlite3-mode--stream query)))
    (if ommit-header
        (cdr data)
      data)))

(defun sqlite3-mode-redraw-page ()
  (when sqlite3-mode--current-table
    (let ((start (window-start))
          (pos (point)))
      (sqlite3-mode-draw-page 
       sqlite3-mode--current-table
       sqlite3-mode--current-page)
      (goto-char pos)
      (set-window-start (selected-window) start))))

(defun sqlite3-mode-draw-page (table page)
  (save-excursion
    (let* ((where (or sqlite3-mode--current-cond "1 = 1"))
           (order "") ;; TODO order by
           (order-by "")                ;TODO
           (query (format 
                   (concat "SELECT ROWID, *"
                           " FROM %s"
                           " WHERE %s"
                           " LIMIT %s OFFSET %s * %s"
                           "%s")
                   table where
                   sqlite3-mode--maximum
                   sqlite3-mode--maximum
                   page order-by))
           (data (sqlite3-mode-read-data query)))
      (cond
       ((or data
            (not (equal sqlite3-mode--current-table table)))
        (setq sqlite3-mode--current-table table)
        (let ((inhibit-read-only t)
              (width-def (sqlite3-mode--calculate-cell-width data)))
          (erase-buffer)
          (remove-overlays (point-min) (point-max))
          (sqlite3-mode--set-header width-def (car data))
          (mapc
           (lambda (row)
             (sqlite3-mode--insert-row width-def row))
           ;; ignore header row
           (cdr data))
          (set-buffer-modified-p nil))
        (setq buffer-read-only t)
        (setq sqlite3-mode--current-page page)
        ;; redraw header forcibly
        (sqlite3-mode--delayed-draw-header t))
       (t nil)))))

(defun sqlite3-mode--calculate-cell-width (data)
  ;; decide list length by header line
  (loop with all-width = (make-list (length (car data)) 3)
        for row in data
        do (loop for datum in row
                 for pair on all-width
                 for idx from 0
                 do (let* ((text (sqlite3-mode--data-to-text datum))
                           (wid (string-width text)))
                      (setcar pair (min (max (car pair) wid) 30))))
        finally return all-width))

(defun sqlite3-mode-stream-status ()
  (let ((stream sqlite3-mode--stream))
    (cond
     ((null stream) 'exit)
     ((not (eq (process-status stream) 'run)) 'exit)
     ((with-current-buffer (process-buffer stream)
        (sqlite3--prompt-waiting-p))
      (cond
       ((process-get stream 'sqlite3-mode-transaction)
        'transaction)
       (t
        'prompt)))
     (t 'querying))))

(defvar sqlite3-mode--context nil)
(make-variable-buffer-local 'sqlite3-mode--context)

;;TODO this local variable to one context variable?

(defvar sqlite3-mode--current-order nil)
(make-variable-buffer-local 'sqlite3-mode--current-order)

(defvar sqlite3-mode--current-cond nil)
(make-variable-buffer-local 'sqlite3-mode--current-cond)

(defvar sqlite3-mode--current-table nil)
(make-variable-buffer-local 'sqlite3-mode--current-table)

(defvar sqlite3-mode--current-page nil)
(make-variable-buffer-local 'sqlite3-mode--current-page)

(defvar sqlite3-mode--maximum 100)

(defun sqlite3-mode--transaction-begin ()
  (unless (verify-visited-file-modtime (current-buffer))
    (error "Database was modified after open"))
  (sqlite3-mode--send-query "BEGIN")
  (process-put sqlite3-mode--stream 'sqlite3-mode-transaction t))

(defun sqlite3-mode--transaction-rollback ()
  ;;TODO file modtime do-suru?
  (sqlite3-mode--send-query "ROLLBACK")
  (process-put sqlite3-mode--stream 'sqlite3-mode-transaction nil))

(defun sqlite3-mode--transaction-commit ()
  ;;TODO file modtime do-suru?
  (sqlite3-mode--send-query "COMMIT")
  (process-put sqlite3-mode--stream 'sqlite3-mode-transaction nil))

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
    (with-timeout (5 (kill-process proc))
      (process-send-string proc ".quit\n")
      (while (eq (process-status proc) 'run)
        (sit-for 0.1)))
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

QUERY is a sql statement that can have not statement end (`;').
 Do Not have multiple statements.

Examples:
Good: SELECT * FROM table1;
Good: SELECT * FROM table1
Good: SELECT * FROM table1\n
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
        (sqlite3-sleep 0.1))
      (when (stringp sqlite3-stream--error)
        (error "%s" sqlite3-stream--error))
      t)))

(defun sqlite3-stream--wait-until-prompt (proc)
  (let ((buf (process-buffer proc)))
    (with-current-buffer buf
      (while (and (eq (process-status proc) 'run)
                  (not (sqlite3--prompt-waiting-p)))
        (sqlite3-sleep 0.1)))))

(defun sqlite3-sleep (seconds)
  (redisplay)
  (sleep-for seconds))

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

;;;###autoload
(defun sqlite3-file-guessed-valid-p (file)
  (with-temp-buffer
    (insert-file-contents file nil 0 256)
    (looking-at sqlite3-file-header-regexp)))

;;;###autoload
(defun sqlite3-find-file (db-file)
  (interactive "FSqlite3 File: ")
  (unless (sqlite3-file-guessed-valid-p db-file)
    (error "Not a valid database file"))
  (let ((buf (get-file-buffer db-file)))
    (unless buf
      (setq buf (create-file-buffer (file-name-nondirectory db-file)))
      (with-current-buffer buf
        (set-visited-file-name db-file)
        (set-buffer-modified-p nil)
        (sqlite3-mode)))
    (switch-to-buffer buf)))

(defun sqlite3-mode-revert-buffer (&rest dummy)
  (sqlite3-mode-redraw-page))

(defun sqlite3--prompt-waiting-p ()
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    (looking-at "^sqlite> \\'")))

(defun sqlite3--read-csv-with-deletion ()
  "Read csv data from current point. Delete csv data if read was succeeded."
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
          (should (equal (sqlite3-db-table-info db "hoge") 
                         '((0 "id" "INTEGER" nil "" t) (1 "text" "TEXT" nil "" nil))))
          (should (sqlite3-stream--send-query stream "INSERT INTO hoge VALUES (1, 'a')"))
          (should (sqlite3-stream--send-query stream "INSERT INTO hoge VALUES (2, 'b')"))
          (should (equal (sqlite3-stream--read-result
                          stream "SELECT * FROM hoge ORDER BY id")
                         '(("id" "text") ("1" "a") ("2" "b"))))
          (should (sqlite3-stream--send-query stream "UPDATE hoge SET id = id + 10, text = text || 'z'"))
          (should (equal
                   (sqlite3-stream--read-result stream "SELECT * FROM hoge")
                   '(("id" "text") ("11" "az") ("12" "bz"))))
          (should (sqlite3-stream--send-query stream "DELETE FROM hoge WHERE id = 11"))
          (should (equal
                   (sqlite3-stream--read-result stream "SELECT * FROM hoge")
                   '(("id" "text") ("12" "bz"))))
          )
      (sqlite3-stream-close stream))))

(ert-deftest sqlite3-irregular-0001 ()
  :tags '(sqlite3)
  (let* ((db (make-temp-file "sqlite3-test-"))
         (stream (sqlite3-stream-open db)))
    (unwind-protect
        (progn
          (sqlite3-stream--send-query stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY)")
          (should-error (sqlite3-stream--send-query stream "CREATE TABLE1"))
          (should-error (sqlite3-stream--send-query stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY)"))
          (sqlite3-stream--send-query stream "INSERT INTO hoge VALUES (1)")
          (should-error (sqlite3-stream--send-query stream "INSERT INTO hoge VALUES (1)"))
          )
      (sqlite3-stream-close stream))))

;;; sqlite3.el ends here
