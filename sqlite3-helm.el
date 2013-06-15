;;; sqlite3-helm.el --- Define helm source for sqlite3 database.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: data
;; URL: http://github.com/mhayashi1120/sqlite3.el/raw/master/sqlite3-helm.el
;; Emacs: GNU Emacs 24 or later
;; Version: 0.0.0
;; Package-Requires: ((sqlite3 "0.0.0"))

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

(require 'sqlite3)
;;TODO need?
;; (require 'helm)

(declare-function 'helm-log "helm")
(declare-function 'helm-get-current-source "helm")

;;;
;;; Sqlite3 for helm (testing)
;;;

(defvar sqlite3-helm-history nil)

(defface sqlite3-helm-finish
    '((t (:foreground "Green")))
  "Face used in mode line when sqlite is finish."
  :group 'helm-grep)

;;TODO coding-system on windows.

;;;###autoload
(defun sqlite3-helm-define (source)
  "This function provides extension while `helm' source composing.
Normally, should not set `candidates-process' directive.
Following sqlite3 specific directive:
`sqlite3-db' File name of sqlite3 database.
`sqlite3-composer' Function which accept one argument `helm-pattern' and return
    a sql query string."
  (let ((file (assoc-default 'sqlite3-db source))
        (composer (assoc-default 'sqlite3-composer source)))
    (unless (stringp file)
      (error "sqlite3-db: Not a valid filename %s" file))
    (unless (functionp composer)
      (error "sqlite3-composer: Not a valid function"))
    (let ((result
           ;;TODO consider `match' directive
           `((name . "sqlite3")
             (candidates-process
              .
              (lambda ()
                (sqlite3-helm-invoke-command
                 ,file
                 (funcall ,composer helm-pattern))))
             (history . sqlite3-helm-history)
             (delayed)
             (candidate-transformer . sqlite3-helm-hack-for-multiline))))
      (dolist (s source)
        (let ((cell (assq (car-safe s) result)))
          (cond
           (cell
            (setcdr cell (cdr s)))
           (t
            (setq result (cons s result))))))
      result)))

(defun sqlite3-helm-invoke-command (file query)
  "Initialize async locate process for `helm-source-locate'."
  ;; sqlite3-helm implementation ignore NULL. (NULL is same as a empty string)
  (let ((proc (sqlite3-start-csv-process file query "")))
    (set-process-sentinel
     proc
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (with-helm-window
           (setq mode-line-format
                 '(" " mode-line-buffer-identification " "
                   (line-number-mode "%l") " "
                   (:eval (propertize
                           (format "[Sqlite3 Process Finish- (%s results)]"
                                   ;;TODO count-line count all of sources.
                                   (max (1- (count-lines
                                             (point-min) (point-max))) 0))
                           'face 'sqlite3-helm-finish))))
           (force-mode-line-update))
         (unless (zerop (process-exit-status proc))
           (helm-log "Error: sqlite3 %s"
                     (replace-regexp-in-string "\n" "" event))))))
    proc))

(defun sqlite3-helm-hack-for-multiline (candidates)
  ;; helm split csv stream by newline. restore the csv and try
  ;; to parse
  ;; OUTPUT-STRING: a,b\nc,d\ne CANDIDATES: ("a,b" "c,d") INCOMPLETE-LINE: "e"
  ;; OUTPUT-STRING: a,b\nc,d\n  CANDIDATES: ("a,b" "c,d") INCOMPLETE-LINE: ""
  ;; OUTPUT-STRING: a,b\nc,d    CANDIDATES: ("a,b")       INCOMPLETE-LINE: "c,d"

  ;; OUTPUT-STRING: a,b\nc,"d\nD"\ne CANDIDATES: ("a,b" "c,\"d\nD\"") INCOMPLETE-LINE: "e"
  ;; OUTPUT-STRING: a,b\nc,"d\nD"\n  CANDIDATES: ("a,b" "c,\"d\nD\"") INCOMPLETE-LINE: ""
  ;; OUTPUT-STRING: a,b\nc,"d\nD"    CANDIDATES: ("a,b" "c,\"d")      INCOMPLETE-LINE: "D\""
  ;; OUTPUT-STRING: a,b\nc,"d\nD     CANDIDATES: ("a,b" "c,\"d")      INCOMPLETE-LINE: "D"
  ;; OUTPUT-STRING: a,b\nc,"d\n      CANDIDATES: ("a,b" "c,\"d")      INCOMPLETE-LINE: ""
  ;; OUTPUT-STRING: a,b\nc,"d        CANDIDATES: ("a,b")              INCOMPLETE-LINE: "c,\"d"
  (let* ((rawtext (concat (mapconcat 'identity candidates "\n") "\n"))
         (source (helm-get-current-source))
         (incomplete-info (assq 'incomplete-line source)))
    (destructuring-bind (data rest) (sqlite3-helm--read-csv rawtext)
      (setcdr incomplete-info (concat rest (cdr incomplete-info)))
      data)))

;; try to read STRING until end.
;; csv line may not terminated and may contain newline.
(defun sqlite3-helm--read-csv (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((res '())
          (start (point)))
      (condition-case nil
          (while (not (eobp))
            (let ((ln (sqlite3--read-csv-line)))
              (setq start (point))
              (setq res (cons ln res))))
        (invalid-read-syntax))
      (list (nreverse res)
            (buffer-substring-no-properties start (point-max))))))


;;;
;;; Any utilities
;;;

;;TODO consider fuzzy search
;;    ^aa* ->  aa%, *bb$ -> %bb
;;    fuzzy aa -> %aa%
;;;###autoload
(defun sqlite3-helm-glob-to-like (glob &optional escape-char)
  "Convenient function to provide unix like GLOB to sql LIKE pattern

See related information in `sqlite3-escape-like'

e.g. hoge*foo -> hoge%foo
     hoge?foo -> hoge_foo"
  (sqlite3--replace
   glob
   (sqlite3-escape--like-table
    escape-char
    '((?* . "%")
      (?\? . "_")
      (?\\ (?\* . "*")
           (?\? . "?")
           (?\\ . "\\\\"))))))

;;;###autoload
(defun sqlite3-helm-fuzzy-glob-to-like (glob &optional escape-char)
  "TODO Convert GLOB to like syntax.
`^' Like regexp, match to start of text.
`$' Like regexp, match to end of text.
`*' Like glob, match to text more than 0.
`?' Like glob, match to a char in text.

If no `^' and `$' are presant, fuzzy match to text.

TODO ESCAPE-CHAR

"
  (let (prefix suffix pattern)
    (cond
     ((string-match "\\`\\^" glob)
      (setq glob (substring glob 1)))
     ((string-match "\\`\\\\\\^" glob)
      ;; escaped ^.
      (setq glob (substring glob 1)
            prefix "%"))
     (t
      (setq prefix "%")))
    (cond
     ((string-match "\\`\\(\\\\.\\|[^\\\\]\\)*\\\\$\\'" glob)
      ;; Match to end of escaped end of `$'
      (setq glob (concat (substring glob 0 -2) "$"))
      (setq suffix "%"))
     ((string-match "\\$\\'" glob)
      ;; end of non-escaped `$'
      (setq glob (substring glob 0 -1)))
     (t
      (setq suffix "%")))
    (setq pattern (sqlite3-helm-glob-to-like glob escape-char))
    (concat prefix pattern suffix)))


(provide 'sqlite3-helm)

;;; sqlite3-helm.el ends here
