
(defconst esqlite-dev-file (make-temp-name (expand-file-name "esqlite-dev-" temporary-file-directory)))
(defconst esqlite-dev-try-count 10)
(defconst esqlite-dev-stream (esqlite-stream-open esqlite-dev-file))

(esqlite-stream-execute esqlite-dev-stream "CREATE TABLE hoge (id);")
(loop for start from 0 to 1000 by 100
      do (esqlite-stream-execute
          esqlite-dev-stream
          (format "INSERT INTO hoge VALUES %s;"
                  (mapconcat (lambda (x) (format "(%d)" x)) (number-sequence start (+ start 100)) ", " ))))


(defmacro esqlite-benchmark (&rest form)
  (declare (indent 0))
  `(let ((start (float-time)))
     (progn ,@form)
     (let ((end (float-time)))
       (- end start))))

(let ((overcount 0)
      (rates nil)
      (limit 1.5))
  (dotimes (i esqlite-dev-try-count)
    (let* ((base-time (esqlite-benchmark 
                        (esqlite-read esqlite-dev-file "SELECT * FROM hoge")))

           ;; stream api is not enough fast. because serialize cost too high.. 
           ;; aim at least 5 times cost above syncronous interface.
           ;; Currently 2013-04-23
           ;; CYGWIN_NT-6.1-WOW64: 3 times
           ;; Linux: 1.5 times

           (async-time (esqlite-benchmark
                         (esqlite-stream-read esqlite-dev-stream "SELECT * FROM hoge")))
           (rate (/ async-time base-time)))
      (push rate rates)
      (when (> rate limit)
        (incf overcount))))
  (should (> (/ esqlite-dev-try-count 2) overcount))
  (list overcount rates))


;; some of GUI tool query planner
  
(defun esqlite-planner-mode ()
  )

(defun esqlite-planner (file sql)
  (let ((explain (esqlite-read
                  file (format "EXPLAIN %s" sql)
                  "-header"))
        (plan (esqlite-read
               file (format "EXPLAIN QUERY PLAN %s" sql)
               "-header"))
        ;;TODO
        (stats (esqlite-read
                file sql
                "-stats")))
    (list explain plan stats)))


;;;
;;; signal support
;;;

;;TODO remove until future
(defvar esqlite--signal-support 'unknown)

(when (eq esqlite--signal-support 'unknown)
  (ignore-errors
    (let* ((file (make-temp-file "emacs-esqlite-"))
           (process (esqlite-start-csv-process file "SELECT 1")))
      (unwind-protect
          (progn
            (and
             ;; suspend
             (prog1 (eq (signal-process process 'stop) 0)
               (sleep-for 0.1))
             ;; check status
             (eq (process-status process) 'stop)
             ;; continue
             (prog1 (eq (signal-process process 'cont) 0)
               (sleep-for 0.1))
             ;; re-check status
             (eq (process-status process) 'run)
             (setq esqlite--signal-support 'support)))
        (delete-process process)
        (kill-buffer (process-buffer process))
        (delete-file file)))))

(defun esqlite-stream-suspend (stream)
  ;; TODO  SIGSTOP
  (signal-process stream 19))

(defun esqlite-stream-continue (stream)
  ;; TODO  SIGCONT
  (signal-process stream 18))

;; TODO I can't get clue.
;;    esh-proc.el: eshell-continue-process, eshell-stop-process says stop status is not yet supported.



(defun esqlite-create-alternate-table (stream create-sql)
  "Execute CREATE-SQL in STREAM. This function not begin transaction.
If you need transaction, begin transaction by your own before calling this function."
  (unless (let ((case-fold-search t))
            (string-match "^[ \t\n]*create[ \t\n]+table[ \t\n]+\\([^ \t\n]+\\)" create-sql))
    (user-error "esqlite: Invalid create sql `%s'" create-sql))
  (let* ((table (match-string 1 create-sql))
         (temp-table (esqlite--unique-name stream table))
         (src-columns (mapcar
                       (lambda (x) (nth 1 x))
                       (esqlite-read-table-schema stream table))))
    (unless src-columns
      (error "esqlite: Unable to get `%s' table columns" table))
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

