
(defconst sqlite3-dev-file (make-temp-name (expand-file-name "sqlite3-dev-" temporary-file-directory)))
(defconst sqlite3-dev-try-count 10)
(defconst sqlite3-dev-stream (sqlite3-stream-open sqlite3-dev-file))

(sqlite3-read sqlite3-dev-file "CREATE TABLE hoge (id);")
(dotimes (i 1000)
  (sqlite3-read sqlite3-dev-file (format "INSERT INTO hoge VALUES (%d);" i)))


(defmacro sqlite3-benchmark (&rest form)
  (declare (indent 0))
  `(let ((start (float-time)))
     (progn ,@form)
     (let ((end (float-time)))
       (- end start))))

(let ((overcount 0)
      (rates nil)
      (limit 1.5))
  (dotimes (i sqlite3-dev-try-count)
    (let* ((base-time (sqlite3-benchmark 
                        (sqlite3-read sqlite3-dev-file "SELECT * FROM hoge")))

           ;; stream api is not enough fast. because serialize cost too high.. 
           ;; aim at least 5 times cost above syncronous interface.
           ;; Currently 2013-04-23
           ;; CYGWIN_NT-6.1-WOW64: 3 times
           ;; Linux: 1.5 times

           (async-time (sqlite3-benchmark
                         (sqlite3-stream-read-query sqlite3-dev-stream "SELECT * FROM hoge")))
           (rate (/ async-time base-time)))
      (push rate rates)
      (when (> rate limit)
        (incf overcount))))
  (should (> (/ sqlite3-dev-try-count 2) overcount))
  (list overcount rates))


;; some of GUI tool query planner
  
(defun sqlite3-planner-mode ()
  )

(defun sqlite3-planner (file sql)
  (let ((explain (sqlite3-read
                  file (format "EXPLAIN %s" sql)
                  "-header"))
        (plan (sqlite3-read
               file (format "EXPLAIN QUERY PLAN %s" sql)
               "-header"))
        ;;TODO
        (stats (sqlite3-read
                file sql
                "-stats")))
    (list explain plan stats)))


;;;
;;; signal support
;;;

;;TODO remove until future
(defvar sqlite3--signal-support 'unknown)

(when (eq sqlite3--signal-support 'unknown)
  (ignore-errors
    (let* ((file (make-temp-file "emacs-sqlite3-"))
           (process (sqlite3-start-csv-process file "SELECT 1")))
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
             (setq sqlite3--signal-support 'support)))
        (delete-process process)
        (kill-buffer (process-buffer process))
        (delete-file file)))))

(defun sqlite3-stream-suspend (stream)
  ;; TODO  SIGSTOP
  (signal-process stream 19))

(defun sqlite3-stream-continue (stream)
  ;; TODO  SIGCONT
  (signal-process stream 18))

;; TODO I can't get clue.
;;    esh-proc.el: eshell-continue-process, eshell-stop-process says stop status is not yet supported.

