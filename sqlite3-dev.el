
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

  
