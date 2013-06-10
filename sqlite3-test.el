;;TODO switch sqlite3 command version.

(require 'ert)

(defun sqlite3-test-wait-exit (process)
  (while (eq (process-status process) 'run) (sleep-for 0.01)))

(ert-deftest sqlite3-normal-0001 ()
  :tags '(sqlite3)
  (let* ((db (make-temp-file "sqlite3-test-"))
         (stream (sqlite3-stream-open db)))
    (unwind-protect
        (progn
          (should (sqlite3-stream-execute-sql stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY, text TEXT)"))
          (should (equal (sqlite3-table-schema stream "hoge")
                         '((0 "id" "INTEGER" nil :null t) (1 "text" "TEXT" nil :null nil))))
          (should (sqlite3-stream-execute-sql stream "INSERT INTO hoge VALUES (1, 'a')"))
          (should (sqlite3-stream-execute-sql stream "INSERT INTO hoge VALUES (2, 'b')"))
          (should (equal (sqlite3-stream-execute-query
                          stream "SELECT * FROM hoge ORDER BY id")
                         '(("1" "a") ("2" "b"))))
          (should (sqlite3-stream-execute-sql stream "UPDATE hoge SET id = id + 10, text = text || 'z'"))
          (should (equal
                   (sqlite3-stream-execute-query stream "SELECT * FROM hoge")
                   '(("11" "az") ("12" "bz"))))
          (should (sqlite3-stream-execute-sql stream "DELETE FROM hoge WHERE id = 11"))
          (should (equal
                   (sqlite3-stream-execute-query stream "SELECT * FROM hoge")
                   '(("12" "bz"))))
          )
      (sqlite3-stream-close stream))))

(ert-deftest sqlite3-normal-onetime-stream-0001 ()
  :tags '(sqlite3)
  (let* ((db (make-temp-file "sqlite3-test-"))
         (p1 (sqlite3-onetime-stream db "CREATE TABLE hoge (id);" (lambda (x)))))
    (sqlite3-test-wait-exit p1)
    (let ((query (mapconcat
                  'identity
                  (mapcar
                   (lambda (n)
                     (format "INSERT INTO hoge VALUES(%d);" n))
                   '(1 2 3 4 5)) "")))
      (let ((p2 (sqlite3-onetime-stream db query (lambda (x)))))
        (sqlite3-test-wait-exit p2))
      (let* ((result '())
             (p (sqlite3-onetime-stream
                 db "SELECT id FROM hoge;"
                 (lambda (x)
                   (unless (eq x :EOF)
                     (setq result (cons (string-to-number (nth 0 x)) result)))))))
        (sqlite3-test-wait-exit p)
        (should (equal '(5 4 3 2 1) result))))))

(ert-deftest sqlite3-irregular-0001 ()
  :tags '(sqlite3)
  (let* ((db (make-temp-file "sqlite3-test-"))
         (stream (sqlite3-stream-open db)))
    (unwind-protect
        (progn
          (sqlite3-stream-execute-sql stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY)")
          (should-error (sqlite3-stream-execute-sql stream "CREATE TABLE1"))
          (should-error (sqlite3-stream-execute-sql stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY)"))
          (sqlite3-stream-execute-sql stream "INSERT INTO hoge VALUES (1)")
          (should-error (sqlite3-stream-execute-sql stream "INSERT INTO hoge VALUES (1)"))
          )
      (sqlite3-stream-close stream))))


(ert-deftest sqlite3-escape ()
  :tags '(sqlite3)
  (should (equal "A" (sqlite3-escape-string "A") ))
  (should (equal "A''''" (sqlite3-escape-string "A''")))
  (should (equal "A''\"" (sqlite3-escape-string "A'\"")))
  (should (equal "A'\"\"" (sqlite3-escape-string "A'\"" ?\")))
  (should (equal "A" (sqlite3-escape-like "A" ?\\)))
  (should (equal "A\\%\\_" (sqlite3-escape-like "A%_" ?\\))))

(ert-deftest sqlite3-glob-to-like ()
  :tags '(sqlite3)
  (should (equal "a" (sqlite3-glob-to-like "a")))
  (should (equal "%ab_" (sqlite3-glob-to-like "*ab?")))
  (should (equal "\\_a" (sqlite3-glob-to-like "_a" ?\\))))
