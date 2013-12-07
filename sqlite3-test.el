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
          (should (sqlite3-stream-execute stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY, text TEXT)"))
          (should (equal '((0 "id" "INTEGER" nil :null t) (1 "text" "TEXT" nil :null nil))
                         (sqlite3-read-table-schema stream "hoge")))
          (should (sqlite3-stream-execute stream "INSERT INTO hoge VALUES (1, 'a')"))
          (should (sqlite3-stream-execute stream "INSERT INTO hoge VALUES (2, 'b')"))
          (should (equal '(("1" "a") ("2" "b"))
                         (sqlite3-stream-read
                          stream "SELECT * FROM hoge ORDER BY id")))
          (should (sqlite3-stream-execute stream "UPDATE hoge SET id = id + 10, text = text || 'z'"))
          (should (equal
                   '(("11" "az") ("12" "bz"))
                   (sqlite3-stream-read stream "SELECT * FROM hoge")))
          (should (sqlite3-stream-execute stream "DELETE FROM hoge WHERE id = 11"))
          (should (equal
                   '(("12" "bz"))
                   (sqlite3-stream-read stream "SELECT * FROM hoge")))
          (should (sqlite3-stream-execute stream "INSERT INTO hoge VALUES(3, 'あイｳ')"))
          (should (equal
                   '(("あイｳ"))
                   (sqlite3-stream-read stream "SELECT text FROM hoge WHERE id = 3")))
          (should (equal
                   '("あイｳ")
                   (sqlite3-stream-read-top stream "SELECT text FROM hoge WHERE id = 3")))
          (should (equal
                   "あイｳ"
                   (sqlite3-stream-read-atom stream "SELECT text FROM hoge WHERE id = 3"))))
      (sqlite3-stream-close stream)
      (delete-file db))))

(ert-deftest sqlite3-irregular-0001 ()
  :tags '(sqlite3)
  (let* ((db (make-temp-file "sqlite3-test-"))
         (stream (sqlite3-stream-open db)))
    (unwind-protect
        (progn
          (sqlite3-stream-execute stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY)")
          (should-error (sqlite3-stream-execute stream "CREATE TABLE1"))
          (should-error (sqlite3-stream-execute stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY)"))
          (sqlite3-stream-execute stream "INSERT INTO hoge VALUES (1)")
          (should-error (sqlite3-stream-execute stream "INSERT INTO hoge VALUES (1)"))
          (should (equal '(("1")) (sqlite3-stream-read stream "SELECT * FROM hoge")))
          (should-error (sqlite3-stream-read stream "SELECT"))
          ;; works fine after syntax error
          (should (equal '(("1")) (sqlite3-stream-read stream "SELECT * FROM hoge")))
          (should (sqlite3-file-guessed-database-p db)))
      (sqlite3-stream-close stream))))

;;TODO not yet working..
;; (ert-deftest sqlite3-async-read ()
;;   :tags '(sqlite3)
;;   (let ((db (make-temp-file "sqlite3-test-")))
;;     (unwind-protect
;;         (progn
;;           (sqlite3-async-execute db "CREATE TABLE hoge (id);" (lambda (x)))
;;           (let ((query (mapconcat
;;                         'identity
;;                         (mapcar
;;                          (lambda (n)
;;                            (format "INSERT INTO hoge VALUES(%d);" n))
;;                          '(1 2 3 4 5)) "")))
;;             (sqlite3-async-read db query (lambda (x)))
;;             (let ((result '()))
;;               (sqlite3-async-read
;;                db "SELECT id FROM hoge;"
;;                (lambda (x)
;;                  (unless (eq x :EOF)
;;                    (setq result (cons (string-to-number (nth 0 x)) result)))))
;;               (should (equal '(5 4 3 2 1) result)))
;;             (should-error (sqlite3-async-read db "SELECT" (lambda (x))))))
;;       (delete-file db)))
;;   )

(ert-deftest sqlite3-read ()
  :tags '(sqlite3)
  (let ((db (make-temp-file "sqlite3-test-")))
    (unwind-protect
        (progn
          (sqlite3-read db "CREATE TABLE hoge (id, text);")
          (sqlite3-read db "INSERT INTO hoge VALUES (1, 'あイｳ');")
          (should (equal '(("あイｳ")) (sqlite3-read db "SELECT text FROM hoge WHERE id = 1")))
          (should-error (sqlite3-read db "SELECT"))
          (should (equal '("あイｳ") (sqlite3-read-top db "SELECT text FROM hoge WHERE id = 1")))
          (should (equal "あイｳ" (sqlite3-read-atom db "SELECT text FROM hoge WHERE id = 1"))))
      (delete-file db))))

(ert-deftest sqlite3-reader-0001 ()
  :tags '(sqlite3)
  (let* ((db (make-temp-file "sqlite3-test-")))
    (unwind-protect
        (progn
          (sqlite3-execute db "CREATE TABLE table1(a,b,c)")
          (sqlite3-execute db "INSERT INTO table1 VALUES (1,'A', NULL)")
          (sqlite3-execute db "INSERT INTO table1 VALUES (2,'B', NULL)")
          (let ((reader (sqlite3-reader-open db "SELECT * FROM table1 ORDER BY a")))
            (unwind-protect
                (progn
                  (should (sqlite3-reader-open-p reader))
                  (should (equal '("1" "A" :null) (sqlite3-reader-read reader)))
                  ;;TODO
                  (should (equal '("2" "B" :null) (sqlite3-reader-peek reader)))
                  (should (equal '("2" "B" :null) (sqlite3-reader-read reader)))
                  (should (equal :EOF (sqlite3-reader-read reader)))
                  (should-not (sqlite3-reader-open-p reader)))
              (sqlite3-reader-close reader))))
      (delete-file db))))

(ert-deftest sqlite3-reader-0002 ()
  :tags '(sqlite3)
  (let* ((db (make-temp-file "sqlite3-test-")))
    (unwind-protect
        (progn
          (sqlite3-execute db "CREATE TABLE table1(a,b,c)")
          (sqlite3-execute db "INSERT INTO table1 VALUES (1,'A', NULL)")
          (let ((reader (sqlite3-reader-open db "SELECT * FROM table1")))
            (unwind-protect
                (progn
                  (should (sqlite3-reader-open-p reader))
                  (sqlite3-reader-close reader)
                  (should-not (sqlite3-reader-open-p reader)))
              (sqlite3-reader-close reader)))))))

;;TODO reader test
;;TODO sqlite3-call/stream transaction

(ert-deftest sqlite3-escape ()
  :tags '(sqlite3)
  (should (equal "A" (sqlite3-escape-string "A") ))
  (should (equal "A''''" (sqlite3-escape-string "A''")))
  (should (equal "A''\"" (sqlite3-escape-string "A'\"")))
  (should (equal "A'\"\"" (sqlite3-escape-string "A'\"" ?\")))
  (should (equal "A" (sqlite3-escape-like "A" ?\\)))
  (should (equal "A\\%\\_" (sqlite3-escape-like "A%_" ?\\)))
  (should (equal "\\\\\\%\\\\\\_" (sqlite3-escape-like "\\%\\_" ?\\))))

(ert-deftest sqlite3-glob-to-like ()
  :tags '(sqlite3)
  (should (equal "a" (sqlite3-helm-glob-to-like "a")))
  (should (equal "%ab_" (sqlite3-helm-glob-to-like "*ab?")))
  (should (equal "\\_a" (sqlite3-helm-glob-to-like "_a" ?\\)))
  (should (equal "*?%_\\%\\_" (sqlite3-helm-glob-to-like "\\*\\?*?\\%\\_" ?\\)))
  (should (equal "*0\\%" (sqlite3-helm-glob-to-like "\\*0%" ?\\)))
  (should (equal "\\0|%||" (sqlite3-helm-glob-to-like "\\\\0%|" ?\|)))
  (should (equal "\\\\0\\%|" (sqlite3-helm-glob-to-like "\\\\0%|" ?\\))))

(ert-deftest sqlite3-glob-to-fuzzy-like ()
  :tags '(sqlite3)
  (should (equal "a%" (sqlite3-helm-glob-to-fuzzy-like "^a")))
  (should (equal "%a%" (sqlite3-helm-glob-to-fuzzy-like "a")))
  (should (equal "%a" (sqlite3-helm-glob-to-fuzzy-like "a$")))
  (should (equal "%^a%" (sqlite3-helm-glob-to-fuzzy-like "\\^a")))
  (should (equal "%a$%" (sqlite3-helm-glob-to-fuzzy-like "a\\$")))
  (should (equal "%a\\\\" (sqlite3-helm-glob-to-fuzzy-like "a\\\\$")))
  (should (equal "%\\$%" (sqlite3-helm-glob-to-fuzzy-like "\\\\$" ?a)))
  )

(ert-deftest sqlite3-glob-to-fuzzy-like ()
  :tags '(sqlite3)
  (should (equal "a*" (sqlite3-helm-glob-to-fuzzy-glob "^a")))
  (should (equal "*a*" (sqlite3-helm-glob-to-fuzzy-glob "a")))
  (should (equal "*a" (sqlite3-helm-glob-to-fuzzy-glob "a$"))))

(ert-deftest sqlite3-format ()
  :tags '(sqlite3)
  ;;TODO error test

  (should (equal
           (concat
            "SELECT "
            "\"a\", \"b\",\"c\",'''text','something'"
            " FROM \"table\""
            " WHERE"
            " \"d\" LIKE 'hoge' ESCAPE '\\' "
            " AND col2 IN ('foo', 1)")
           (let ((search-text "hoge"))
             (sqlite3-format
              "SELECT %O,%o,%T,%V FROM %o WHERE %o LIKE %L{search-text} AND col2 IN (%V)"
              '("a" "b")
              "c" "'text"
              "something"
              "table"
              "d" '("foo" 1)))))
  (should (equal
           (concat
            "INSERT INTO (\"a\", \"b\")\n"
            " VALUES ('1', 2) ")
           (sqlite3-format
            '(
              "INSERT INTO (%O)"
              " VALUES (%V) ")
            '("a" "b") '("1" 2)))))

(ert-deftest sqlite3-number ()
  :tags '(sqlite3)
  (should (sqlite3-numeric-text-p "1"))
  (should (sqlite3-numeric-text-p "+2"))
  (should (sqlite3-numeric-text-p "-3"))
  (should (sqlite3-numeric-text-p "+4.0"))
  (should (sqlite3-numeric-text-p "+4.0E50")))
