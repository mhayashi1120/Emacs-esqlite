;;TODO switch esqlite command version.

(require 'ert)

(defun esqlite-test-wait-exit (process)
  (while (eq (process-status process) 'run) (sleep-for 0.01)))

(ert-deftest esqlite-normal-0001 ()
  :tags '(esqlite)
  (let* ((db (make-temp-file "esqlite-test-"))
         (stream (esqlite-stream-open db)))
    (unwind-protect
        (progn
          (should (esqlite-stream-execute stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY, text TEXT)"))
          (should (equal '((0 "id" "INTEGER" nil :null t) (1 "text" "TEXT" nil :null nil))
                         (esqlite-read-table-schema stream "hoge")))
          (should (esqlite-stream-execute stream "INSERT INTO hoge VALUES (1, 'a')"))
          (should (esqlite-stream-execute stream "INSERT INTO hoge VALUES (2, 'b')"))
          (should (equal '(("1" "a") ("2" "b"))
                         (esqlite-stream-read
                          stream "SELECT * FROM hoge ORDER BY id")))
          (should (esqlite-stream-execute stream "UPDATE hoge SET id = id + 10, text = text || 'z'"))
          (should (equal
                   '(("11" "az") ("12" "bz"))
                   (esqlite-stream-read stream "SELECT * FROM hoge")))
          (should (esqlite-stream-execute stream "DELETE FROM hoge WHERE id = 11"))
          (should (equal
                   '(("12" "bz"))
                   (esqlite-stream-read stream "SELECT * FROM hoge")))
          (should (esqlite-stream-execute stream "INSERT INTO hoge VALUES(3, 'あイｳ')"))
          (should (equal
                   '(("あイｳ"))
                   (esqlite-stream-read stream "SELECT text FROM hoge WHERE id = 3")))
          (should (equal
                   '("あイｳ")
                   (esqlite-stream-read-top stream "SELECT text FROM hoge WHERE id = 3")))
          (should (equal
                   "あイｳ"
                   (esqlite-stream-read-atom stream "SELECT text FROM hoge WHERE id = 3"))))
      (esqlite-stream-close stream)
      (delete-file db))))

(ert-deftest esqlite-irregular-0001 ()
  :tags '(esqlite)
  (let* ((db (make-temp-file "esqlite-test-"))
         (stream (esqlite-stream-open db)))
    (unwind-protect
        (progn
          (esqlite-stream-execute stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY)")
          (should-error (esqlite-stream-execute stream "CREATE TABLE1"))
          (should-error (esqlite-stream-execute stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY)"))
          (esqlite-stream-execute stream "INSERT INTO hoge VALUES (1)")
          (should-error (esqlite-stream-execute stream "INSERT INTO hoge VALUES (1)"))
          (should (equal '(("1")) (esqlite-stream-read stream "SELECT * FROM hoge")))
          (should-error (esqlite-stream-read stream "SELECT"))
          ;; works fine after syntax error
          (should (equal '(("1")) (esqlite-stream-read stream "SELECT * FROM hoge")))
          (should (esqlite-file-guessed-database-p db)))
      (esqlite-stream-close stream))))

;;TODO not yet working..
(ert-deftest esqlite-async-read ()
  :tags '(esqlite)
  (let ((db (make-temp-file "esqlite-test-")))
    (unwind-protect
        (progn
          (esqlite-async-execute db "CREATE TABLE hoge (id);" (lambda ()))
          (let ((query (mapconcat
                        'identity
                        (mapcar
                         (lambda (n)
                           (format "INSERT INTO hoge VALUES(%d);" n))
                         (number-sequence 1 5)) "")))
            (esqlite-async-read db query (lambda (x)))
            (lexical-let ((result '()))
              (esqlite-async-read
               db "SELECT id FROM hoge;"
               (lambda (x)
                 (if (eq x :EOF)
                     (should (equal '(5 4 3 2 1) result))
                   (setq result (cons (string-to-number (nth 0 x)) result))))))
            (should-error (esqlite-async-read db "SELECT" (lambda (x))))))
      (delete-file db))))

(ert-deftest esqlite-read ()
  :tags '(esqlite)
  (let ((db (make-temp-file "esqlite-test-")))
    (unwind-protect
        (progn
          (esqlite-read db "CREATE TABLE hoge (id, text);")
          (esqlite-read db "INSERT INTO hoge VALUES (1, 'あイｳ');")
          (should (equal '(("あイｳ")) (esqlite-read db "SELECT text FROM hoge WHERE id = 1")))
          (should-error (esqlite-read db "SELECT"))
          (should (equal '("あイｳ") (esqlite-read-top db "SELECT text FROM hoge WHERE id = 1")))
          (should (equal "あイｳ" (esqlite-read-atom db "SELECT text FROM hoge WHERE id = 1"))))
      (delete-file db))))

;;TODO esqlite-call/stream transaction

(ert-deftest esqlite-escape ()
  :tags '(esqlite)
  (should (equal "A" (esqlite-escape-string "A") ))
  (should (equal "A''''" (esqlite-escape-string "A''")))
  (should (equal "A''\"" (esqlite-escape-string "A'\"")))
  (should (equal "A'\"\"" (esqlite-escape-string "A'\"" ?\")))
  (should (equal "A" (esqlite-escape-like "A" ?\\)))
  (should (equal "A\\%\\_" (esqlite-escape-like "A%_" ?\\)))
  (should (equal "\\\\\\%\\\\\\_" (esqlite-escape-like "\\%\\_" ?\\))))

(ert-deftest esqlite-glob-to-like ()
  :tags '(esqlite)
  (should (equal "a" (esqlite-helm-glob-to-like "a")))
  (should (equal "%ab_" (esqlite-helm-glob-to-like "*ab?")))
  (should (equal "\\_a" (esqlite-helm-glob-to-like "_a" ?\\)))
  (should (equal "*?%_\\%\\_" (esqlite-helm-glob-to-like "\\*\\?*?\\%\\_" ?\\)))
  (should (equal "*0\\%" (esqlite-helm-glob-to-like "\\*0%" ?\\)))
  (should (equal "\\0|%||" (esqlite-helm-glob-to-like "\\\\0%|" ?\|)))
  (should (equal "\\\\0\\%|" (esqlite-helm-glob-to-like "\\\\0%|" ?\\))))

(ert-deftest esqlite-glob-to-fuzzy-like ()
  :tags '(esqlite)
  (should (equal "a%" (esqlite-helm-glob-to-fuzzy-like "^a")))
  (should (equal "%a%" (esqlite-helm-glob-to-fuzzy-like "a")))
  (should (equal "%a" (esqlite-helm-glob-to-fuzzy-like "a$")))
  (should (equal "%^a%" (esqlite-helm-glob-to-fuzzy-like "\\^a")))
  (should (equal "%a$%" (esqlite-helm-glob-to-fuzzy-like "a\\$")))
  (should (equal "%a\\\\" (esqlite-helm-glob-to-fuzzy-like "a\\\\$")))
  (should (equal "%\\$%" (esqlite-helm-glob-to-fuzzy-like "\\\\$" ?a)))
  )

(ert-deftest esqlite-glob-to-fuzzy-like ()
  :tags '(esqlite)
  (should (equal "a*" (esqlite-helm-glob-to-fuzzy-glob "^a")))
  (should (equal "*a*" (esqlite-helm-glob-to-fuzzy-glob "a")))
  (should (equal "*a" (esqlite-helm-glob-to-fuzzy-glob "a$"))))

(ert-deftest esqlite-format ()
  :tags '(esqlite)
  ;;TODO error test

  (should (equal
           (concat
            "SELECT \n"
            "\"a\", \"b\",\"c\",'''text','something'\n"
            " FROM \"table\"\n"
            " WHERE\n"
            " \"d\" LIKE 'hoge' ESCAPE '\\' \n"
            " AND col2 IN ('foo', 1)")
           (let ((search-text "hoge"))
             (esqlite-format
              '("SELECT "
                "%O,%o,%T,%V"
                " FROM %o"
                " WHERE"
                " %o LIKE %L{search-text}"
                " AND col2 IN (%V)")
              '("a" "b")
              "c" "'text"
              "something"
              "table"
              "d" '("foo" 1)))))
  (should (equal
           (concat
            "INSERT INTO (\"a\", \"b\")\n"
            " VALUES ('1', 2) ")
           (esqlite-format
            '(
              "INSERT INTO (%O)"
              " VALUES (%V) ")
            '("a" "b") '("1" 2))))

  ;; malicious name
  (should
   (equal 
    "\"odd \"\" table\""
    (esqlite-format "%o" "odd \" table")))

  )

(ert-deftest esqlite-number ()
  :tags '(esqlite)
  (should (esqlite-numeric-text-p "1"))
  (should (esqlite-numeric-text-p "+2"))
  (should (esqlite-numeric-text-p "-3"))
  (should (esqlite-numeric-text-p "+4.0"))
  (should (esqlite-numeric-text-p "+4.0E50")))
