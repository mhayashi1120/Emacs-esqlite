(require 'ert)

(ert-deftest sqlite3-normal-0001 ()
  :tags '(sqlite3)
  (let* ((db (make-temp-file "sqlite3-test-"))
         (stream (sqlite3-stream-open db)))
    (unwind-protect
        (progn
          (should (sqlite3-stream--send-query stream "CREATE TABLE hoge (id INTEGER PRIMARY KEY, text TEXT)"))
          (should (equal (sqlite3-db-table-schema db "hoge")
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

