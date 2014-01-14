Manipulate sqlite file from Emacs
=================================

## API

### Stream API

staying permanently as emacs subprocess
Programmer must manage the stream todo

[Function] esqlite-stream-open
[Function] esqlite-stream-read, esqlite-stream-read-top, esqlite-stream-read-atom, esqlite-stream-read-list
[Function] esqlite-stream-close

sample:

    (setq my-stream (esqlite-stream-open "database.sqlite"))
    (unwind-protect
        (esqlite-stream-read my-stream "SELECT * FROM hoge")
      (esqlite-stream-close my-stream))

### Sync Read API

Respond as list or string from synchronous process.

[Function] esqlite-read, esqlite-read-top, esqlite-read-atom, esqlite-read-list

    (esqlite-read "database.sqlite" "SELECT * FROM hoge")

=>  (("a" "b") ("c" "d"))

### Async Read API

Respond as csv line from async subprocess, each time result is arrived from process.

sample:

    (esqlite-async-read "database.sqlite" "SELECT * FROM hoge"
	   (lambda (data) (message "%s" (mapcocnat 'identity data ", "))))
	   
### Read DB schema

[Function] esqlite-read-all-objects, esqlite-read-views, esqlite-read-tables, esqlite-read-indexes, esqlite-read-triggers
[Function] esqlite-read-table-schema, esqlite-read-table-columns

sample: todo

### Construct SQL

[Function] esqlite-escape-string

    (esqlite-escape-string  "a'b")

  => "a''b" 

[Function] esqlite-format-text

     (esqlite-text "a'b")
   
  => "'a''b'"

[Function] esqlite-escape-like

      (esqlite-escape-like "F%OO_" ?\\)
	  
  => "F\\%OO\\_"

[Function] esqlite-format

     (esqlite-format
       `(
        "SELECT name "
        " FROM sqlite_master "
        " WHERE 1 = 1 "
        ,@(and type
               `(" AND type = %T{type}"))))

[Function] esqlite-format-blob

todo

## Helm

To construct helm source from sqlite database.

https://github.com/emacs-helm/helm

[Function] esqlite-helm-define

