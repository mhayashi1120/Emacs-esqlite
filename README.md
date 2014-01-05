sqlite manipulation for Emacs
=============================


## API

### Stream API

staying permanently as emacs subprocess
Programmer must manage the stream todo

[Function] esqlite-stream-open
[Function] esqlite-stream-read, esqlite-stream-read-top, esqlite-stream-read-atom
[Function] esqlite-stream-close

sample:

    (setq my-stream (esqlite-stream-open "database.sqlite"))
    (unwind-protect
        (esqlite-stream-read my-stream "SELECT * FROM hoge")
      (esqlite-stream-close my-stream))

### Sync Read API

Respond as list of csv from synchronous process.

[Function] esqlite-read, esqlite-read-top, esqlite-read-atom

    (esqlite-read "database.sqlite" "SELECT * FROM hoge")

=>  (("a" "b") ("c" "d"))

### Async Read API

Respond as csv line from async subprocess, each time result is arrived.

sample:

    (esqlite-async-read "database.sqlite" "SELECT * FROM hoge"
	   (lambda (data) (message "%s" (mapcocnat 'identity data ", "))))
	   
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

## Helm

To construct helm source from sqlite database.

https://github.com/emacs-helm/helm

[Function] esqlite-helm-define

