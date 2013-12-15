Emacs sqlite3
=============

## API

### Stream API

staying permanently as emacs subprocess
Programmer must manage the stream todo

[Function] sqlite3-stream-open
[Function] sqlite3-stream-read, sqlite3-stream-read-top, sqlite3-stream-read-atom
[Function] sqlite3-stream-close

sample: todo

### Sync Read API

Respond as list of csv from synchronous process.

[Function] sqlite3-read, sqlite3-read-top, sqlite3-read-atom

    (sqlite3-read filename "SELECT * FROM hoge")

=>  (("a" "b") ("c" "d"))

### Async Read API

Respond as csv line from async subprocess, each time result is arrived.

sample: todo



    (sqlite3-async-read filename "SELECT * FROM hoge"
	   (lambda (data) (message "%s" (mapcocnat 'identity data ", "))))
	   
### Construct SQL

[Function] sqlite3-escape-string

    (sqlite3-escape-string  "a'b")

  => "a''b" 

[Function] sqlite3-text

     (sqlite3-text "a'b")
   
   => "'a''b'"

[Function] sqlite3-escape-like

      (sqlite3-escape-like "F%OO_" ?\\)
	  
  => "F\\%OO\\_"

[Function] sqlite3-format

     (sqlite3-format
       `(
        "SELECT name "
        " FROM sqlite_master "
        " WHERE 1 = 1 "
        ,@(and type
               `(" AND type = %T{type}"))))


## Helm

TODO helm url

todo sample of definitions helm
