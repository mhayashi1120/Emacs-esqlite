Emacs sqlite3
=============

## UI

### Edit cell

TODO when enter the cell edit start transaction
TODO discard (rollback edited)


### Add row

TODO

### Delete row

TODO

### Commit transaction

TODO

## API

### Stream API

staying permanently as emacs subprocess
Programmer must manage the stream todo

[Function] sqlite3-stream-open
[Function] sqlite3-stream-read, sqlite3-stream-read-top, sqlite3-stream-read-atom
[Function] sqlite3-stream-close

sample: todo

### Reader API

Read a query result each time program request.
Must close this reader.

sample: todo

### Read API

Respond as list of csv from synchronous process.

[Function] sqlite3-read, sqlite3-read-top, sqlite3-read-atom

sample:

    (sqlite3-read filename "SELECT * FROM hoge")

=>  (("a" "b") ("c" "d"))




### Async Read API

Respond as csv line from async subprocess, each time result is arrived.

sample: todo



    (sqlite3-async-read filename "SELECT * FROM hoge"
	   (lambda (data) (message "%s" (mapcocnat 'identity data ", "))))
	   
TODO

### Construct SQL

[Function] sqlite3-text
[Function] sqlite3-escape-string 
[Function] sqlite3-escape-like
[Function] sqlite3-format
