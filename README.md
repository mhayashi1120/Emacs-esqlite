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

sample: todo

### Reader API

Read a query result each time program request.
Must close this reader.

sample: todo

### Read API

Respond as list of csv from synchronous process.

sample: todo


    (sqlite3-read filename "SELECT * FROM hoge")

=>  (("a" "b") ("c" "d"))


### Async Read API

Respond as csv line from async subprocess, each time result is arrived.

sample: todo



    (sqlite3-async-read filename "SELECT * FROM hoge"
	   (lambda (data) (message "%s" (mapcocnat 'identity data ", "))))
	   
TODO

