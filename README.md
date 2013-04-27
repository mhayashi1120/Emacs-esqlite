Emacs sqlite3
=============

## UI

### Edit cell

TODO when enter the cell edit start transaction
TODO discard (rollback edited)


### Add row


### Delete row

### Commit transaction


## API

### Stream API

staying permanently as emacs subprocess

### Reader API

Read a query result each time program request.
Must close this reader.

sample:

### Onetime reader API

Respond as csv line from subprocess, each time result is arrived.

sample:

### Onetime query API

Respond as list of csv from synchronous process.

