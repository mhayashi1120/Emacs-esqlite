esqlite.el
==========

Manipulate sqlite file from Emacs

## API

### Stream API

Staying permanently as a stream. Programmer can manage the stream as you like with using following API:

[Function] `esqlite-stream-open`

[Function] `esqlite-stream-read`, `esqlite-stream-read-top`, `esqlite-stream-read-atom`, `esqlite-stream-read-list`

[Function] `esqlite-stream-close`

sample:

```
(setq my-stream (esqlite-stream-open "database.sqlite"))
(unwind-protect
    (esqlite-stream-read my-stream "SELECT * FROM hoge")
  (esqlite-stream-close my-stream))
```

### Sync Read API

Respond as list or string from synchronous process.

[Function] `esqlite-read`, `esqlite-read-top`, `esqlite-read-atom`, `esqlite-read-list`

```
(esqlite-read "database.sqlite" "SELECT * FROM hoge")
```

=>  (("a" "b") ("c" "d"))

### Async Read API

Respond as csv line from async subprocess, each time result is arrived from process.

sample:

```
(esqlite-async-read "database.sqlite" "SELECT * FROM hoge"
   (lambda (data) (message "%s" (mapconcat 'identity data ", "))))
```

### Read DB schema

[Function] `esqlite-read-all-objects`, `esqlite-read-views`, `esqlite-read-tables`, `esqlite-read-indexes`, `esqlite-read-triggers`

[Function] `esqlite-read-table-schema`, `esqlite-read-table-columns`

Read database schema as a list

Here is a firefox "places.sqlite" sample db:

```
(esqlite-read-tables "places.sqlite")
```

  => ("moz_anno_attributes" "moz_annos" "moz_bookmarks" "moz_bookmarks_roots" "moz_favicons" "moz_historyvisits" "moz_inputhistory" "moz_items_annos" "moz_keywords" "moz_places" "sqlite_sequence" "sqlite_stat1")

```
(esqlite-read-table-schema
 "places.sqlite"
 "moz_anno_attributes")
```

=> ((0 "id" "INTEGER" nil :null t) (1 "name" "VARCHAR(32)" t :null nil))

### Construct SQL

[Function] `esqlite-format-value`

  The most convenient function to encode Emacs object to SQL.

```
(esqlite-format-value "a")
```

=> "'a'"

```
(esqlite-format-value 1)
```

=> "1"

```
(esqlite-format-value '(1 2 3))
```

=> "1, 2, 3"

```
(esqlite-format-value '("a" "b"))
```

=> "'a', 'b'"

[Function] `esqlite-prepare`

```
(let ((type "table"))
  (esqlite-prepare
   `(
     "SELECT name "
     " FROM sqlite_master "
     " WHERE 1 = 1 "
     ,@(and type
            `(" AND type = %T{type}")))
   :type type))
```

[Function] `esqlite-format-text`

```
(esqlite-format-text "a'b")
```

=> "'a''b'"

[Function] `esqlite-format-blob`

```
(esqlite-format-blob "a\00b")
```

=> "x'610062'"

[Function] `esqlite-escape-string`

```
(esqlite-escape-string  "a'b")
```

=> "a''b"

[Function] `esqlite-escape-like`

```
(esqlite-escape-like "F%OO_" ?\\)
```

=> "F\\%OO\\_"

## Helm

To construct helm source from sqlite database.

https://github.com/emacs-helm/helm

[Function] `esqlite-helm-define`


## URI filenames

[Function] `esqlite-filename-to-uri`

http://www.sqlite.org/uri.html
Convert local filename to sqlite URI filenames
