(require (planet "sqlite.ss" ("jaymccarthy" "sqlite.plt" 3)))
(define *db* (open (build-path "/tmp/yow.db")))

(exec/ignore *db*
             "create table if not exists foo(key blob primary key on conflict replace, value blob, timestamp blob)")
(let ((insert (prepare *db*  "insert into foo values (?, ?, ?)")))
  (with-handlers
      ([void void])
    (run insert "snicker" "snack" (current-milliseconds)))
  (finalize insert))
(write (select *db* "select * from foo"))
(newline)
