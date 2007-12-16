(require (planet "sqlite.ss" ("jaymccarthy" "sqlite.plt" 3)))
(define *db* (open (build-path "/tmp/yow.db")))

(with-handlers
    ([exn:sqlite?
      (lambda (e)
        (unless (regexp-match #rx"table .* already exists" (exn-message e))
          (raise e)))])
  (exec/ignore *db* "create table foo(key varchar, value varchar, timestamp varchar)"))
(let ((insert (prepare *db*  "insert into foo values (?, ?, ?)")))
  (with-handlers
      ([void void])
    (run insert "snicker" "snack" (number->string (current-milliseconds))))
  (finalize insert))
(write (select *db* "select * from foo"))
(newline)
