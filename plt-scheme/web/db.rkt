#lang racket

(require (prefix-in sqlite: (planet jaymccarthy/sqlite:4)))

(define *db-filename* "/tmp/tinyurl.db")
(define *db* #f)

(provide db-init!)
(define (db-init!)
  (set! *db* (sqlite:open (string->path *db-filename*)))
  (with-handlers ([exn? void])
    (sqlite:exec/ignore *db*
                        (string-append
                         "CREATE TABLE shortened "
                         "(short TEXT PRIMARY KEY,"
                         "long TEXT)"))))

(provide db-lookup)
(define (db-lookup key)
  (let ([results (sqlite:select *db* "SELECT long FROM shortened WHERE short = ?" key)])
    (and (not (null? results))
         (vector-ref (second results) 0))))

(provide db-add!)
(define (db-add! key value)
  (sqlite:insert *db* "INSERT INTO shortened VALUES (?, ?)" key value)
  value)
