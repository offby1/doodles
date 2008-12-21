#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require srfi/13)

(define-struct db (stuff) #:transparent)

(provide/contract [port->db [input-port? . -> . db?]])
(define (port->db ip)
  (make-db
   (for/fold ([db (make-immutable-hash '())])
       ([string (in-lines ip)])
       (for/fold ([db db])
           ([word (in-list (string-tokenize string))])
           (hash-update db word (lambda (existing)
                                  (cons string existing))
                        '()
                        )))))

(provide/contract [lookup [string? db? . -> . (or/c string? false/c)]])
;; TODO -- do sorting at insertion time, not lookup time
(define (lookup word db)
  (let ((found (hash-ref (db-stuff db) word #f)))
    (and (list? found)
         (car (sort found > #:key string-length)))))

