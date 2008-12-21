#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(define-struct db (stuff) #:transparent)

(provide/contract [strings->db [->* () () #:rest (listof (or/c string? list?)) db?]])
(define (strings->db . strings-or-lists)
  (make-db (flatten strings-or-lists)))

(provide/contract [lookup [string? db? . -> . (or/c string? false/c)]])
(define (lookup word db)
  (match (db-stuff db)
    [(list val1 val ...)
     val1]
    [_ #f]))

