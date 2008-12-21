#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(define-struct db (stuff) #:transparent)

(provide/contract [file->db [ string? . -> . db?]])
(define (file->db filename)
  (make-db 'stuff))

(provide/contract [lookup [string? db? . -> . (or/c string? false/c)]])
(define (lookup word db)
  (and (string? (db-stuff db))
      (db-stuff db)))

