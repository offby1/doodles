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
                                  ;; Only save this string if it's
                                  ;; longer than any other we've seen.
                                  (if (< (string-length existing)
                                         (string-length string))
                                      string
                                      existing))
                        "")))))

(provide/contract [lookup [string? db? . -> . (or/c string? false/c)]])
(define (lookup word db)
   (hash-ref (db-stuff db) word #f))

(provide main)
(define (main . args)
  (let ((db (port->db
             (apply
              input-port-append
              #t
              (map
               (lambda (infile-name)
                 (fprintf
                  (current-error-port)
                  "Queueing ~s ...~%" infile-name)
                 (open-input-file infile-name)) args)))))
    (call-with-output-file
        "/tmp/db.dump"
      (lambda (op)
        (write db op)
        (newline op))
      #:exists 'truncate)))
