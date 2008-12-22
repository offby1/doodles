#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require srfi/13)

(define-struct db (stuff) #:prefab)

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

;; ip -> ip
(define (strip-irc-protocol-chatter ip)
  (define (transform line)
    (regexp-replace
     #px"PRIVMSG #[^[:blank:]]+ *:"
     (regexp-replace
      #px"^:[^[:blank:]]* *"
      line
      "")
     ""))
  (let-values (((pipe-ip pipe-op)
                (make-pipe 500)))
    (thread (lambda ()
              (let loop ()
                (let ((line (read ip)))
                  (if (eof-object? line)
                      (close-output-port pipe-op)
                      (begin
                        (display (transform line) pipe-op)
                        (newline pipe-op)
                        (loop)))))))
    pipe-ip))

(define *db-dump-file-name* "db.dump")

(define (cwif fn proc message-fn)
  (call-with-input-file fn
    (lambda (ip)
      (fprintf (current-error-port) "~a ... " (message-fn fn))
      (begin0
          (proc ip)
        (fprintf (current-error-port) "done~%")))))

(provide main)
(define (main . args)
  (with-handlers
      ([exn:fail:filesystem?
        (lambda (e)
          ;; TODO -- count the lines in the input, and then provide some
          ;; feedback as we chew through them
          (let ((db (cwif
                     "irc-lines"
                     (lambda (ip)
                       (port->db
                        (strip-irc-protocol-chatter
                         ip)))
                     (lambda (fn) (format "Reading ~s" fn)))))
            (call-with-output-file
                *db-dump-file-name*
              (lambda (op)
                (pretty-print db op))
              #:exists 'truncate))

          (apply main args))])
    (let ((db (call-with-input-file
                  *db-dump-file-name*
                (lambda (ip)
                  (fprintf
                   (current-error-port)
                   "Reading from ~s ..."
                   ip)
                  (begin0
                      (read ip)
                    (fprintf (current-error-port) "done~%"))))))
      (for ([word args])
        (printf "~s => ~s~%" word (lookup word db))))))


