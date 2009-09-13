#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; simple command-line thing that does a Google search.

#lang scheme

(require net/url srfi/13
         (planet dherman/json:3:0))

(provide main)
(define (main . args)
  (let ((url (string->url "http://ajax.googleapis.com/ajax/services/search/web")))
    (set-url-query! url `((v . "1.0")
                          (q
                           .
                           ,(string-join (vector->list (current-command-line-arguments)) " "))))

    (let ([big-hash (call/input-url
                     url
                     get-pure-port
                     read-json)])

      (match big-hash
        [(hash-table
          ('responseStatus responseStatus)
          ('responseDetails responseDetails)
          ('responseData responseData))
         (case responseStatus
           ((200)
            (let ([hits (hash-ref responseData 'results)])
              (printf "Where the hits keep on coming~%")
              (pretty-print (map (lambda (h)
                                   (for/list ([key (in-list '(titleNoFormatting url))])
                                     (cons key (hash-ref h key))))
                                 hits))))
           (else
            (fprintf (current-error-port)
                     "Oh noes, search failed with ~a: ~s~%" responseStatus responseDetails)))]))))
