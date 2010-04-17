#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (except-in "incubot.ss" main))

(define make-incubot-server
  (match-lambda
   [(? string? ifn)
    (call-with-input-file ifn make-incubot-server)]
   [(? input-port? ip)
    (let ([c (time
              (for/fold ([c (public-make-corpus)])
                  ([line (in-lines ip)])
                  (add-to-corpus line c)))])
      (lambda (inp) (incubot-sentence inp  c)))
    ])
  )

(provide main)
(define (main . args)
  (let ([s (make-incubot-server "/tmp/davinci.txt")])
    (for ([inp (in-list (list
                         "Oh shit"
                         "Oops, ate too much cookie dough"
                         "It's almost inconceivable that none of these words appears in that manual"
                         "I'm impressed that I can find stuff already."))])
      (printf "~a => ~a~%" inp (s inp)))))
