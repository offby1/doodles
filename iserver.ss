#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (except-in "incubot.ss" main))

(define (make-incubot-server ifn)
  (let ([*to-server*   (make-channel)]
        [*from-server* (make-channel)])
    (thread
     (lambda ()
       (call-with-input-file ifn
         (lambda (ip)
           (let ([c (for/fold ([c (public-make-corpus)])
                        ([line (in-lines ip)])
                        (add-to-corpus line c))])
             (let loop ()
               (let ([inp (channel-get *to-server*)])
                 (channel-put *from-server* (incubot-sentence inp c)))
               (loop)))))))
    (lambda (inp)
      (channel-put *to-server* inp)
      (channel-get *from-server*))))

(provide main)
(define (main . args)
  (let ([s (time (make-incubot-server "/tmp/davinci.txt"))])
    (for ([inp (in-list (list
                         "Oh shit"
                         "Oops, ate too much cookie dough"
                         "It's almost inconceivable that none of these words appears in that manual"
                         "I'm impressed that I can find stuff already."))])
      (printf "~a => ~a~%" inp (time (s inp))))))
