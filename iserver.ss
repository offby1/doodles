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
           (let ([c (time
                     (for/fold ([c (public-make-corpus)])
                         ([line (in-lines ip)])
                         (add-to-corpus line c)))])
             (let loop ([c c])
               (match (channel-get *to-server*)
                 [(cons 'get inp)
                  (channel-put *from-server* (incubot-sentence inp c))
                  (loop c)]
                 [(cons 'put sentence)
                  (channel-put *from-server* #t)
                  (loop (add-to-corpus sentence c))])))))))

    (lambda (command-sym inp)
      (channel-put *to-server* (cons command-sym inp))
      (channel-get *from-server*))))

(provide main)
(define (main . args)
  (let ([s (make-incubot-server "/tmp/davinci.txt")])
    (define (try input)
      (printf "~a => ~s~%" input (time (s 'get input))))
    (try "Oh shit")
    (try "Oops, ate too much cookie dough")
    (s 'put "What is all this shit?")
    (try "Oh shit")))

