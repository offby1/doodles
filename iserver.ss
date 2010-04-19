#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (except-in "incubot.ss" main))

(define (pf fmt . args)
  (apply fprintf (current-error-port) fmt args))

(define (make-incubot-server ifn)
  (let ([*to-server*   (make-channel)]
        [*from-server* (make-channel)])
    (define funcs-by-symbol
      (make-immutable-hash
       (list
        (cons 'get
              (lambda (inp c)
                (channel-put *from-server* (incubot-sentence inp c))
                c))
        (cons 'put
              (lambda (sentence c)
                (channel-put *from-server* #t)
                (add-to-corpus sentence c))))))
    (thread
     (lambda ()
       (call-with-input-file ifn
         (lambda (ip)
           (let ([c (time
                     (pf "Snarfing ~s ..." ifn)
                     (begin0
                         (for/fold ([c (public-make-corpus)])
                             ([line (in-lines ip)])
                             (add-to-corpus line c))
                       (pf "done~%"))
                     )])
             (let loop ([c c])
               (match (channel-get *to-server*)
                 [(cons symbol inp)
                  (loop ((hash-ref funcs-by-symbol symbol) inp c))])))))))

    (lambda (command-sym inp)
      (channel-put *to-server* (cons command-sym inp))
      (channel-get *from-server*))))

(provide main)
(define (main . args)
  (let ([s (make-incubot-server "/tmp/davinci.txt")])
    (define (get input) (s 'get input))
    (define (put sentence) (s 'put sentence))

    (define (try input) (printf "~a => ~s~%" input (time (get input))))

    (try "Oh shit")
    (try "Oops, ate too much cookie dough")
    (put "What is all this shit?")
    (try "hamsters")
    (try "Oh shit")))

