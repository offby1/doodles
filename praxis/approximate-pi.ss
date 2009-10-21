#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;;  http://programmingpraxis.com/2009/10/09/calculating-pi/

#lang scheme

(define (random-point)
  (make-rectangular (random) (random)))

(define (is-in-circle? p)
  (<= (magnitude p)
      1))

(define (make-calm-notifier proc)
  (define/contract (is-power-of-two? x)
    ((and/c natural-number/c positive?) . -> . boolean?)
    (or (= 1 x)
        (and (even? x)
             (is-power-of-two? ( / x 2)))))
  (let ((invocation-count 0))
    (lambda args
      (when (and (positive? invocation-count)
                 (is-power-of-two? invocation-count))
        (apply proc args))
      (set! invocation-count (add1 invocation-count)))))

(define (p val) (display val) (newline))

(define (main . args)
  (let-values
      (([ignore-me best-approximation]
        (let ([note (make-calm-notifier p)])
          (for/fold ([number-inside-circle 0]
                     [current-approximation #f])
              ([trial (in-range 1000000)])
              (note (cons trial current-approximation))
            (values
             ((if (is-in-circle? (random-point)) add1 values) number-inside-circle)
             (* 4.0 (/ number-inside-circle (add1 trial))))))))
    best-approximation))

(provide main)
