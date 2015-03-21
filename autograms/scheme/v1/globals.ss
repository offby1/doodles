#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

#lang scheme
(provide *min* *max*
         *tries* *loop-passes*
         *max-worker-mem* *worker-custodian*
         nl)
(define *min* 1)
(define *max* 48)

(define (make-modifiable-global)
  (let ((value 0))
    ;; you could wrap this in a call-with-semaphore if you wanted, but
    ;; I don't think there's any need
    (lambda args
      (when (not (null? args))
        (set! value (car args)))
      value
      )))

(define *worker-custodian* (make-custodian))
(define *max-worker-mem* (* 200 (expt 10 6)))
(custodian-limit-memory *worker-custodian* *max-worker-mem*)
(define *tries* (make-modifiable-global))
(define *loop-passes* (make-modifiable-global))

(port-count-lines! (current-error-port))
(port-count-lines! (current-output-port))
(define (nl)
  (let-values (((line col pos)
                (port-next-location (current-output-port))))
    (unless (zero? col)
      (newline))))
