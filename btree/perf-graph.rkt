#lang racket

;; Load me in DrRacket (version 5.2 or better), then click "run", to
;; see a spiffy graph.

(require (except-in "btree.rkt" main)
         plot)

(define (list->dict dict seq)
  (for/fold ([dict dict])
      ([elt seq])
      (dict-set dict elt elt)))

(define (cpu-time thunk)
  (let-values ([(results cpu real gc) (time-apply thunk '())])
    cpu))

(define (size->times n constructor)
  (set! n (inexact->exact (round n)))
  (define l (shuffle (build-list n values)))
  (define dict (constructor))
  (cpu-time (thunk (list->dict dict l))))

(plot (list (function #:label "tree" #:color 3 (curryr size->times tree) 1000 2000)
            (function #:label "hash" (curryr size->times hash) 1000 2000))
      #:x-label "number of elements in dictionary"
      #:y-label "total insertion time, ms")
