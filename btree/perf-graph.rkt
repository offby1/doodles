#lang racket

;; Load me in DrRacket (version 5.2 or better), then click "run", to
;; see a spiffy graph.

(require (except-in "btree.rkt" main)
         plot)

(define (list->dict dict seq)
  (for/fold ([dict dict])
      ([elt seq])
      (dict-set dict elt elt)))

(define (gc-time thunk)
  (let-values ([(results cpu real gc) (time-apply thunk '())])
    gc))

(define (cpu-time thunk)
  (let-values ([(results cpu real gc) (time-apply thunk '())])
    cpu))

(define (size->times n constructor value-generator)
  (set! n (inexact->exact (round n)))
  (define l (build-list n value-generator))
  (define dict (constructor))
  (cpu-time (thunk (list->dict dict l))))

(define r (lambda (_) (random)))

(parameterize ([plot-x-transform log-transform])
  (let ([lx 100]
        [ux 200])

    (define (quickfunc label ctor vg)
      (function #:label label (curryr size->times ctor vg) lx ux))

    (plot (list (quickfunc "tree, ordered" tree values)
                ;;(quickfunc "vector, ordered" vector values)
                (quickfunc "tree, random"  tree r)
                (quickfunc "hash" hash r))
          #:x-label "number of elements in dictionary"
          #:y-label "CPU time, ms")))
