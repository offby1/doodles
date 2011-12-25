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

(parameterize ([plot-font-size 18])
  (let ([lx 100]
        [ux 200])

    (define (quickfunc label ps color ctor vg)
      (function #:label label
                #:style ps
                #:color color
                #:width 2
                (curryr size->times ctor vg) lx ux))

    (time
     (plot (list (quickfunc "tree, ordered" 'solid 0 tree values)
                 ;;(quickfunc "vector, ordered" vector values)
                 (quickfunc "tree, random"  'dot 1 tree r)
                 (quickfunc "hash, ordered" 'long-dash 2 hash values))
           #:x-label "number of elements in dictionary"
           #:y-label "CPU time, ms"))))
