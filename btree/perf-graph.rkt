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

(define (size->times n constructor)
  (set! n (inexact->exact (round n)))
  (define l (shuffle (build-list n values)))
  (define d (list->dict (constructor) l))
  (cpu-time
   (thunk
    (for/fold ([d d])
        ([elt l])
        (dict-remove d elt)))))

(parameterize ([plot-font-size 18])
  (let ([lx 100]
        [ux 200])

    (define (quickfunc label ps color ctor)
      (function #:label label
                #:style ps
                #:color color
                #:width 2
                (curryr size->times ctor) lx ux))

    (time
     (plot (list (quickfunc "tree"  'solid     0 tree )
                 (quickfunc "alist" 'dot       1 (thunk '()) )
                 (quickfunc "hash"  'long-dash 2 hash ))
           #:title "Time to delete all the elements, one by one"
           #:x-label "number of elements in dictionary"
           #:y-label "CPU time, ms"))))
