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

(define (embiggen n)
  (expt 10 n))

(define log10
  (let ([l (log 10)])
    (lambda (x)
      (/ (log (+ x .1))
         l))))

(define (perf-test-delete n constructor)
  (set! n (inexact->exact (round (embiggen n))))
  (define l (shuffle (build-list n values)))
  (define d (list->dict (constructor) l))
  (log10
   (cpu-time
    (thunk
     (for/fold ([d d])
         ([elt l])
         (dict-remove d elt))))))

(define (perf-test-insert n constructor)
  (set! n (inexact->exact (round (embiggen n))))
  (define l (shuffle (build-list n values)))
  (log10
   (cpu-time
    (thunk
     (list->dict (constructor) l)))))

(define (go)
  (parameterize ([plot-font-size 18])
    (let ([lx 2]
          [ux 3])

      (define (quickfunc label perf-test ps color ctor)
        (function #:label label
                  #:style ps
                  #:color color
                  #:width 2
                  (curryr perf-test ctor) lx ux))

      (time
       (plot (list (quickfunc "insert: tree"  perf-test-insert 'solid     2 tree )
                   (quickfunc "insert: alist" perf-test-insert 'dot       2 (thunk '()) )
                   ;;(quickfunc "insert: hash"  perf-test-insert 'long-dash 2 hash )

                   (quickfunc "delete: tree"  perf-test-delete 'solid     3 tree )
                   (quickfunc "delete: alist" perf-test-delete 'dot       3 (thunk '()) )
                   ;; (quickfunc "delete: hash"  perf-test-delete 'long-dash 3 hash )
                   )
             #:title "Various dict operation times"
             #:x-label "log10 number of elements in dictionary"
             #:y-label "log10 CPU time, ms")))))
