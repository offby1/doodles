#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec gracket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require plot
         "btree.rkt")

(define (list->dict dict seq)
  (for/fold ([dict dict])
      ([elt seq])
      (dict-set dict elt elt)))

;; Keeps the plot package from computing the log of zero
(define (nonzero x)
  (if (zero? x)
      0.01
      x))

(define (gc-time thunk)
  (let-values ([(results cpu real gc) (time-apply thunk '())])
    (nonzero gc)))

(define (cpu-time thunk)
  (let-values ([(results cpu real gc) (time-apply thunk '())])
    (nonzero cpu)))


;; Sorta like "shuffle", but all the small elements are in the first
;; half of the result.
(define (half-shuffle seq)
  (let* ([midpoint (round (/ (length seq) 2))]
         [one (take seq midpoint)]
         [two (list-tail seq midpoint)])
    (append (shuffle one)
            (shuffle two))))

(define (time-list-consumer n list-chewer)
  (let ([l (shuffle
            (build-list
             (inexact->exact (round  n))
             values))])
    (cpu-time
     (list-chewer
      l))))

(define (perf-test-delete n dict-ctor)
  (time-list-consumer
   n
   (lambda (l)
     (define d (list->dict (dict-ctor) l))
     (thunk
      (for/fold ([d d])
          ([elt l])
          (dict-remove d elt))))))

(define (perf-test-insert n dict-ctor)
  (time-list-consumer
   n
   (lambda (l)
     (thunk
      (list->dict (dict-ctor) l)))))

(provide main)
(define (main . args)
  (parameterize ([plot-y-transform log-transform]
                 [plot-x-transform log-transform]
                 [plot-font-size 18]
                 [plot-new-window? #t]
                 [plot-width 800]
                 [plot-height 650]
                 [line-samples 10])
    (let ([lx 200]
          [ux 5000])

      (define (quickfunc label perf-test ps color ctor)
        (function #:label label
                  #:style ps
                  #:color color
                  #:width 2
                  (curryr perf-test ctor) lx ux))

      (time
       (plot
        (list
         (quickfunc "insert: tree"  perf-test-insert 'dot   3 tree )
         (quickfunc "delete: tree"  perf-test-delete 'solid 3 tree )
         (quickfunc "insert: alist" perf-test-insert 'dot   2 (thunk '()) )
         (quickfunc "delete: alist" perf-test-delete 'solid 2 (thunk '()) )
         (function #:label "identity" (curry * 3/50) #:style 'short-dash)
         )
        #:title "Various dict operation times"
        #:x-label "number of elements in dictionary"
        #:y-label "CPU time, ms")))))
