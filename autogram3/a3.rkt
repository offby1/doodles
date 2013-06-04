#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
PLTSTDERR=debug exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require  unstable/debug
          (planet neil/numspell/numspell)
          (planet dherman/memoize))

(define (add-two-hashes h1 h2)
  (for/fold ([rv h1])
      ([(k v) (in-dict h2)])
      (hash-update rv k (curry + v) 0)))

(define (add-hashes . hashes)
  (cond
   ((null? hashes)
    (make-immutable-hash))
   ((null? (cdr hashes))
    (car hashes))
   (else
    (foldl add-two-hashes (car hashes) (cdr hashes)))))

(define (string->hist str)
  (for/fold ([rv (make-immutable-hash)])
      ([ch str])
      (hash-update rv ch add1 0)))

(define (char->string ch histogram)
  (number->english (dict-ref histogram ch)))

(let ([template (list "This sentence has " #\e "s.")])

  (let-values ([(chars strings) (partition char? template)])
    (let loop ()
      (let* ([current-hist (apply add-hashes (map string->hist strings))]
             [updated-hist (map (curryr char->string current-hist) chars)])
        (append
         updated-hist
         strings)))))
