#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require "bfs.rkt")

;; Creates a function named FUNCTION-NAME that applies
;; INPUT-PORT-CONSUMER to an input port derived from its lone
;; argument.  If that argument is already an input port, it just uses
;; it unchanged; if it's a string or a path, then it opens the port in
;; the obvious way (and closes it when it's done).
(define-syntax-rule (define-flexible-reader function-name input-port-consumer)
  (define function-name
    (match-lambda
     [(? string? input-file-name)
      (function-name (build-path input-file-name))]
     [(? path? input-path)
      (call-with-input-file input-path function-name)]
     [(? input-port? inp)
      (input-port-consumer inp)])))

(define (diag string thunk)
  (display string (current-error-port))
  (begin0
      (thunk)
    (display "done" (current-error-port))
    (newline (current-error-port))))

(define (letters-only str) (regexp-replace* #px"[^[:alnum:]]+" str ""))

(define-flexible-reader read-dictionary
  (lambda (inp)
    (diag
     (format "Reading ~a..." inp)
     (thunk
      (for/fold ([words-by-length (make-immutable-hash)])
          ([line (in-lines inp)])
          (let ([word (letters-only line)])
            (hash-update
             words-by-length
             (string-length word)
             (lambda (s) (set-add s word))
             (set))))))))

(define *alphabet*
  (for/fold ([v (set)])
            ([i (in-range (char->integer #\a) (add1 (char->integer #\z)))])
    (set-add v (integer->char i))))

(define (build-word word index ch)
  (string-append
   (substring word 0 index)
   (string ch)
   (substring word (add1 index) (string-length word))))

(define (25-varieties word index avoid-this-character)
  (for/fold ([v (set)])
      ([ch *alphabet*])
      (if (char=? ch avoid-this-character)
          v
          (set-add v (build-word word index ch)))))

(define (potential-neighbors word)
  (for/fold ([n (set)])
      ([(ch i) (in-indexed word)])
      (set-union n (25-varieties word i ch))))

(define (real-words-only words length-to-words)
  (for/fold ([real (set)])
      ([w words])
      (if (set-member? (hash-ref length-to-words (string-length w) #f) w)
          (set-add real w)
          real)))

(define (real-neighbors word dict)
  (real-words-only (potential-neighbors word)
                   dict))

(provide main)
(define (main . args)
  (let ([dict (read-dictionary "/usr/share/dict/words")])
    (for ([word args])
      (pretty-print
       (sort
        (dict-map
         (bfs word (curryr real-neighbors dict))
         cons) < #:key cdr)))))
