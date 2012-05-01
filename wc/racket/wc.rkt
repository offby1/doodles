#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require "bfs.rkt"
         unstable/debug)

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

(define-flexible-reader read-dictionary
  (lambda (inp)

    (define (diag string thunk)
      (display string (current-error-port))
      (begin0
          (thunk)
        (display "done" (current-error-port))
        (newline (current-error-port))))

    (diag
     (format "Reading ~a..." inp)
     (thunk
      (define (letters-only str) (regexp-replace* #px"[^[:alnum:]]+" str ""))

      (for/fold ([words-by-length (make-immutable-hash)])
          ([line (in-lines inp)])
          (let ([word (string-downcase (letters-only line))])
            (hash-update
             words-by-length
             (string-length word)
             (lambda (s) (set-add s word))
             (set))))))))

(define *alphabet*
  (for/set ([i (in-range (char->integer #\a) (add1 (char->integer #\z)))])
           (integer->char i)))

(define (neighbors word words)

  (define (25-varieties word index avoid-this-character)
    (for/list ([ch *alphabet*]
               #:when (not (char=? ch avoid-this-character)))
      (string-append
       (substring word 0 index)
       (string ch)
       (substring word (add1 index) (string-length word)))))

  (for/fold ([n (list)])
      ([(ch i) (in-indexed word)])
      (append (filter (curry set-member? words) (25-varieties word i ch)) n)))

(define *dict-desription* 'unknown)
(define progress-indicator-thread
  (thread
   (thunk
    (let loop ()
      (when (not (eq? 'unknown *dict-desription*))
        (fprintf (current-error-port) "Dictionary: ~a~%" *dict-desription*))
      (sleep 5)
      (loop)))))

(provide main)
(define (main . args)

  (define word-length (string->number (first args)))

  (let ([dict (read-dictionary "/usr/share/dict/words")])
    (let loop ([same-length-words (hash-ref dict word-length)]
               [longest '((dummy . ()))])

      (define (one-item-from-set s) (for/first ([item (in-set s)]) item))

      (set! *dict-desription* (format "~a ~a-letter words" (set-count same-length-words) word-length))
      (if (set-empty? same-length-words)
          (pretty-print (first longest))
          (let ([h (sort
                    (dict-map
                     (b-f-traverse
                      (one-item-from-set same-length-words)
                      (curryr neighbors same-length-words))
                     cons) > #:key (compose length cdr))
                   ])
            (loop (set-subtract same-length-words (apply set (dict-keys h)))
                  (if (> (length (cdr (first h)))
                         (length (cdr (first longest))))
                      h
                      longest))
            )))))
