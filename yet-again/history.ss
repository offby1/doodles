#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module history mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1")
         (only (lib "1.ss" "srfi" ) every append-map)
         (all-except "trick.ss" whose-turn)
         (rename "trick.ss" trick:whose-turn whose-turn)
         (lib "trace.ss"))
(provide (all-defined-except make-history)
         (rename my-make-history make-history))

(define-struct history (tricks) #f)
(define (my-make-history tricks)
  (unless (and (list? tricks)
               (every trick? tricks))
    (raise-mismatch-error 'make-history "I want a list, not " tricks))
  (make-history tricks))
(define (history-length h)
  (length (history-tricks h)))
(define (history-empty? h)
  (zero? (history-length h)))
(define (history-latest-trick h)
  (list-ref (history-tricks h)
            (sub1 (history-length h))))
(define (history-complete? h)
  (and (= 13 (history-length h))
       (trick-complete? (history-latest-trick h))))
(define (history-card-set h)
  (append-map trick-cards (history-tricks h)))

(define (whose-turn h)
  (assert (not (history-complete? h)))
  (let ((latest (history-latest-trick h)))
    (if (trick-complete? latest)
        (winner latest)
      (trick:whose-turn latest))))

;; nondestructive
(define (add-card h c)
  (let ((t (history-latest-trick h)))
    (my-make-history
     (if (trick-complete? t)
         (cons (make-trick (list c)
                           (cdr (winner t)))
               (history-tricks h) )
       (cons (make-trick (cons c (trick-cards t)) (trick:whose-turn t))
             (cdr (history-tricks h)))))))
;(trace add-card)
)
