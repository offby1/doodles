#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module history mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1")
         (lib "pretty.ss")
         (only (lib "1.ss" "srfi" ) every append-map remove drop-right)
         (all-except "trick.ss" whose-turn)
         (rename "trick.ss" trick:whose-turn whose-turn)
         (lib "trace.ss"))
(provide (all-defined-except make-history)
         (rename my-make-history make-history))

(define-struct history (tricks opening-leader) #f)

(define (my-make-history tricks)

  (define (mostly-complete? tricks)
    (or (null? tricks)
        (null? (cdr tricks))
        (and (trick-complete? (car tricks))
             (mostly-complete? (cdr tricks)))))

  (unless (and (list? tricks)
               (every trick? tricks))
    (raise-mismatch-error 'make-history "I want a list, not " tricks))
  (unless (mostly-complete? tricks)
    (raise-mismatch-error 'make-history "I want mostly complete tricks, not " tricks))
  (make-history tricks 'north))
(define (history-length h)
  (length (history-tricks h)))
(define (history-empty? h)
  (zero? (history-length h)))
(define (history-latest-trick h)
  (assert (not (history-empty? h)))
  (list-ref (history-tricks h)
            (sub1 (history-length h))))
(define (history-complete? h)
  (and (= 13 (history-length h))
       (trick-complete? (history-latest-trick h))))
(define (history-card-set h)
  (append-map trick-cards (history-tricks h)))

(define (whose-turn h)
  (assert (not (history-complete? h)))
  (if (history-empty? h)
      (history-opening-leader h)
    (let ((latest (history-latest-trick h)))
      (if (trick-complete? latest)
          (winner latest)
        (trick:whose-turn latest)))))

;(trace whose-turn)
;; nondestructive
(define (add-card h c)
  (if (history-empty? h)
      (my-make-history (list (make-trick (list c) (history-opening-leader h))))
    (let ((last (history-latest-trick h)))
      (my-make-history
       (if (trick-complete? last)
           (append (history-tricks h)
                   (list (make-trick (list c)
                                     (winner last))) )

         (append (drop-right (history-tricks h) 1)
                 (list (t:add-card last c)))
         )))))

;(trace add-card)
)
