#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module history mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1")
         (lib "pretty.ss")
         (only (lib "1.ss" "srfi" ) every append-map remove drop-right list-copy)
         (all-except "trick.ss" whose-turn)
         (rename "trick.ss" trick:whose-turn whose-turn)
         (lib "trace.ss"))
(provide (all-defined-except make-history)
         (rename my-make-history make-history))

(define-struct history (opening-leader tricks) #f)

(define (my-make-history tricks-or-opening-leader)

  (define (mostly-complete? tricks)
    (or (null? tricks)
        (null? (cdr tricks))
        (and (trick-complete? (car tricks))
             (mostly-complete? (cdr tricks)))))

  (if (list? tricks-or-opening-leader)
      (let ((tricks tricks-or-opening-leader))
        (unless (every trick? tricks)
          (raise-mismatch-error 'make-history "I want a list, not " tricks))
        (unless (mostly-complete? tricks)
          (raise-mismatch-error 'make-history "I want mostly complete tricks, not " tricks))
        (make-history (leader (car tricks )) tricks))
    (let ((opening-leader tricks-or-opening-leader))
      (unless (member opening-leader *seats*)
        (raise-mismatch-error 'make-history (format "leader must be in ~a, not " *seats*) opening-leader))
      (make-history opening-leader '()))))

;; I considered keeping the length in a cache (i.e., a separate slot
;; in the history structure), rather than recomputing it each time as
;; I do here, but that only sped the overall program up by about 2%.
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
  ;; copy the history wholesale, then either add a new trick, or add a
  ;; card to the last one
  (let ((rv (make-history (history-opening-leader h)
                          (list-copy (history-tricks h)))))

    (set-history-tricks!
     rv
     (cond
      ((history-empty? rv)
       (list (make-trick (list c) (history-opening-leader rv))))
      ((let ((l (history-latest-trick rv)))
         (and (trick-complete? l)
              l))
       => (lambda (l)
            (append (history-tricks rv)
                    (list (make-trick (list c) (winner l))))))
      (else
       (append (drop-right (history-tricks rv) 1)
               (list (t:add-card (t:copy (history-latest-trick rv)) c))))))

    (assert (= 1 (- (length (history-card-set rv))
                    (length (history-card-set h)))))
    rv))

)
