#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module history mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1")
         (lib "pretty.ss")
         (only (lib "list.ss") sort)
         (only "card.ss" card-suit)
         "zprintf.ss"
         (only (lib "1.ss" "srfi" ) every append-map remove drop-right list-copy)
         (all-except "trick.ss" whose-turn)
         (rename "trick.ss" trick:whose-turn whose-turn)
         (lib "trace.ss"))
(provide (all-defined-except make-history)
         (rename my-make-history make-history))

(define (history-print history port write?)
  (display "(" port)
  (let loop ((printed 0)
             (tricks (reverse (history-tricks history))))
    (when (not (null? tricks))
      (fprintf port "t ~a: ~a" (add1 printed) (car tricks))
      (when (not (null? (cdr tricks)))
        (display "; " port))
      (loop (add1 printed)
            (cdr tricks))))
  (display ")" port))

(define-values (s:history make-history history? s:history-ref history-set!)
  (make-struct-type 'history #f 2 0 #f
                    (list (cons prop:custom-write history-print)) #f))

(define (history-opening-leader h) (s:history-ref h 0))
;; TRICKS is a simple list of tricks, but in reverse chronological
;; order.  I.e., the car is the most recent.
(define (history-tricks         h) (s:history-ref h 1))

(define (history-complete-tricks-only h)
  (check-type 'history-complete-tricks-only history? h)
  (make-history
   (history-opening-leader h)
   (let ((ts (history-tricks h)))
     (cond
      ((or (null? ts)
           (trick-complete? (car ts)))
       ts)
      (else
       (cdr ts))))))

(define (set-history-tricks! h t) (history-set! h 1 t))

(define (my-make-history tricks-or-opening-leader)

  ;; are all tricks (except possibly the last) complete?
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
        (make-history (leader (car tricks )) (reverse tricks)))
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
  (car (history-tricks h)))
(define (hi:trick-complete? h)
  (trick-complete? (history-latest-trick h)))
(define (suit-led h)
  (card-suit (trick-ref (history-latest-trick h) 0)))
(define (history-complete? h)
  (and (= 13 (history-length h))
       (hi:trick-complete? h)))
(define (history-card-set h)
  (append-map trick-cards (history-tricks h)))
(define (history-length-cards h)
  (length (history-card-set h)))
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
  ;; either add a new trick, or add a card to the last one
  (let ((rv (make-history (history-opening-leader h)
                          (history-tricks h))))

    (set-history-tricks!
     rv
     (cond
      ((history-empty? rv)
       (list (make-trick (list c) (history-opening-leader rv))))
      ((let ((l (history-latest-trick rv)))
         (and (trick-complete? l)
              l))
       => (lambda (l)
            (cons (make-trick (list c) (winner l))
                  (history-tricks rv))))
      (else
       (cons (t:add-card (t:copy (history-latest-trick rv)) c)
             (cdr (history-tricks rv))))))

    (unless (= 1 (- (length (history-card-set rv))
                    (length (history-card-set h))))
      (error
       'add-card
       "Internal error: expected ~s to have exactly one more card than ~s"
       rv h))
    rv))

(define (compute-score h)
  (check-type 'compute-score history? h)
  (let ((tricks-by-seat (make-hash-table)))
    (define (hash-table-increment! k)
      (let ((v (hash-table-get tricks-by-seat k 0)))
        (hash-table-put! tricks-by-seat k (add1 v))))
    (for-each (lambda (t)
                (hash-table-increment! (winner t)))
              (history-tricks h))
    (map (lambda (seat)
           (cons seat
                 (hash-table-get tricks-by-seat seat 0)))
              *seats*)))
)
