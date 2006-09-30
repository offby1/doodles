#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module hand mzscheme
(require (only (lib "1.ss" "srfi") every list-copy)
         (only (lib "13.ss" "srfi")  string-join)
         (only (lib "list.ss") remove sort)
         "card.ss"
         (only "trick.ss" *seats*)
         (lib "trace.ss"))
(provide (rename my-make-hand make-hand)
         (rename hand-cards cards)
         (rename hand-seat seat)
         ->string
         hand?
         empty?
         remove-card
         add-card!
         sort!)
(define-struct hand (cards seat) #f)
(define (my-make-hand cards . seat)
  (unless (and (list? cards)
               (every card? cards))
    (raise-mismatch-error 'make-hand "Not a list of cards: " cards))

  (when (not (null? seat))
    (set! seat (car seat))              ; "car seat".  Haw haw.

    (unless (memq seat *seats*)
      (raise-mismatch-error 'make-hand (format "Seat gotta be one of ~a, not " *seats*) seat)))

  (make-hand cards (if (null? seat)
                       'unknown
                     seat)))
;(trace my-make-hand)
(define (copy h)
  (make-hand (list-copy (hand-cards h)) (hand-seat h)))

(define (all-distinct? seq < =)
  (let loop ((seq (sort seq <)))
    (cond
     ((null? seq) #t)
     ((null? (cdr seq)) #t)
     ((= (car seq)
         (cadr seq))
      #f)
     (else
      (loop (cdr seq))))))
(define (remove-card! h c)
  (unless (member c (hand-cards h))
    (raise-mismatch-error 'remove-card (format "Can't remove from ~a because it's not present: "  h) c))

  (set-hand-cards! h (remove c (hand-cards h)))
  h)
(define (remove-card h c)
  (let ((new (copy h)))
    (remove-card! new c)
    new))

(define (add-card! h c)
  (if (member c (hand-cards h))
      (raise-mismatch-error 'add-card! (format "Can't add to ~a because it's already present: "  h) c))
  (set-hand-cards! h (cons c (hand-cards h)))
  h)

(define (sort! h)
  (set-hand-cards! h (sort (hand-cards h) card<)))

(define (empty? h)
  (null? (hand-cards h)))
(define (->string h)
  (string-join (map ca->string (reverse (hand-cards h)))))

)
