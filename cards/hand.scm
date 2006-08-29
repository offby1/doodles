#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module hand mzscheme
(require (only (lib "43.ss" "srfi") vector-for-each vector-unfold)
         (only (lib "setf.ss" "swindle") push!)
         (only (lib "list.ss") sort)
         (lib "trace.ss")
         (lib "setf.ss" "swindle"))
(provide *num-ranks*
         num-suits
         shape
         get-hand
         (rename my-make-card make-card)
         (rename my-make-hand make-hand))

(define *suits* '(clubs diamonds hearts spades))
(define (num-suits) (length *suits*))
(define *num-ranks* 13)

(define-struct card (rank suit) #f)
(define (my-make-card rank suit)

  (unless (member suit *suits*)
    (error 'make-card "Wanted one of ~a, but got ~s" *suits* suit))

  (unless (or (member rank '(jack queen king ace))
              (and (number? rank)
                   (exact? rank)
                   (integer? rank)
                   (<= 2 rank 10)))
    (error 'make-card "Wanted integer twixt 2 and 14, but got ~s" rank))
  (make-card rank suit))

(define-struct hand (suit-holdings) #f)
(define (my-make-hand)
   (make-hand (hash-table-copy (make-immutable-hash-table (map (lambda (suit)
                                                                 (cons suit '()))
                                                               *suits*)))))

(define (hash-table-push! table key new-value)
  (hash-table-put! table key (cons new-value (hash-table-get table key))))

(define (hand-add-card! h c)
  (hash-table-push!  (hand-suit-holdings h) (card-suit c) (card-rank c)))

(define (get-hand n deck)
  (let ((h (my-make-hand)))
    (vector-for-each
     (lambda (i c)
       (hand-add-card! h c))

     (vector-unfold (lambda (index seed)
                      (values (vector-ref deck (+ (* n *num-ranks*) index)) seed))
                    *num-ranks*
                    0))
    h))

(define (hand-map suit-func hand)
  (hash-table-map (hand-suit-holdings hand) (lambda (key value)
                                              (suit-func value)) ))

(define (shape hand)
  (sort (hand-map length hand) >))
)