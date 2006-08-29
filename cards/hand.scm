#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module hand mzscheme
(require (only (lib "43.ss" "srfi") vector-for-each vector-unfold)
         (only (lib "setf.ss" "swindle") push!)
         (only (lib "list.ss") sort))
(provide (all-defined))
(define *num-suits* 4)
(define *num-ranks* 13)
(define-struct card (rank suit) #f)
(define-struct hand (clubs diamonds hearts spades) #f)
;; ick ick ick.  I wonder if Eli has some clever way of avoiding
;; spelling out the accessor names.
(define (get-hand n deck)
  (let ((h (make-hand '() '() '() '())))
    (vector-for-each
     (lambda (i c)
       (case (card-suit c)
         ((clubs)   (push! c (hand-clubs    h)))
         ((diamonds)(push! c (hand-diamonds h)))
         ((hearts)  (push! c (hand-hearts   h)))
         ((spades)  (push! c (hand-spades   h)))))

     (vector-unfold (lambda (index seed)
                      (values (vector-ref deck (+ (* n *num-ranks*) index)) seed))
                    *num-ranks*
                    0))
    h))

(define (hand-map suit-func hand)
  (map (lambda (accessor)
         (suit-func (accessor hand)))
       (list hand-clubs hand-diamonds hand-hearts hand-spades)))

(define (shape hand)
  (sort (hand-map length hand) >))
)