#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; Stuff to examine statistical properties of a bridge hand.  For
;; example, if I deal 1000 hands, what's the longest suit that I can
;; expect to appear?

(module cards mzscheme
(require (only (lib "1.ss" "srfi")
               filter
               unfold)
         (only (lib "43.ss" "srfi") vector-unfold)
         (lib "pretty.ss")
         (planet "histogram.ss" ("offby1" "offby1.plt"))
         (planet "fys.ss" ("offby1" "offby1.plt"))
         "hand.scm")

(define *deck-size* (* *num-ranks* (num-suits)))

(define (new-deck)
  (fisher-yates-shuffle!
   (vector-unfold
    (lambda (index seed)
      (values (make-card
               (vector-ref #(2 3 4 5 6 7 8 9 10 jack queen king ace)  (remainder index *num-ranks*))
               (vector-ref #(clubs diamonds hearts spades) (quotient  index *num-ranks*)))
              seed))
    *deck-size*
    0)))

(define (notrumpy? shape)
  (member shape '((4 4 3 2) (5 3 3 2) (4 3 3 3))))

(define (five-five-or-better? shape)
  (< 1 (length (filter (lambda (length)
                         (< 4 length))
                       shape))))

;; Deal a bunch of deals, and report the shapes of each hand.
(let ((deals 10000))

  (printf "After ~a deals, suit lengths were distributed like this ('5' represents 5-5 or better shape; '*' represents notrump shape) :~%"
          deals)

  (parameterize ((pretty-print-columns 24))
    (pretty-display
     (map (lambda (entry)
            (let ((shape (car entry)))
              (cond
               ((five-five-or-better? shape)
                (cons entry '(5)))
               ((notrumpy? shape)
                (cons entry '(*)))
               (list entry))))
          (cdr-sort
           (list->histogram
            (unfold
             (lambda (p) (= deals  p))
             (lambda ignored (shape (get-hand 0 (new-deck))))
             add1
             0))))))))

