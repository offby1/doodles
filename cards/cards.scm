#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; Stuff to examine statistical properties of a bridge hand.  For
;; example, if I deal 1000 hands, what's the longest suit that I can
;; expect to appear?

(module cards mzscheme
(require (only (lib "1.ss" "srfi") unfold)
         (only (lib "43.ss" "srfi") vector-unfold)
         (lib "pretty.ss")
         (lib "histogram.ss" "offby1")
         "hand.scm")

(print-struct #t)

(define *deck-size* (* *num-ranks* *num-suits*))

(define (num->rank n)
  (vector-ref #(2 3 4 5 6 7 8 9 10 jack queen king ace) n))
(define (num->suit n)
  (vector-ref #(clubs diamonds hearts spades) n))

(define (new-deck)
  (define (fisher-yates-shuffle! v)
    (define (swap! a b)
      (let ((tmp (vector-ref v a)))
        (vector-set! v a (vector-ref v b))
        (vector-set! v b tmp)))
    (do ((i 0 (add1 i)))
        ((= i (vector-length v))
         v)
      (let ((j (+ i (random (- (vector-length v) i)))))
        (swap! i j))))

  (fisher-yates-shuffle!
   (vector-unfold
    (lambda (index seed)
      (values (make-card
               (num->rank (remainder index *num-ranks*))
               (num->suit (quotient  index *num-ranks*)))
              seed))
    *deck-size*
    0)))

(define (notrumpy? shape)
  (member shape '((4 4 3 2) (5 3 3 2) (4 3 3 3))))

;; Deal a bunch of deals, and report the shapes of each hand.
(let ((deals 10000))

  (printf "After ~a deals, suit lengths were distributed like this ('*' represents notrump shape) :~%"
          deals)

  (parameterize ((pretty-print-columns 24))
    (pretty-display
     (map (lambda (entry)
            (let ((shape (car entry)))
              (if (notrumpy? shape)
                  (cons entry '(*))
                (list entry))))
          (cdr-sort
           (list->histogram
            (unfold
             (lambda (p) (= deals  p))
             (lambda ignored (shape (get-hand 0 (new-deck))))
             add1
             0))))))
  ))
