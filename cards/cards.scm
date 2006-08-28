;; Stuff to examine statistical properties of a bridge hand.  For
;; example, if I deal 1000 hands, what's the longest suit that I can
;; expect to appear?

(module cards mzscheme
(require (only (lib "1.ss" "srfi") unfold)
         (only (lib "43.ss" "srfi") vector-unfold vector-for-each)
         (only (lib "setf.ss" "swindle") push!)
         (lib "pretty.ss")
         (lib "histogram.ss" "offby1")
         (only (lib "list.ss") sort))

(print-struct #t)
(define-struct card (rank suit) #f)
(define-struct hand (clubs diamonds hearts spades) #f)
(define *num-suits* 4)
(define *num-ranks* 13)
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

;; Deal a bunch of deals, and report the shapes of each hand.
(let ((deals 10000))

  (printf "After ~a deals, suit lengths were distributed like this:~%"
          deals)

  (parameterize ((pretty-print-columns 20))
    (pretty-display
     (cdr-sort
      (list->histogram
       (unfold
        (lambda (p) (= deals  p))
        (lambda ignored (shape (get-hand 0 (new-deck))))
        add1
        0)))))
  ))
