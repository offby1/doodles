(defpackage :bridge.hand
  (:use :common-lisp  :test-stuff)
  (:import-from  :bridge.card
                 :card :card-suit :card-rank :card-goren-hcp)
  (:export :make-hand :hand-value :hand-suit :suit-lengths :by-suits
           :nclubs :ndiamonds :nhearts :nspades))

(in-package :bridge.hand)

(defun make-hand (card-sym-list)
  "Simply returns a list of its arguments, but first asserts that
they're all cards, that there are no more than 13 of them, and that
there are no duplicates."
  (assert (<= (length card-sym-list) 13))
  (assert (and (= (length card-sym-list)
                  (length (remove-duplicates card-sym-list)))
               "No duplicate arguments allowed!"))
  (assert (dolist (one-symbol card-sym-list t)
            (when (not (typep one-symbol 'card))
              (format t "~A ain't no card~&" one-symbol)
              (return nil))))
  card-sym-list)

(defun hand-suit (h s)
  "Returns the cards in H that are of suit S."
   (remove-if-not #'(lambda (c)
                      (eql s (card-suit c))) h))

(defun by-suits (h)
  "Returns four lists -- one for each suit.  Each list contains those
cards in H that are of that suit.  The lists are ordered clubs,
diamonds, hearts, then spades."
  (mapcar #'(lambda (s)
              (hand-suit h s))
          (list :clubs :diamonds :hearts :spades)))

(defun nclubs (h)
  (length (nth 0 (by-suits h))))

(defun ndiamonds (h)
  (length (nth 1 (by-suits h))))

(defun nhearts (h)
  (length (nth 2 (by-suits h))))

(defun nspades (h)
  (length (nth 3 (by-suits h))))

(defun by-suits-tests ()
  (expect-equal (by-suits (make-hand '( c2)))
                '((c2)
                  ()
                  ()
                  ()))

  (expect-equal (by-suits (make-hand '( h4
                                        d2 d3 d4 d5 d6 d7
                                        c2 c3 c4 c5 c6
                                        h5)))
                '((c2 c3 c4 c5 c6)
                  (d2 d3 d4 d5 d6 d7)
                  (h4 h5)
                  ()))
  (let ((h (make-hand '( s3 s4 s2
                         d2 d3 d4 d5 d6
                         c2 c3 c4 c5 
                         h5))))
    (expect-equal 4 (nclubs h))
    (expect-equal 5 (ndiamonds h))
    (expect-equal 3 (nspades h))
    (expect-equal 1 (nhearts h))))

(defun suit-lengths (h)
  "Returns a list of four numbers, each of which is the length of
some suit.  Useful for judging the 'balancedness' of a hand."
  (mapcar #'length
          (by-suits h)))

(defun long-suit-points (h)
  (apply #'+ (mapcar
              #'(lambda (suit-length)
                  (max (- suit-length 4) 0))
              (suit-lengths h)))  )

(defun short-suit-points (h)
  (apply #'+ (mapcar
              #'(lambda (suit-length)
                  ;; Note that we'll count shortness in the trump suit
                  ;; just like any other.  So if partner bid spades, and
                  ;; our spade suit has one card, we'll count that as
                  ;; three points.  I'm hoping this won't be a problem;
                  ;; I'm hoping the bidding logic will be smart enough to
                  ;; Not Do That.
                  (case suit-length
                    ((0) 5)
                    ((1) 3)
                    ((2) 1)
                    (t 0)))
              (suit-lengths h))))

(defun high-card-points (h)
  (apply #'+ (mapcar #'card-goren-hcp h)))

(defun hand-value (h &optional trump-suit)
  "Returns the point value of H for bidding.  Considers distribution
if TRUMP-SUIT is not :notrump -- if TRUMP-SUIT is nil, counts length
as you would for opening (i.e., each card beyond the fourth in a suit
counts as an extra point); if TRUMP-SUIT names a suit, counts for
dummy points (i.e., each short suit gets extra points).)"
  (+  (high-card-points h)
      (cond
       ((eql trump-suit :notrump)
        0)
       ((null trump-suit)
        (long-suit-points h))
       (t
        (short-suit-points h)))))


(defun hand-value-tests ()
  (expect-error (make-hand '( c2 c2)))  ;duplicate
  (expect-error (make-hand '( c2 c3 c8 ca
                                 d6 d7
                                 h8 ht hq hk
                                 s5 s8 s9 st))) ; > 13 cards
  (expect-error (make-hand '( 1 2 3 4))) ; contains non-card things
  (flet ((expect-hand-value (v h &optional trump-suit)
                            (expect-equal v (hand-value h
                                                        trump-suit))))
    (let ((weak-hand (make-hand '(
                                  c2 c3 c8 ca
                                  d6 d7
                                  h8 ht hq hk
                                  s5 s8 s9)))
          (opening-count-hand (make-hand '(
                                           c2 c3 c8 ca
                                           d6 d7 da
                                           ht hq hk
                                           s5 s8 sq)))
          (unbalanced-hand (make-hand '(
                                        c2 c3 c4 c5 c6
                                        d2 d3 d4 d5 d6 d7
                                        h4 h5
                                        ))))
   
      (expect-tree-equal (suit-lengths weak-hand) '(4 2 4 3))
      (expect-tree-equal (suit-lengths unbalanced-hand) '(5 6 2 0))

      (expect-hand-value  9 weak-hand)
      (expect-hand-value  9 weak-hand :notrump)
      (expect-hand-value  10 weak-hand :hearts)
      (expect-hand-value  15 opening-count-hand)
      (expect-hand-value  15 opening-count-hand :notrump)
      (expect-hand-value  15 opening-count-hand :hearts)
      (expect-hand-value  3 unbalanced-hand)
      (expect-hand-value  0 unbalanced-hand :notrump)
      (expect-hand-value  6 unbalanced-hand :hearts)

      (expect-hand-value 14 (make-hand '(
                                         c2 c3 c8 ca
                                         h4 h8 ht hq hk
                                         s5 s8 s9 st)) :clubs)
      (expect-hand-value 12 (make-hand '(
                                         c2 c3 c8 ca
                                         d2
                                         h8 ht hq hk
                                         s5 s8 s9 st)) :clubs)

      ;; Note that we count shortness in the trump suit just the same way
      ;; we'd count shortness in any other suit.  This is insane, but
      ;; hopefully won't be a problem in practice.  If it were to become
      ;; a problem, I could simply have HAND-VALUE skip the named suit
      ;; when calculating short-suit points.
      (expect-hand-value  12 (make-hand '(
                                          c2 c3 c8 ca
                                          d2
                                          h8 ht hq hk
                                          s5 s8 s9 st)) :diamonds)

      (expect-hand-value  10 (make-hand '(
                                          c3 c8 ca
                                          d2 d3
                                          h8 ht hq hk
                                          s5 s8 s9 st)) :clubs)

      (expect-hand-value  19 (make-hand '(
                                          c2 c3 c4 c5 c6 c7 c8 c9 ct cj cq
                                          ck ca)))

      (expect-hand-value  10 (make-hand '(
                                          c2 c3 c4 c5 c6 c7 c8 c9 ct cj cq
                                          ck ca)) :notrump)

      (expect-hand-value  25 (make-hand '(
                                          c2 c3 c4 c5 c6 c7 c8 c9 ct cj cq
                                          ck ca)) :spades)

      )))

(run-tests "hand"
  by-suits-tests
  hand-value-tests)
