(defpackage :bridge.bidding.1nt-response
  (:use :common-lisp
        :test-stuff
        :bridge.bidding.misc)
  (:import-from :bridge.hand
                :make-hand :hand-value :hand-suit :suit-lengths :by-suits )
  (:import-from :bridge.card
                :card :card-suit :card-rank :card-goren-hcp)
  (:export :make-opening-call))

(in-package :bridge.bidding.1nt-response)

(defun respond-to-1nt-opening (my-hand)
  (let ((hcp (hand-value my-hand :notrump)))
    (case hcp
     ((0 1 2 3 4 5 6 7)
      (let ((sans-clubs (longest-suit-holdings (remove-if #'(lambda (card)
                                                              (eq
                                                               (card-suit
                                                                card)
                                                               :clubs)) my-hand))))
        (if (>= (length (car sans-clubs)) 5)
            (cons 2 (card-suit (caar sans-clubs)))
          :pass)))
     ((8 9)
      '(2 . :notrump))
     ((10 11 12 13 14)
      (let ((lmh (longest-major-holdings my-hand)))
        (cond
         ((>= (length (car lmh))
              6)
          (cons 4 (card-suit (caar lmh))))
         ((= (length (car lmh))
             5)
          (cons 3 (card-suit (caar lmh))))
         (t '(3 . :notrump)))))
     (otherwise (warn "I don't know how to respond to 1 NT with ~S points." 
                      hcp)))))

(defun respond-to-1nt-opening-tests ()
  (macrolet ((expect-response (my-hand expected)
                              `(expect-equal ,expected
                                             (respond-to-1nt-opening
                                              (make-hand ',my-hand)))))
    ;; With 0 to 7 (partscore):
    ;; * Bid 2 spades, 2 hearts, or 2 diamonds with a five-card or longer
    ;; suit...
    (expect-response ( c3 c4 c5         ; 0 + 0
                          d3 d4 d5
                          h4 h5
                          s2 s3 s4 s5 s6)
                     '(2 . :spades))

    ;; ...  (2 clubs is reserved for the Stayman convention)
    (expect-response (c3 c4 c5 c6 c7    ; 0 + 0
                         d3 d4 d5
                         h4 h5
                         s2 s3 s4)
                     :pass)

    ;; * Otherwise, pass.
    (expect-response (c2 c3 c4 c5       ; 0 + 0
                         d2 d3 d4 d5
                         h2 h3 h4 h5
                         s2) :pass)

    ;; * With 8 or 9 points: bid 2 NT.
    (expect-response (c2 c3 c4 ca       ; 8 + 0
                         d2 d3 d4 da
                         h2 h3 h4 h5
                         s2)
                     '(2 . :notrump))

    ;; make sure we don't count distrubution points.
    (expect-response (c2 c3 c4 c8 ck    ; 7 + 1
                         d2 d3 d4 da
                         h2 h3 
                         s2 s3)
                     :pass)

    ;; With 10 to 14 (game);
    ;; * bid 4 spades or 4 hearts with a six-card or longer major
    ;; suit.
    (expect-response (
                      c2 c3
                      d2 d3
                      h2 h3 hk
                      s2 s3 s4 s5 sk sa)
                     '(4 . :spades))

    (expect-response (
                      c2 c3
                      d2 d3
                      h2 h3 h4 h5 hk ha
                      s2 s3 sk)
                     '(4 . :hearts))

    ;; * bid 3 spades or 3 hearts with a five-card major.
    (expect-response (
                      c2 c3
                      d2 d3 d4
                      h3 h4 h5 hk ha
                      s2 s3 sk)
                     '(3 . :hearts))

    ;; * Otherwise, bid 3NT.
    (expect-response (
                      c2 c3
                      d2 d3 d4 d5
                      h4 h5 hk ha
                      s2 s3 sk)
                     '(3 . :notrump))

    (expect-response (
                      c2
                      d2 d3 d4 d5 d6
                      h4 h5 hk ha
                      s2 s3 sk)
                     '(3 . :notrump))

    (expect-response (
                      c2
                      d2 d3 d4 d5 d6 d7
                      h5 hk ha
                      s2 s3 sk)
                     '(3 . :notrump))))

(run-tests "response to 1NT opening"
           respond-to-1nt-opening-tests)