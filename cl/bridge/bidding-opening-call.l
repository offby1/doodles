(defpackage :bridge.bidding.opening-call
  (:use :common-lisp
        :test-stuff
        :bridge.bidding.misc)
  (:import-from :bridge.hand
                :make-hand :hand-value :hand-suit :suit-lengths :by-suits )
  (:import-from :bridge.card
                :card :card-suit :card-rank :card-goren-hcp)
  (:export :make-opening-call))

(in-package :bridge.bidding.opening-call)

(defun make-opening-call (h)

  (assert (= 13 (length h)))

  ;; get notrump out of the way first.
  
  ;; Note that we may well end up calling hand-value twice -- once to
  ;; count points for notrump, and once to count points for a suit
  ;; bid.

  (if (and (<= 15 (hand-value h :notrump) 17)
           (balancedp h))
      (return-from make-opening-call (cons 1 :notrump)))

  (let ((v (hand-value h)))
    (cond
     ((< v 13)
      (let ((lsh (car (longest-suit-holdings h))))
        (if (and (<= 7 (length lsh))    ; note we needn't check any
                                        ; but the first list from
                                        ; (longest-suit-holdings h),
                                        ; since we're sure to never
                                        ; have more than one
                                        ; seven-card suit.
                 (<= 3 (honor-count lsh)))
            (cons 3 (card-suit (car lsh)))
          :pass)))
     ((< v 22)
      
      (cons 1
            (let ((bs (by-suits h)))
              (if (have-at-least-a-five-card-suit h)

                  ;; highest-ranking of two long suits
                  (card-suit (caar (reverse (all-longest bs))))

                (let ((longest-minor-suit-holdings (all-longest (list (nth 0 bs)
                                                                      (nth 1 bs)))))
                  (card-suit
                   (if (= 1 (length longest-minor-suit-holdings))
                       (caar longest-minor-suit-holdings)
                     (car (funcall (if (= 4 (length (car longest-minor-suit-holdings)))
                                       #'highest-ranking 
                                     #'lowest-ranking)
                                   longest-minor-suit-holdings)))))))))
     
     ((balancedp h)
      (cond
       ((< v 25)
        (cons 2 :notrump))
       ((< v 28)
        (cons 3 :notrump))
       (t
        ;; I don't know what to do here, but it doesn't come up very
        ;; often -- I've dealt something over 20,000 hands without
        ;; seeing this.
        (format t "~&Gack! Audrey doesn't say what to do with a balanced 28-point hand.~&"))))
     (t
      ;; Here we've got at least 22 points, and an unbalanced hand.
      ;; Audrey simply says "bid two of your longest suit".  However,
      ;; she says nothing about how to break a tie between two
      ;; equally-long suits.  So we just pick whichever one that
      ;; appears first in the return from longest-suit-holdings (i.e.,
      ;; the lowest-ranking).
      (cons 2 (card-suit (caar (longest-suit-holdings h))))))))


(defun make-opening-call-tests ()
  (expect (typep :clubs   'denomination))
  (expect (typep :notrump 'denomination))
  (expect (not (typep :frodo 'denomination)))

  (expect (typep 2 'level))
  (expect (not (typep 0 'level)))
  (expect (not (typep 8 'level)))

  (expect-error (make-opening-call (make-hand '( c2)))) ; short suit

  (macrolet ((expect-opening-call (expected hand)
                                  `(expect-equal ,expected (make-opening-call
                                                            (make-hand ',hand)))))

    (expect-opening-call  :pass
                          (c3 c4 c5 c6  ; 0 + 0
                              d7 d8 d9
                              h2 h3 h4
                              s2 s3 s4))

    (expect-opening-call  :pass
                          (           ; 12 + 0 -- smallest un-openable
                           c3 c4 c5 ct
                           d7 d8 dq
                           ht hj hq
                           s2 sk sa))
 
 
    (expect-opening-call '(1 . :notrump)
                         (              ; 16 + 0 points
                          c2 c4 ck ca
                          d8 dk da
                          h2 h3 h4
                          s8 s9 sq))

    (expect-opening-call  '(1 . :notrump)
                          (           ; 16 + 1 points.  Audrey prefers
                                        ; notrump to a major suit.
                           cq ca
                           d2 d3 dq
                           h2 h3 h4 hj hk
                           s2 sj sk))
 
    ;; a balanced hand where we'd have had 16 points if we accidentally
    ;; counted length.
    (expect-opening-call '(1 . :notrump)
                         (              ; 15 + 1 points
                          c2 c3 c4 ck ca
                          d8 dk da
                          h2 h3 h4
                          s9 sq))


    ;; With a five-card suit or longer, bid your longest suit.
    (expect-opening-call '(1 . :spades)
                         (
                          ct c9         ; 12 + 1 points
                          dt d9
                          hq ht h9 h8
                          sa sk sq sj st))
   
    ;; With a five-card suit or longer, bid your longest suit.
    (expect-opening-call '(1 . :diamonds)
                         (
                          ct c9         ; 12 + 1 points
                          da dk dq dj dt
                          hq ht h9 h8
                          st s9))

    ;; With a five-card suit or longer, bid your longest suit.
    (expect-opening-call '(1 . :diamonds)
                         (
                          ct            ; 12 + 3 points
                          da dk dq dj dt d9
                          hq ht h9 h8 h7
                          st))

    ;; With a five-card suit or longer, bid your longest suit.
    (expect-opening-call  '(1 . :hearts)
                          (             ; 13 + 1 points
                           c2 cq ck
                           d2 d3
                           h2 h3 h4 hj hk
                           s2 sj sk))

    ;; highest-ranking of two five-card or six-card suits
    (expect-opening-call '(1 . :hearts)
                         (
                          ct c9         ; 12 + 2 points
                          da dk dq dj dt
                          hq ht h9 h8 h7
                          st))
   
    ;; highest-ranking of two five-card or six-card suits
    (expect-opening-call '(1 . :spades)
                         (
                          ct c9         ; 12 + 2 points
                          dt
                          hq ht h9 h8 h7
                          sa sk sq sj st
                          ))
   
    ;; highest-ranking of two five-card or six-card suits
    (expect-opening-call '(1 . :hearts)
                         (
                          ct            ; 12 + 3 points
                          da dk dq dj dt
                          hq ht h9 h8 h7 h2
                          st))
   
    ;; with no five-card suit or longer, bid your longer minor suit.
    (expect-opening-call  '(1 . :diamonds)
                          (             ; 13 + 0 points
                           c2 cq ck
                           d2 d3 d4 d5
                           h2 hj hk
                           s2 sj sk))

    ;; with no five-card suit or longer, bid your longer minor suit.
    (expect-opening-call '(1 . :diamonds)
                         (              ; 13 + 0 points
                          ct cj
                          dt d9 d8 d7
                          hq ht h9
                          sa sk sq sj))

    ;; Bid the higher-ranking of two four-card minor suits ...

    (expect-opening-call '(1 . :diamonds)
                         (              ; 13 + 0 points
                          ck cq ct cj
                          da dt d9 d8
                          ht h9
                          sq sj st))

    ;; ... or the lower-ranking of two three-card minor suits.
    (expect-opening-call '(1 . :clubs)
                         (              ; 13 + 0 points
                          ck cq  cj
                          da dt d9
                          ht h9 h8
                          sq sj st s8))
   
    ;; With an unbalanced hand of 22 or more points, open the bidding
    ;; at the two level in your longest suit.
    (expect-opening-call '(2 . :clubs)
                         (ca ck cq cj ct
                             da dk dq d2
                             ha hk  
                             sa sk  ))

    (expect-opening-call '(2 . :hearts)
                         (ca ck 
                             da dk dq dj
                             ha hk hq hj ht
                             sa sk  ))

    ;; With a balanced hand of 22 to 24 points, open 2 NT.
   
    (expect-opening-call '(2 . :notrump)
                         (ca cq ct
                             da dq dt
                             ha hq ht
                             sa sj st s9))

    ;; With a balanced hand of 25 to 27 points, open 3 NT.
    (expect-opening-call '(3 . :notrump)
                         (ca cq ck
                             da dq dt
                             ha hq ht
                             sa sj st s9))

    ;; With a long suit -- usually seven or more cards with three of
    ;; the top five honors in the suit, and less than the point-count
    ;; values for an opening bid, bid three of that long suit.
    (expect-opening-call '(3 . :hearts) ; 6 + 3
                         (st s9
                             hk hq hj ht h9 h8 h7
                             dt d9
                             ct c9))))

(run-tests "opening call"
           make-opening-call-tests
           )
