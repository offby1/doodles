(defpackage :bridge.bidding.1-suit-response
  (:use :common-lisp
        :test-stuff
        :bridge.bidding.misc)
  (:import-from :bridge.hand
                 :make-hand
                 :hand-suit
                 :hand-value
                 :nclubs :ndiamonds :nhearts :nspades)
  (:import-from :bridge.card
                :card-suit
                :suit<)
  (:import-from :bridge.dealer :example-hand)
  (:export :respond-to-1-of-a-suit))

(in-package :bridge.bidding.1-suit-response)

(defun respond-to-1-of-a-suit (partners-bid my-hand)
  (let* ((the-suit (cdr partners-bid))
         (good-fit (<= 8 (+ (length (hand-suit my-hand the-suit))
                            (if (suit-is-minor the-suit)
                                3
                              5))))

         (value (hand-value my-hand (and ; use dummy points only if
                                        ; we're about to raise a major
                                        ; suit.
                                     good-fit
                                     (not (suit-is-minor the-suit))
                                     the-suit))))
    
    (case value
      ((0 1 2 3 4 5)
       (return-from respond-to-1-of-a-suit :pass))
      ((6 7 8 9 10)
       (cond
        (good-fit
      
         (cons 2 the-suit))
     
        ((and (eq the-suit :hearts)
              (<= 4 (length (hand-suit my-hand :spades))))
         (cons 1 :spades))
        (t
         (let ((higher-ranking-holdings-with-at-least-four-cards
                (remove-if-not #'(lambda (holding)
                                   (<= 4 (length holding)))
                               (higher-ranking-holdings the-suit my-hand))))
           (if higher-ranking-holdings-with-at-least-four-cards
               (cons 1 (card-suit (caar (last higher-ranking-holdings-with-at-least-four-cards))))
             '(1 . :notrump))))))
      ((11 12)
       (forcing-response-to-1-of-a-suit partners-bid my-hand))
      (otherwise
       (warn "I don't know how to respond to one of a suit with ~S
points." value)))

    ))

(defun invitational-response-to-1-of-a-suit-tests ()
  (flet ((expect-response (partners-bid my-hand expected-response)
                          (expect-equal expected-response
                                        (respond-to-1-of-a-suit
                                         partners-bid
                                         (make-hand my-hand)))))
    
    ;; With 6 to 10 points responding to a major suit:

    ;; * raise to the two level with three-card support.
    (expect-response '(1 . :hearts)
                     '(c2 c3 cq
                          d2 d3 dq
                          h2 h3 hq
                          s2 s3 sq s5)
                     '(2 . :hearts))

    (expect-response '(1 . :spades)
                     '(c2 c3 cq
                          d2 d3 dq
                          h2 h3 hq
                          s2 s3 sq s5)
                     '(2 . :spades))

    ;; here's 9 points if you count dummy points, but only 5 if you
    ;; (mistakenly) count only length.
    (expect-response '(1 . :spades)
                     '(
                       d2 d3 d4 d5 d6
                       h3 h4 h5 h6
                       s2 s3 s4 sa)
                     '(2 . :spades))
    
    ;; The inverse of the above -- counting dummy points would be
    ;; wrong, because we have no trump support.
    (expect-response '(1 . :hearts)
                     '(
                       c3 c4 c5 c6
                       d2 d3 d4 d5 d6
                       s2 s3 s4 sa)
                     :pass)
    
    ;; Don't count dummy points at all if it's a minor suit.
    (expect-response '(1 . :diamonds)
                     '(
                       d2 d3 d4 d5 d6
                       h8 h9 ht hj
                       s9 st sj sq)
                     :pass)

    ;; lacking three-card support, bid a new suit at the one level.
    ;; This only applies if the opening bid was hearts, and we're
    ;; considering responding with spades.  Note that Audrey doesn't
    ;; say how many spades you need, but the Standard American Yellow
    ;; Card (http://www.annam.co.uk/sayc01.htm#1m) says you gotta have
    ;; 4.
    (expect-response '(1 . :hearts)
                     '(c2 c3 cq
                          d2 d3 d4 dq
                          h3 hq
                          s2 s3 sq s5)
                     '(1 . :spades))

    ;; lacking a suit that you can bid at the one-level, bid 1
    ;; notrump.
    (expect-response '(1 . :spades)
                     '(c2 c3 c6 c8 cq
                          d2 d3 d4 dq
                          h3 hq
                          sq s5)
                     '(1 . :notrump))

    (expect-response '(1 . :hearts)
                     '(c2 c3 c6 cq
                          d2 d3 d4 dq
                          h3 hq
                          s3 sq s5)
                     '(1 . :notrump))

    ;; With 6 to 10 points, responding to one of a minor suit:

    ;; * Raise to two with five-card support.
    
    (expect-response '(1 . :diamonds)
                     '(c2 cq
                          d2 d3 d4 d5 dq
                          h3 hq
                          s2 s3 sq s5)
                     '(2 . :diamonds))

    (expect-response '(1 . :clubs)
                     '(c2 c3 c4 c5 cq
                          d2 dq
                          h3 hq
                          s2 s3 sq s5)
                     '(2 . :clubs))

    ;; * Lacking five-card support, bid a new suit at the one level.
    (expect-response '(1 . :clubs)
                     '(c2 c3 c4
                          d2 d3 d4 dq
                          h3 hq
                          s2 s3 sq s5)
                     '(1 . :diamonds))
    ))


;; Now we're getting ugly :-\
(defun forcing-response-to-1-of-a-suit (partners-bid my-hand)
  (let ((partners-suit (cdr partners-bid))
        (my-longest-suit (card-suit (caar (longest-suit-holdings my-hand)))))
    (flet ((minimum-level (s)
                          (if (suit< s partners-suit)
                              2
                            1)))
      (if (suit-is-minor partners-suit)
          (let* ((cheapest-four-card-major-holding (car (remove-if-not  #'(lambda (h)
                                                                            (<= 4 (length h)))
                                                  
                                                                        (longest-major-holdings my-hand))))
                 (cfcm (and cheapest-four-card-major-holding
                            (card-suit (car cheapest-four-card-major-holding)))))
            (cond
             (cfcm
              (cons (minimum-level cfcm) cfcm))
             ((<= 5 (length (hand-suit my-hand partners-suit)))
              (cons 3 partners-suit))
             (t
              (let* ((longest-holdings (longest-suit-holdings
                                        my-hand))
                     (last-resort-suit (card-suit (caar longest-holdings))))
                (cons (minimum-level last-resort-suit)
                      last-resort-suit)))))
        
        (if (<= 3 (length (hand-suit my-hand partners-suit)))
            (cons 3 partners-suit)
          (cons
           (minimum-level my-longest-suit)
           my-longest-suit))))))

(defun forcing-response-to-1-of-a-suit-tests ()
  (trace forcing-response-to-1-of-a-suit)
  (macrolet ((expect-response (partners-bid
                               my-hand-criterion
                               expected-response)
                              `(spew-trace-on-failure
                                  (expect-equal ',expected-response
                                                (forcing-response-to-1-of-a-suit
                                                 ',partners-bid
                                                 (example-hand
                                                  #'(lambda (h) ,my-hand-criterion)))))))

    ;; With 11 or 12 points, responding to a major suit:

    ;; * "limit" raise to the three level with three-card or better
    ;; support
    
    (expect-response (1 . :hearts)
                     (and (<= 11 (hand-value h :hearts) 12)
                          (=   3 (nhearts h))
                          (=   4 (nspades h)))
                     (3 . :hearts))
    
    (expect-response (1 . :spades)
                     (and (<= 11 (hand-value h :spades) 12)
                          (=   3 (nhearts h))
                          (=   4 (nspades h)))
                     (3 . :spades))

    ;; * bid a new suit (at the two level)
    (expect-response (1 . :hearts)
                     (and (<= 11 (hand-value h) 12)
                          (< (nhearts h) 3)
                          (= 3
                             (ndiamonds h)
                             (nclubs h))
                          (<= 4 (nspades h)))
                     (1 . :spades))
    
    (expect-response (1 . :spades)
                     (and (<= 11 (hand-value h) 12)
                          (< (nspades h) 3)
                          (< (max (nspades h)
                                  (nhearts h)
                                  (ndiamonds h))
                             (nclubs h)))
                     (2 . :clubs))

    ;; With 11 or 12 points, responding to a minor suit:

    ;; * "limit" raise to the three level with five-card or better
    ;; support

    (expect-response (1 . :diamonds)
                     (and (<= 11 (hand-value h :diamonds) 12)
                          (> 4 (nspades h))
                          (> 4 (nhearts h))
                          (<= 5 (ndiamonds h)))
                     (3 . :diamonds))

    ;; * bid a new suit, preferring a major suit
    (expect-response (1 . :clubs)
                     (and (<= 11 (hand-value h) 12)
                          (= (nclubs h) 4)
                          (<= 4 (nspades h))
                          (< (max (nclubs h)
                                  (ndiamonds h)
                                  (nhearts h))
                             (nspades h)))
                     (1 . :spades))
      
    (expect-response (1 . :clubs)
                     (and (= 11 (hand-value h))
                          (= 3
                             (nclubs h)
                             (nhearts h)
                             (nspades h))
                          (= 4 (ndiamonds h)))
                     (1 . :diamonds)))
  
  (untrace forcing-response-to-1-of-a-suit))



(defun respond-to-1-of-a-suit-tests ()
  (macrolet ((expect-response (partners-bid
                               my-hand
                               expected-response)
                              `(expect-equal ,expected-response
                                             (respond-to-1-of-a-suit
                                              ,partners-bid
                                              (make-hand ',my-hand)))))

    (expect-response '(1 . :hearts)
                     (c2 c3 ct
                         d2 d3 dq
                         h2 h3 hq
                         s2 s3 sj s5)
                     :pass)



    ))

(run-tests "response to one-of-a-suit opening"
           respond-to-1-of-a-suit-tests
           forcing-response-to-1-of-a-suit-tests
           invitational-response-to-1-of-a-suit-tests
)