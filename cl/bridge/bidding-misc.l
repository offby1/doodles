(defpackage :bridge.bidding.misc
  (:use :common-lisp
        :test-stuff)
  (:import-from :bridge.hand
                :make-hand :hand-value :hand-suit :suit-lengths :by-suits )
  (:import-from :bridge.card
                :card :card-suit :card-rank :card-goren-hcp)
  (:export :all-longest :balancedp :denomination
           :have-at-least-a-five-card-suit
           :highest-ranking
           :higher-ranking-holdings
           :honor-count
           :longest-suit-holdings
           :longest-major-holdings
           :lowest-ranking
           :level
           :suit-is-minor))

(in-package :bridge.bidding.misc)

(declaim (optimize safety))

(deftype level ()
  `(member 1 2 3 4 5 6 7))

(deftype denomination ()
  `(member :clubs
           :diamonds
           :hearts
           :spades
           :notrump))

(defun onep (n)
  (= n 1))

(defun twop (n)
  (= n 2))

(defun balancedp (h)
  (let* ((lengths       (suit-lengths h))
         (voids         (length (remove-if-not #'zerop lengths)))
         (singletons    (length (remove-if-not #'onep  lengths)))
         (doubletons    (length (remove-if-not #'twop  lengths))))
    (and (zerop voids)
         (zerop singletons)
         (< doubletons 2))))

(defun balancedp-tests ()
  (expect (not (balancedp (make-hand '( ; void
                                       c2 c3 c4 c5
                                       d2 d3 d4 d5
                                       h2 h3 h4 h5 h6)))))

  (expect (not (balancedp (make-hand '( c2 c3 c4 c5 ; singleton
                                           d2 d3 d4 d5
                                           h2 h3 h4 h5
                                           s2)))))
 
  (expect (not (balancedp (make-hand '( c2 c3 c5 c6 ; two doubletons
                                           d2 d3 d4 d5 d7
                                           h2 h3 
                                           s2 s3)))))
 
  (expect (balancedp (make-hand '( c2 c3 c4 c5
                                      d2 d3 d4 
                                      h2 h3 h4 
                                      s2 s3 s4))))

  (expect (balancedp (make-hand '( c2 c3 c4 c5 c7
                                      d2 d3 d4 
                                      h2 h3 h4 
                                      s3 s4)))))

(defun have-at-least-a-five-card-suit (h)
  ;; you could also simply say "if we have more than zero long-suit
  ;; points, we must have at least one five-card suit".
  (not (zerop (length (remove-if #'(lambda (l)
                                     (< l 5)) (suit-lengths h))))))

(defun have-at-least-a-five-card-suit-tests ()
  (expect (not (have-at-least-a-five-card-suit (make-hand '(
                                                            c2 c3 c4 c5
                                                            d2 d3 d4
                                                            h2 h3 h4
                                                            s2 s3 s4)))))

  (expect (have-at-least-a-five-card-suit (make-hand '(
                                                       c2 c3 c4 c5 c6
                                                       d2 d3 d4
                                                       h2 h3 h4
                                                       s3 s4))))

  (expect (have-at-least-a-five-card-suit (make-hand '(
                                                       c2 c3 c4 c5 c6 c7
                                                       d2 d3 d4
                                                       h2 h3 h4
                                                       s4))))

  (expect (have-at-least-a-five-card-suit (make-hand '(
                                                       c2 c3 c4 c5 c6
                                                       d2 d3 d4 d5 d6
                                                       h2 h3 
                                                       s3 )))))

(defun honor-count (hand-or-holdings)
  (length (remove-if-not #'(lambda (r)
                             (<= 10 r))
                         ;; reduce the argument to just the ranks.
                         (if (symbolp (car hand-or-holdings))
                             (mapcar #'card-rank hand-or-holdings)
                           (apply #'append (mapcar #'cdr hand-or-holdings))))))

(defun honor-count-tests ()
  (expect-equal 0 (honor-count '(c2)))

  (expect-equal 0  (honor-count '(
                                  c2 c3 c4 c5 c6
                                  d2 d3 d4 d5 d6
                                  h2 h3 
                                  s3 )))

  (expect-equal 1 (honor-count '( c2 ct)))
 
  (expect-equal 1  (honor-count '(
                                  c2 c3 c4 c5 c6
                                  d2 d3 d4 d5 da
                                  h2 h3 
                                  s3 )))
  (expect-equal 13  (honor-count '(
                                   ca ck cq cj
                                   da dk dq
                                   ha hk hq
                                   sa sk sq
                                   ))))

(defun in-descending-rank-order (suit-holdings)
  (stable-sort (copy-seq suit-holdings)
        #'(lambda (h1 h2)
            ;; it just so happens that the ranking of the suits
            ;; agrees with their alphabetical order
            (string-greaterp (symbol-name (car h1))
                             (symbol-name (car h2))))))

(defun highest-ranking (suit-holdings)
  (car (in-descending-rank-order suit-holdings)))

(defun lowest-ranking (suit-holdings)
  (car (last (in-descending-rank-order suit-holdings))))

(defun ranking-tests ()
  (expect-tree-equal '((s2 s3)(h9 ht ha)) (in-descending-rank-order '((h9 ht ha)(s2 s3))))
  (expect-tree-equal '((s2 s3)(h9 ht ha)) (in-descending-rank-order '((s2 s3)(h9 ht ha))))
  (expect-tree-equal '(s2 s3)    (highest-ranking '((h9 ht ha)(s2 s3))))
  (expect-tree-equal '(h9 ht ha) (lowest-ranking  '((h9 ht ha)(s2 s3)))))

(defun all-longest (seqs)
  "Returns all the elements of SEQS that are as long as the longest of
them.  Preserves the order of equally-long elements."
  ;; There's probably a way to do this with a built-in function such
  ;; as `reduce', but I haven't figured out how.
  (do ((result '())
       (seqs seqs (cdr seqs)))
      ((null seqs) (reverse result))
    (cond
     ((or (null result )
          (< (length (car result))
             (length (car seqs))))
      (setf result (list (car seqs))))
     ((= (length (car result))
         (length (car seqs)))
      (push (car seqs) result)))))

(defun all-longest-tests ()
  (expect-tree-equal '((1 2 3) (4 5 6))
                      (all-longest '(
                                     (1 2)
                                     ()
                                     (1 2 3)
                                     (2 3)
                                     (4 5 6)
                                     (7)
                                     )))

  ;; Make sure it's preserving the order.
  (expect-tree-equal '((4 5 6) (1 2 3))
                      (all-longest '(
                                     (1 2)
                                     ()
                                     (4 5 6)
                                     (2 3)
                                     (1 2 3)
                                     (7)
                                     ))))

(defun longest-suit-holdings (h &key test)
  "Returns a list of \"suit holdings\".  Each is a list of all cards
from the hand of a given suit.  The list includes only those suits
whose lengths are at least as long as the lengths of every other
suit.  The holdings are ordered with the lowest first (i.e., clubs
come before spades)."
  (all-longest (remove-if-not (or test
                              #'identity)
                          (by-suits h))))

(defun suit-is-minor (s)
  (member s '(:clubs :diamonds)))

(defun card-is-minor (c)
  (suit-is-minor (card-suit c)))

(defun suit-holding-is-minor (cards)
   (card-is-minor (car cards)))

(defun longest-minor-holdings (h)
  (longest-suit-holdings h :test #'suit-holding-is-minor))

(defun longest-tests ()
  (expect (suit-holding-is-minor '(c1 c2 c3)))
  (expect (not (suit-holding-is-minor '(h23))))


  (expect-tree-equal '((c2 c3 c4 c5 c6))
                      (longest-suit-holdings (make-hand '(
                                                          c2 c3 c4 c5 c6
                                                          d2 d3 d4 d5
                                                          h2 h3 h4
                                                          sa))))

  (expect-tree-equal '((d2 d3 d4 d5)
                        (h2 h3 h4 h5))
                      (longest-suit-holdings (make-hand '(
                                                          c2 c3 c4 
                                                          d2 d3 d4 d5
                                                          h2 h3 h4 h5
                                                          sq sj))))
  (expect-tree-equal '((c2 c3 c4 c5 c6))
                      (longest-minor-holdings (make-hand '(
                                                           c2 c3 c4 c5 c6
                                                           d2 d3 d4 d5
                                                           h2 h3 h4
                                                           sa))))

  (expect-tree-equal '((d2 d3 d4 d5))
                      (longest-minor-holdings (make-hand '(
                                                           c2 c3 c4 
                                                           d2 d3 d4 d5
                                                           h2 h3 h4 h5
                                                           sq sj)))))
(defun longest-major-holdings (my-hand)
  (longest-suit-holdings
   (remove-if
    #'card-is-minor
    my-hand)))

;; This is sort of silly -- these tests are more complicated than the
;; implementation
(defun higher-ranking-holdings (suit hand)
  (remove-if #'(lambda (holding)
                 (string<= (card-suit (car holding))
                           suit))
             (in-descending-rank-order (by-suits hand))))

(defun higher-ranking-holdings-tests ()
  (let ((hand-sans-voids (make-hand '(c2 d2 h3 sa))))
    (expect-tree-equal '((sa) (h3) (d2))
                        (higher-ranking-holdings :clubs
                                                 hand-sans-voids))
    (expect-tree-equal '((sa) (h3))
                        (higher-ranking-holdings :diamonds
                                                 hand-sans-voids))
    (expect-tree-equal '((sa))
                        (higher-ranking-holdings :hearts
                                                 hand-sans-voids))
    (expect-tree-equal nil
                        (higher-ranking-holdings :spades
                                                 hand-sans-voids))
    ))

(run-tests "bidding-misc"
           balancedp-tests
           higher-ranking-holdings-tests
           have-at-least-a-five-card-suit-tests
           honor-count-tests
           ranking-tests
           all-longest-tests
           longest-tests
           )