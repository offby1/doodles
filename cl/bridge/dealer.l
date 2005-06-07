(defpackage :bridge.dealer
  (:use :common-lisp)
  (:import-from  :bridge.card
                 :card :card-suit :card-rank)
  (:use :bridge.hand)
  (:use :bridge.bidding.misc)
  (:import-from :bridge.bidding.opening-call
                :make-opening-call)
  (:export :example-hand :h))

(in-package :bridge.dealer)

(defconstant deck
  '(c2 c3 c4 c5 c6 c7 c8 c9 ct cj cq ck ca
       d2 d3 d4 d5 d6 d7 d8 d9 dt dj dq dk da
       h2 h3 h4 h5 h6 h7 h8 h9 ht hj hq hk ha
       s2 s3 s4 s5 s6 s7 s8 s9 st sj sq sk sa))

(defun shuffle-list (l)
  "Returns a list with the same elements as L, but in a random order."
  (mapcar
   #'car
   (sort
    (mapcar
     #'(lambda (entry)
         (cons entry (random most-positive-fixnum)))
     l)
    #'(lambda (entry1 entry2)
        (< (cdr entry1)
           (cdr entry2))))))
    
(defun sort-cards (cards)
  "Returns a list with the same elements as CARDS, but in an order
that's convenient for display -- minor suits first, lower ranks
first."
  (sort (copy-seq cards)                ; copy-seq because sort is
                                        ; destructive
        #'(lambda (c1 c2)
            (let ((s1 (card-suit c1))
                  (s2 (card-suit c2)))
              (cond 
               ((string< s1 s2)
                t)
               ((string= s1 s2)
                (< (card-rank c1)
                   (card-rank c2)))
               (t nil))))))

(defun random-deal ()
  (let ((cards (shuffle-list deck)))
    (values (sort-cards (subseq cards  0 13))
            (sort-cards (subseq cards 13 26))
            (sort-cards (subseq cards 26 39))
            (sort-cards (subseq cards 39 52)))))

(if t
    (let ((deals 10000)
          (outfile-name "/tmp/x"))
      (format t "Generating ~A random deals in ~A..." deals outfile-name)
      (finish-output)
      (with-open-file (x outfile-name :direction :output)
                      (time (dotimes (i deals)
                              (let ((d (random-deal)))
                                (format x "~&~A => ~A~&" d (make-opening-call d))))))
      (format t "done.~%")))

(defun example-hand (criterion)
  "Returns a hand that satisfies the CRITERION, which is an expression
in which BRIDGE.DEALER:H is free, and represents the hand.
Second value returned is the number of randomly-chosen hands we needed
to examine in order to find the hand we returned."
  (let ((candidates-examined 0))
    (loop
     (let ((candidate (random-deal)))
       (when (funcall criterion candidate)
         (return (values candidate
                         (1+ candidates-examined))))
       (incf candidates-examined)))))

;; strictly to save typing
(defmacro f (stuff)
  `#'(lambda (h) ,stuff))

#+cmulisp
(setf ext:*gc-verbose* nil)

(dolist (p
         `(("Here's a hand with exactly one void"
            ,(f (= 1 (count-if #'null (by-suits h)))))

           ;; pretty slow!
           ;;("Two voids" ,(f (= 2 (count-if #'null (by-suits h)))))

           ("Five spades"
            ,(f (= 5 (length (nth 3 (by-suits h))))))

           ,(let ((length 6))
              (list
               (format nil "~A ~R-card or longer suit" (if (< 13 (* 2 length))
                                                          "A(n)"
                                                        "At least one")
                                                      length)
               (f (not (zerop (count-if #'(lambda (holding)
                                            (<= length (length holding)))
                                        (by-suits h)))))))
           
           ("Weak 3 opening, according to Audrey"
            ,(f (not (zerop (count-if #'(lambda (holding)
                                          (and (<= 7 (length holding))
                                               (<= 3
                                                   (honor-count holding))))
                                      (by-suits h))))))
           ("Totally boring -- flat, 8 to 11 points"
            ,(f (and (<= 8 (hand-value h) 11) (balancedp h))))
          
           ("Coca-Cola, aka Yarborough -- no aces or face cards"
            ,(f (zerop (hand-value h :notrump))))

           ("Ideal one-notrump opener (no five-card suit)"
            ,(f (and (zerop (count-if #'(lambda (l) (<= 5 l)) (suit-lengths h)))
                     (<= 16 (hand-value h :notrump) 18))))
                                                           
           ,(let ((n 5))
              (list
               (format nil "At least ~R distribution points"
                       n)
              
               (f (<= n (- (hand-value h nil)
                           (hand-value h :notrump))))))
           ))
  (apply #'format (append
                   (list t "~A: ~A (~A tries) ~&"
                         (car p))
                   (multiple-value-list (example-hand (nth 1 p))))))

(defparameter *runs* 10000)

(defun how-frequent (criterion)
  (let ((candidates-to-examine *runs*)
        (matching-hands-found 0))
    (loop
     (let ((candidate (random-deal)))
       (when (zerop candidates-to-examine)
         (return (/ matching-hands-found *runs*)))
       (when (funcall criterion candidate)
         (incf matching-hands-found))
       (decf candidates-to-examine)))))

(let*  ((*runs* 100)
        (range `(16 . 18))
        (notrump-criteria #'(lambda (h) (and (< (doubletons h) 2)
                                             (zerop (voids h))
                                             (zerop (singletons h))
                                             (<= (car range) 
                                                 (hand-value h :notrump) 
                                                 (cdr range))))))
    
  (format t "Here's an example ~A-~A notrump hand: ~A ... " (car range)
          (cdr range)
                                                            (example-hand notrump-criteria))
  (force-output)
  (format t "they come up about ~A%~&" (* 100.0 (how-frequent notrump-criteria))))
