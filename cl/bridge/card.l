(defpackage :bridge.card
  (:use :common-lisp  :test-stuff)
  (:export :card :card-suit :card-rank :card-goren-hcp :suit<))

(in-package :bridge.card)

(defun right-symbol (thing)
  (member (symbol-name thing)
          '("C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9" "CT" "CJ" "CQ" "CK" "CA"
            "D2" "D3" "D4" "D5" "D6" "D7" "D8" "D9" "DT" "DJ" "DQ" "DK" "DA"
            "H2" "H3" "H4" "H5" "H6" "H7" "H8" "H9" "HT" "HJ" "HQ" "HK" "HA"
            "S2" "S3" "S4" "S5" "S6" "S7" "S8" "S9" "ST" "SJ" "SQ" "SK" "SA")
          :test #'string=))

(deftype card ()
  `(satisfies right-symbol))

(defun card-suit (c)
  (ecase (char-downcase (aref (symbol-name c) 0))
    ((#\c) :clubs   )
    ((#\d) :diamonds)
    ((#\h) :hearts  ) 
    ((#\s) :spades  )))

(defun suit< (s1 s2)
  (string< s1 s2))

(defun card-rank (c)
  (let ((r-char (char-downcase (aref (symbol-name c) 1))))
    (ecase r-char
      ((#\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (- (char-int r-char)
                                            (char-int #\0)))
      ((#\t) 10)
      ((#\j) 11)
      ((#\q) 12)
      ((#\k) 13)
      ((#\a) 14))))

(defun card-goren-hcp (c)
  (max 0 (- (card-rank c) 10)))

(defun card-tests ()
  (expect (typep 'c3 'card))

  ;; I haven't decided if it's bad to let cards be symbols whose length
  ;; exceeds two.
  (expect (not (typep 'sally-jesse-raphael 'card)))
  (expect (eql 14        (card-rank 'sally-jesse-raphael)))
  (expect (eql :spades   (card-suit 'sally-jesse-raphael)))

  (expect (not (typep 'c3bob 'card)))
  (expect (eql 3         (card-rank 'c3bob)))
 
  (expect (eql 3         (card-rank 'c3)))
  (expect (eql :clubs    (card-suit 'c3)))
  (expect (eql 10        (card-rank 'dt)))
  (expect (eql 11        (card-rank 'hj)))
  (expect (eql 14        (card-rank 'sa)))
 
  (expect (eql 0         (card-goren-hcp 'c2)))
  (expect (eql 0         (card-goren-hcp 'dt)))
  (expect (eql 2         (card-goren-hcp 'hq)))
  (expect (eql 4         (card-goren-hcp 'sa)))

  (expect (suit< :clubs :spades))
  (expect (not (suit< :spades :hearts))))

(run-tests "card"
 card-tests)
