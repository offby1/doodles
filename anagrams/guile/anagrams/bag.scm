(define-module (bag))
(use-modules (srfi srfi-1)
             (srfi srfi-13)
             (primes))

(define bag-type (make-record-type "bag" '(number num-factors)))
(define make-bag-from-numbers (record-constructor bag-type))

(define-public bag-number (record-accessor bag-type 'number))
(define-public bag-size   (record-accessor bag-type 'num-factors))

(define-public (hash-bag b size) (remainder (bag-number b) size))
(define-public (assoc-bag b alist)
  (find (lambda (pair) (= (bag-number b) (bag-number (car pair)))) alist))


(define primes #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101))
(define prime-list (vector->list primes))

(define char->factor
  (let ((a-code (char->integer #\a)))
    (lambda (c)
      (if (char-alphabetic? c)
          (let ((index (- (char->integer (char-downcase c))
                          a-code)))
            (vector-ref primes index))
        1))))

(define-public (bag s)
  "Return an object that describes all the letters in S, without
regard to order."
  (let loop ((chars-to-examine (string-length s))
             (bag-size 0)
             (product 1))
    (if (zero? chars-to-examine)
        (make-bag-from-numbers product bag-size)
      (let ((factor (char->factor (string-ref s (- chars-to-examine 1)))))
        (loop (- chars-to-examine 1)
              (+ bag-size (if (= 1 factor)
                              0
                            1))
              (* product factor))))))

(define-public (subtract-bags b1 b2)
  (if (zero? (bag-size b2))
      (error "Hey!  Don't subtract the empty bag."))
  (let ((quotient (/ (bag-number b1)
                     (bag-number b2))))
    (and (integer? quotient)
         (make-bag-from-numbers 
          quotient 
          (- (bag-size b1)
             (bag-size b2))))))

(define-public (bag-empty? b)
  (zero? (bag-size b)))

(define-public (bag->string b)
  "Returns a string of all the characters in bag B, in some arbitrary order.
This is intended only for debugging, because it's mighty slow."
  (apply string
         (map (lambda (p)
                (integer->char
                 (+ (char->integer #\a)
                    (- (vector-length primes)
                       (length (member p prime-list)))))) 
              (sort (factors (number b)) <))))

(define-public (bags=? b1 b2)
  (= (bag-number b1)
     (bag-number b2)))
