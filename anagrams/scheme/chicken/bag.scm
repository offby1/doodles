(declare (unit bag))

(define (increment! char alist)
  (define probe (assq char alist))
  (when (not probe)
    (set! probe (cons char 0))
    (set! alist (cons probe alist)))
  (let ((prev (cdr probe)))
    (set-cdr! probe (+ 1 prev))
    alist))

;; the returned object is a simple alist mapping chars to numbers.
;; It's sorted alphabetically by character.
(define (bag s)
  "Return an object that describes all the letters in S, without
regard to order."
  (let loop ((chars-to-examine (string-length s))
             (result '()))
    (if (zero? chars-to-examine)
        (sort result (lambda (p1 p2) (char<? (car p1) (car p2))))
      (loop (- chars-to-examine 1)
            (increment! (char-downcase (string-ref s (- chars-to-examine 1))) result)))))

(define bag-empty? null?)
(define bags=? equal?)

(define (sb-internal b1 b2 result)
  (cond
   ((bag-empty? b2)
    result)
   ((bag-empty? b1)
    #f)
   (else
    (let ((entry1 (car b1))
          (entry2 (car b2)))
      (cond
       ((char=? (car entry1)
                (car entry2))
        (let ((c (car entry2))
              (diff (- (cdr entry1)
                       (cdr entry2))))
          (cond
           ((negative? diff)
            #f)
           ((zero? diff)
            (sb-internal (cdr b1)
                         (cdr b2)
                         result))
           (else
            (sb-internal (cdr b1)
                         (cdr b2)
                         (cons (cons c diff)
                               result))))))
       ((char>? (car entry1)
                (car entry2))
        #f)
       (else
        (let ((sub (sb-internal (cdr b1)
                                b2
                                result)))
          (and sub
               (cons entry1
                     sub)))
        ))))))

(define (subtract-bags b1 b2)
  (if (bag-empty? b2)
      (error "Hey!  Don't subtract the empty bag."))
  (sb-internal b1 b2 '()))



;;; unit tests

;; Notes about bags in general:

;; creating a bag from a string needn't be all that fast, since we'll
;; probably only do it a few thousand times per application (namely,
;; reading a dictionary of words), whereas subtracting bags needs to
;; be *really* fast, since I suspect we do this O(n!) times where n is
;; the length of the string being anagrammed.

(assert (bag-empty? (bag "")))
(assert (not (bag-empty? (bag "a"))))
(assert (bags=? (bag "abc")
                (bag "cba")))

(assert (not (bags=? (bag "abc")
                     (bag "bc"))))

(assert (bags=? (bag "a")
                (subtract-bags (bag "ab")
                               (bag "b"))))

(assert (not (subtract-bags (bag "a")
                            (bag "b"))))
(assert (not (subtract-bags (bag "a")
                            (bag "aa"))))

(let ((empty-bag (subtract-bags (bag "a")
                                (bag "a"))))
  (assert (bag-empty? empty-bag))
  (assert empty-bag))

(display  "bag tests passed.")
(newline)
