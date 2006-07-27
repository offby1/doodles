(module call mzscheme
  (require (planet "test.ss"    ("schematics" "schemeunit.plt" 1)))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1)))
  (require "misc.ss")

  (provide call->string
           bid?
           bid-level
           bid-denomination
           make-bid
           calls-equal?
           bid>
           *denominations*)

  (define *denominations* `(clubs diamonds hearts spades notrump))
  (define-struct bid (level denomination))
  (define (bid->int b)
    (+ (* (sub1 (bid-level b)) (length *denominations*))
       (index (bid-denomination b) *denominations*)))
  (define (bid->string b)
    (format "~A ~A" (bid-level b)
            (bid-denomination b)))
  (define (call->string c)
    (if (bid? c)
        (bid->string c)
      (format "~A" c)))
    
  (define (calls-equal? c1 c2)
    (if (bid? c1)
        (and (bid? c2)
             (= (bid-level c1)
                (bid-level c2))
             (eq? (bid-denomination c1)
                  (bid-denomination c2)))
      (and (not (bid? c2))
           (eq? c1 c2))))

  (define (bid< b1 b2)
    (< (bid->int b1)
       (bid->int b2)))

  (define (bid> b1 b2)
    (and (not (calls-equal? b1 b2))
         (not (bid< b1 b2))))
  
  (test/text-ui
   (make-test-suite
    "Evahthang"
    (make-test-case
     "uh ..."
     (let ((b1 (make-bid 1 'notrump))
           (b2 (make-bid 1 'notrump))
           (b3 (make-bid 2 'notrump))
           (p1 'pass)
           (p2 'pass)
           (d  'double))
       (assert-true (calls-equal? b1 b2))
       (assert-false (calls-equal? b2 b3))
       (assert-false (calls-equal? b1 b3))
       (assert-false (calls-equal? b1 p1))
       (assert-true  (calls-equal? p1 p2)))
     )
    (make-test-case
     ">"
     (begin
       (assert-true (bid< (make-bid 1 'spades)
                          (make-bid 1 'notrump)))
       (assert-false (bid< (make-bid 1 'notrump)
                           (make-bid 1 'notrump)))
       (assert-true (bid> (make-bid 1 'notrump)
                          (make-bid 1 'spades)))
       (assert-false (bid> (make-bid 4 'diamonds)
                           (make-bid 4 'diamonds)))))
    ))
  )
