(define-macro amb
  (lambda alts...
    `(let ((+prev-amb-fail amb-fail))
       (call/cc
        (lambda (+sk)

          ,@(map (lambda (alt)
                   `(call/cc
                     (lambda (+fk)
                       (set! amb-fail
                             (lambda ()
                               (set! amb-fail +prev-amb-fail)
                               (+fk 'fail)))
                       (+sk ,alt))))
                 alts...)

          (+prev-amb-fail))))))


(define call/cc call-with-current-continuation)


(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
          (lambda ()
            "throws error: amb tree exhausted"
            (error "amb tree exhausted")))))

(initialize-amb-fail)

(define assert
  (lambda (pred)
    (if (not pred) (amb))))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

(let
    ((q0 (amb 0 1 2 3 4 5 6 7))         ; all the possible rows that
                                        ; this queen might occupy.  We
                                        ; assume that this queen's
                                        ; rank is always 0.
     (q1 (amb 0 1 2 3 4 5 6 7))
     (q2 (amb 0 1 2 3 4 5 6 7))
     (q3 (amb 0 1 2 3 4 5 6 7))
     (q4 (amb 0 1 2 3 4 5 6 7))
     (q5 (amb 0 1 2 3 4 5 6 7))
     (q6 (amb 0 1 2 3 4 5 6 7))
     (q7 (amb 0 1 2 3 4 5 6 7)))

  (assert (distinct? (list q0 q1 q2 q3 q4 q5 q6 q7)))

  (list q0 q1 q2 q3 q4 q5 q6 q7))
