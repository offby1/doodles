;; given a list of numbers, a procedure that takes N numeric
;; arguments, and N itself, return a list of numbers gotten by
;; applying the procedure many times -- first, to SEQ[0] through SEQ[N
;; - 1] inclusive, then to SEQ[1] through SEQ[N] inclusive, and so on,
;; up to and including SEQ[L - N] through SEQ[L - 1] inclusive, where
;; L is the length of the list SEQ.

;; You could figure a moving average like this:

;;(define (average . args) (/ (apply + args) (length args)))

;;(map-n average (list 1 2 3 4 5 6 7 8 9) 3)

(require 'common-list-functions)

(define (map-n proc seq n)

  (define (first-n n seq)
    (cond 
     ((= n (length seq)) seq)
     ((< n (length seq)) (butnthcdr n seq))
     (t (error "Bad argument to first-n" n seq))))

  
  (if (or (not (list? seq))
          (> n (length seq))
          (not (procedure? proc)))
      (error "Bad args!"))

  (let loop ((seq seq)
             (results '()))
    (if (> n (length seq))
        (reverse results)
      (loop (cdr seq)
            (cons (apply proc (first-n n seq)) results)))))
