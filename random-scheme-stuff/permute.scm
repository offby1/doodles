;; () -> ()
;; (a) -> ((a))
;; (a b) -> ((a b) (b a))
;; (a b c) -> ((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; From SICP, 2nd ed., more or less

#lang scheme

(define (perms s)

  (if (null? s)
      (list '())
    (append-map (lambda (x)
                  (map (curry cons x)
                       (perms (remove x s))))
                s)))

(let ([n 9])
  (printf "There are at least ~a permutations of the first ~a nonnegative integers~%"
          (length (perms (build-list n values)))
          n))
