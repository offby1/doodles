;; () -> ()
;; (a) -> ((a))
;; (a b) -> ((a b) (b a))
;; (a b c) -> ((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; From SICP, 2nd ed., more or less

(require 'filter)
(define (perms s)

  (define (remove item sequence) (filter (lambda (x) (not (eq? x item))) sequence))

  (if (null? s)                         ; empty set?
      (list '())                        ; sequence containing empty set
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (perms (remove x s))))
             s)))
