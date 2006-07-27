;; () -> ()
;; (a) -> ((a))
;; (a b) -> ((a b) (b a))
;; (a b c) -> ((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; From SICP, 2nd ed., more or less

(module permute mzscheme
(require (only (lib "1.ss" "srfi") delete append-map))
(define (perms s)

  (if (null? s)
      (list '())
    (append-map (lambda (x)
                  (map (lambda (p) (cons x p))
                       (perms (delete x s))))
                s)))
)
