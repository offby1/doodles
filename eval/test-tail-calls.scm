;; This is a simple tail-recursive procedure; it should run for
;; forever.
(define (str) (str))

;; This is a simple recursive, but not tail-recursive, procedure.  It
;; should blow up the stack.
(define (ntr) (+ (ntr) 0))

;; These are two mutually-tail-recursive procedures.  They should run
;; forever.
(define (a) (b))
(define (b) (a))
