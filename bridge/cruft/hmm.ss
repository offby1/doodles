(require (lib "list.ss" "srfi" "1"))
(require (lib "thread.ss"))
(require (lib "trace.ss"))
(define passes 10)

(define consumer
  (lambda (ca)
    (printf "Consumer: passes is ~s~n" passes)
    (printf "Consumer got a completed auction: ~s~n" ca)))

(define-values (consumer-thread-id send) (consumer-thread consumer))

;; simple function that acts vaguely like
;; "some-auctions-with-given-prefix"
(define makes-big-lists
  (lambda (seq max exit)
    (when (any (lambda (x)
                 (> x max))
               seq)
      (raise-type-error 'makes-big-lists (format "list of integers <= ~a" max) seq))
    (for-each (lambda (n)
                (define (allowable-successors seq max)
                  (let ((l (last seq)))
                    (iota (- max l) (+ 1 l))))
                (let ((extended (append seq (list n))))
                  (if (= n max)
                      (begin
                        (printf "Producer: passes is ~a~n" passes)
                        (when (not (positive? passes))
                          (exit))
                        (send extended)
                        (set! passes (- passes 1)))
                    (makes-big-lists extended max exit))))
              (allowable-successors seq max)))
  )
(trace makes-big-lists)
(define (go)
  (call/cc
   (lambda (exit)
     (makes-big-lists '(0) 5 exit)
     ))
  (kill-thread consumer-thread-id))