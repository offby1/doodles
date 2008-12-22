#lang scheme

(define (is-power-of-two? x)
  (and (integer? x)
       (positive? x)
       (exact? x)
       (or (= x 1)
           (and (even? x)
                (is-power-of-two? (/ x 2))))))

(define (thunk? thing)
  (and (procedure? thing)
       (equal? 0 (procedure-arity thing))))
(define thunk/c (flat-named-contract "thunk" thunk?))
(provide/contract (make-notifier [thunk/c . -> . thunk/c]))

(define (make-notifier thunk)
  (let ((times-called 0))
    (lambda ()
      (set! times-called (add1 times-called))
      (when (is-power-of-two? times-called)
        (thunk)))))

(define (make-fprintf-notifier  op format . args)
  (make-notifier
   (lambda ()
     (apply fprintf op format args))))

(define note-progress!
  (make-fprintf-notifier  (current-output-port) "Yo ~a~%" 'vinnie))
