
(let ()

  ;; Does a little work, then returns a thunk that will do the rest of
  ;; the work.
  (define (make-engine name)
    (call-with-current-continuation
     (lambda (return)
       (define (yield) (call-with-current-continuation (lambda (cc) (return (lambda () (cc 'ignored))))))
       (display-many name ": First bit of work" #\newline)
       (yield)
       (display-many name ": Second bit of work" #\newline)
       (yield)
       (display-many name ": Third bit of work" #\newline)
       (yield)
       (display-many name ": Last bit of work" #\newline)
       (yield)
       (display-many name ": done" #\newline)
       'final-value)))

  (define x (make-engine "bob"))
  (define y (make-engine "sally"))

  (display-many "OK, now to call them..." #\newline)
  (let loop ()
    (if (or (procedure? x)
            (procedure? y))
        (begin
          (if (procedure? x) (set! x (x)))
          (if (procedure? y) (set! y (y)))
          (loop))
      "all done")))

(let ()
  
  (define (make-engine thunks)
    (cond
     ((null? thunks)
      (lambda ()
        (display "Does nothing\n")))
     ((null? (cdr thunks))
      (lambda ()
        ((car thunks))))
     (#t
      (lambda ()
        ((car thunks))
        (make-engine (cdr thunks))))))

  (define (run-engine e) (if (procedure? e) (run-engine (e)) e))

  (let ((e (make-engine (list (lambda ()
                                (display "E works\n")
                                "I'm a task\n")
                              (lambda ()
                                "I'm another\n"))))
        (e2 (make-engine (list (lambda ()
                                 (display "e2 runs\n"))
                               (lambda ()
                                 "The rain in spain\n")))))

    (run-engine e)
    (run-engine e2)))
