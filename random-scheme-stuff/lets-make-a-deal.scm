;; simulates the famous "Let's Make A Deal" paradox
(require 'random)
(define run-experiment
  (let ((trials 0)
        (switching-successes 0)
        (doors (make-vector 3 0)))
    
    (define (choose-random-door) (random (vector-length doors)))

    (define (choose-empty-door-ignoring number)
      (let loop ((choice (choose-random-door)))
        (if (and (not (= choice number))
                 (zero? (prize choice) ))
            choice
          (loop (choose-random-door)))))

    (define (choose-random-door-ignoring n1 n2)
      (let loop ((choice (choose-random-door)))
        (if (and (not (= choice n1))
                 (not (= choice n2)))
            choice
          (loop (choose-random-door)))))
    
    (define (prize K) (vector-ref doors K))

    (lambda ()
      
      ;; Hide the prize
      (vector-fill! doors 0)
      (vector-set! doors (choose-random-door) 1)

      (let* ((players-first-choice (choose-random-door))
             (montys-door (choose-empty-door-ignoring players-first-choice))
             (players-second-choice (choose-random-door-ignoring
                                     montys-door
                                     players-first-choice)))
        (if (> (prize players-second-choice)
               (prize players-first-choice))
            (begin
              (set! switching-successes (+ 1 switching-successes))))
        (set! trials (+ 1 trials)))

      (/ switching-successes trials)
      )))
