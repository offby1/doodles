;; Chooses the string in STRINGS that is most likely to be text.

(define (most-deviant strings)

  (define (frequency-distribution str)
    (define counts (make-vector char-set-size 0))
    (let loop ((chars (string->list str)))
      (if (null? chars)
          counts
        (begin
          (let* ((ch (car chars))
                 (old-count (vector-ref counts (my-char->integer ch))))
            (vector-set! counts (my-char->integer ch) (+ 1 old-count)))
          (loop (cdr chars))))))

  (define (standard-deviation numbers)

    (define (mean numbers)
      (/ (apply + numbers)
         (length numbers)))

    (if (and
         (list? numbers)
         (> (length numbers) 1))
        (let ((m (mean numbers)))
          (sqrt (/ (apply + (map (lambda (n)
                                   (expt (- m n) 2))
                                 numbers))
                   (- (length numbers)
                      1))))
      #f))

  (cdar
   (sort  
    (map (lambda (str) (cons
                        (standard-deviation (vector->list (frequency-distribution str)))
                        str))
         strings)
    (lambda (a b)
      (> (car a)
         (car b))))))

;; Not foolproof, but pretty good
(define (decrypt str j)
  (most-deviant (j str)))