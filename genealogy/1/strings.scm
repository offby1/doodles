;; counts from zero
(define nth-word #f)
(define string-tail #f)

(let ()
  
  (define eat-whitespace
    (lambda (str)
      (define chars-skipped 0)
      (let loop ((str str))
        (if (and (not (zero? (string-length str)))
                 (char-whitespace? (string-ref str 0)))
            (begin
              (set! chars-skipped (+ 1 chars-skipped))
              (loop (make-shared-substring str 1 (string-length str))))
          (cons str chars-skipped)))))

  ;; Doesn't actually return a string -- instead it returns a pair of
  ;; integers.  The first is the index into STR of the first character
  ;; in the first word (or, to put it another way, the length of the
  ;; leading whitespace); the second is the length of the word.  To
  ;; actually get the string, do (make-shared-substring STR (car ret)
  ;; (cdr ret)).  This apparant inconvenience is actually handy if you
  ;; want to skip over the first word -- you know to skip (+ (car ret)
  ;; (cdr ret)) characters.
  (define (first-word str)
    
    (define ws (eat-whitespace str))
    (set! str (car ws))
    
    (let loop ((str str)
               (chars-examined 0))
      ;; (display "str:           `") (display str) (display "'") (newline)
      ;; (display "chars-examined: ") (display chars-examined) (newline)
      ;; (display "result:        `") (display result) (display "'") (newline)
      (if (or
           (zero? (string-length str))
           (char-whitespace? (string-ref str 0)))
          (cons (cdr ws) chars-examined)
        (loop (make-shared-substring str 1 (string-length str))
              (+ 1 chars-examined)))))

  (set!
   nth-word
   (lambda (n str)

     (if (or
          (not (integer? n))
          (negative? n))
         (error "n must be a non-negative integer, but is instead " n))

     (let loop ((n n)
                (result str))
       (let ((fw (first-word result)))
         (if (zero? n)
             (make-shared-substring
              result
              (car fw)
              (+ (car fw) (cdr fw)))
           (loop (- n 1)
                 (make-shared-substring
                  result
                  (+ (car fw) (cdr fw)))))))))
  (set!
   string-tail
   (lambda (str k)
     (let loop ((str str)
                (words-to-skip k))
       (if (zero? words-to-skip)
           (car (eat-whitespace str))
         (let ((chars-to-skip 
                (let ((tmp (first-word str)))
                  (+ (car tmp)
                     (cdr tmp)))))
           (loop (make-shared-substring str chars-to-skip)
                 (- words-to-skip 1))))))))


