;; Return the Nth term of the Fibonacci sequence
(define (fib-term term)
  (let loop ((term term)
	     (last 1)
	     (next-to-last 0))

    ;; Just for fun, print out the ratio of last to next-to-last,
    ;; as long as the latter isn't zero.
    ;; (if (not (= 0 next-to-last)) (begin (display "-> ") (write (/ last next-to-last)) (newline)))

    (cond
     ((= term 0)
      #f)
     ((= term 1)
      next-to-last)
     ((= term 2)
      last)
     (else
      (loop (- term 1)
	    (+ last next-to-last)
	    last)))))

;; Return a list of the first N terms

(define (fib requested-terms)
  (let loop ((list-so-far (list 1 0))
	     (list-length 2))
    (if (< list-length requested-terms)
	(loop (cons (+ (car list-so-far)
		       (cadr list-so-far))
		    list-so-far)
	      (+ 1 list-length))
	(reverse list-so-far))))
