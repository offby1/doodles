;; Comment these if your Scheme implementation has u8vectors.
;; U8vectors are part of Gambit Scheme, and are presumably faster than
;; regular vectors, although I've never actually noticed the
;; difference.
(define make-u8vector make-vector)
(define u8vector-length vector-length)
(define u8vector-ref vector-ref)
(define u8vector-set! vector-set!)

(define (survey string)

  (define (vector-modify! v index fn)
    (u8vector-set! v index (fn (u8vector-ref v index))))

  (let ((result (make-u8vector (+ 1 (- (char->integer #\z)
				       (char->integer #\a)))
			     0)))
    (let loop ((string string))
      (if (= (string-length string) 0)
	  result
	(let ((ch (string-ref string 0)))
	  (if (char-alphabetic? ch)
	      (vector-modify! result
			      (- (char->integer (char-downcase ch))
				 (char->integer #\a))
			      (lambda (x) (+ 1 x))))
	  (loop (substring string 1 (string-length string))))))))

(define (is-contained-in? short-s/v long-s/v)
  ;;(print 'is-contained-in? short long)

  (define (all-less-than-or-equal a b)
    (let loop ((i 0)
	       (result #t))
      (if (or
	   (= i (u8vector-length b))
	   (not result))
	  result
	  (loop (+ i 1)
		(<= (u8vector-ref a i)
		    (u8vector-ref b i))))))

  (and (>= (string-length (car long-s/v))
	   (string-length (car short-s/v)))
       (all-less-than-or-equal (cdr short-s/v)
			       (cdr long-s/v))))
;(trace is-contained-in?)
(define (subtract-letters big-s/v small-s/v)

  (define (vector-subtract big little)
    (let ((difference (make-u8vector (u8vector-length big))))
      (let loop ((slots-checked 0))
	(if (= slots-checked (u8vector-length difference))
	    difference
	  (let ((diff (- (u8vector-ref big slots-checked)
			 (u8vector-ref little slots-checked))))
	    (if (negative? diff)
		(error "Vector " big " isn't really bigger than vector " little))
	    (u8vector-set! difference slots-checked diff)
	    (loop (+ 1 slots-checked)))))))

  (let ((vector-diff (vector-subtract (cdr big-s/v) (cdr small-s/v))))
    (cons
     (let loop ((result "")
		(letters-processed 0))
       (if (= letters-processed (u8vector-length (cdr big-s/v)))
	   result
	 (loop (string-append result (make-string (u8vector-ref
						   vector-diff
						   letters-processed)
						  (integer->char (+ letters-processed
								    (char->integer #\a)))))
	       (+ 1 letters-processed))))
	  vector-diff)))
;(trace subtract-letters)