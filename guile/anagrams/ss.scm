(define survey
  (let ((hash-cache (make-vector 
                     
                     ;; a prime number approx. equal to the number of
                     ;; dictionary entries
                     25013)))

    (define (vector-modify! v index fn)
      (uniform-vector-set! v index (fn (uniform-vector-ref v index))))

    (lambda (string)

      (let ((cache-hit (hash-ref hash-cache string #f)))
        (or cache-hit
            (begin
              (let ((result (make-uniform-vector (+ 1 (- (char->integer #\z)
                                                         (char->integer #\a)))
                                                 1
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
                      (loop (substring string 1 (string-length
                                                 string))))))

                (hash-set! hash-cache string result)

                result)))))))

(define (is-contained-in? short-s/v long-s/v)
  ;;(print 'is-contained-in? short long)

  (define (all-less-than-or-equal a b)
    (let loop ((i 0)
	       (result #t))
      (if (or
	   (= i (uniform-vector-length b))
	   (not result))
	  result
        (loop (+ i 1)
              (<= (uniform-vector-ref a i)
                  (uniform-vector-ref b i))))))

  (and (<= (string-length short-s/v)
	   (string-length long-s/v))
       (all-less-than-or-equal (survey short-s/v)
			       (survey long-s/v))))

;(trace is-contained-in?)
(define (subtract-letters big small)

  (define (vector-subtract big little)
    (let ((difference (make-uniform-vector (uniform-vector-length big) 1)))
      (let loop ((slots-checked 0))
	(if (= slots-checked (uniform-vector-length difference))
	    difference
	  (let ((diff (- (uniform-vector-ref big slots-checked)
			 (uniform-vector-ref little slots-checked))))
	    (if (negative? diff)
		(error "Vector " big " isn't really bigger than vector " little))
	    (uniform-vector-set! difference slots-checked diff)
	    (loop (+ 1 slots-checked)))))))

  (let* ((big-survey (survey big))
         (vector-diff (vector-subtract big-survey (survey small))))
    
    (let loop ((letters-to-process (uniform-vector-length big-survey))
               (result ""))
      (let ((this-char (integer->char (+ letters-to-process -1 (char->integer #\a)))))
        (if (zero? letters-to-process)
             result
          (loop (- letters-to-process 1)
                (string-append result 
                               (make-string 
                                (uniform-vector-ref vector-diff (- letters-to-process 1))
                                this-char)
                               )))))))
;(trace subtract-letters)