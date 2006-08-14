(define (hotpo n)
  (if (even? n)
      (/ n 2)
    (+ 1 (* 3 n))))

;; (call fn init 0) => ()
;; (call fn init 1) => ((fn init))
;; (call fn init 2) => ((fn init) (fn (fn init)))
;; (call fn init 3) => ((fn init) (fn (fn init)) (fn (fn (fn init))))
;; (call fn init n) => ((fn init) ... fn ^ n init)
;; (call fn init n) => (append (fn init (- n 1)) (fn (last (previous-list))))

(define (call fn init times)
  (if (or
       (negative? times)
       (not (integer? times)))
      (error times "is not a non-negative integer")
    (cond
     ((= times 0)
      '())
     ((= times 1)
      (list (fn init)))
     (#t
      (let ((temp (call fn init (- times 1))))
	(define (last l)
	  (list-ref l (- (length l)
			 1)))
	(append temp
	 (list (fn (last temp)))))))))

(define (call-until fn init finished?)
  (let loop ((results (list (fn init))))
    (define (last l)
      (list-ref l (- (length l)
		     1)))
    (if (finished? (last results))
	results
      (loop (append results
		    (list (fn (last results))))))))

(let loop ((n 0)
	   (results '()))

  (if (= n 30)
      results
    (loop (+ 1 n)
	  (cons (list n (call-until hotpo n (lambda (n) (or (= n 0) (= n 1)))))
		results))))
