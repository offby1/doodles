(define (dragon order)
  (define (internal-dragon orientation order forwards)

    (define segment list)

    (define (next orientation)
      (case orientation
        ((east ) 'north)
        ((north) 'west)
        ((west ) 'south)
        ((south) 'east)
        (else (error "Bad orientation:" orientation))))

    (if (<= order 0)
	(segment orientation)
      ((lambda (x y forwards)
	 (if forwards
	     (append x y)
	   (append y x)))
       (internal-dragon       orientation  (- order 1) forwards)
       (internal-dragon (next orientation) (- order 1) (not forwards))
       forwards)))

  (internal-dragon 'east order #t))

(dragon 4)

;; (east north west north west south west north west south east south west south west north)