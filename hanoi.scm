(define (hanoi source dest helper disks)
  (cond ((> disks 0)
	   (append (hanoi source helper dest (- disks 1))
		   (list (list source dest))
		   (hanoi helper dest source (- disks 1))))
	  (else '())))

(hanoi 'a 'b 'c 3)
