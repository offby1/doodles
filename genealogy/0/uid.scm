(define uid=? string=?)
(define (uid? thing) 
  (and  
   (string? thing)
   (>= (string-length thing) 4)          ; two @s, a letter, and a digit
   (char=? #\@ (string-ref thing 0))
   (memq (string-ref thing 1) '(#\I #\F)) 
   (char=? #\@ (string-ref thing (- (string-length thing)
                                    1)))
   (let ((number (make-shared-substring thing 2 (- (string-length
                                                    thing)
                                                   1))))
     (not (not (string->number number))))))