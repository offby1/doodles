(define (ut-entries)
  (dynamic-wind
   setutent
   (lambda ()
     (let loop ((u (getutent))
                (result '()))
       (if (not u)
           (reverse result)
         (loop (getutent)
               (cons u result)))))
   endutent))
