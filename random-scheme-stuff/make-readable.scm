(define (make-readable s) 

  (define (char->string c)

    (define (possibly-pad s)
      (string-append
       (if (= 1 (string-length s))
           "0"  
         "")
       s))
    
    (define (string-downcase s)
      (let loop ((s s)
                 (result ""))
        (if (zero? (string-length s))
            result
          (loop (substring s 1 (string-length s))
                (string-append result
                               (string (char-downcase (string-ref s
                                                                  0))))))))
      
    (possibly-pad (string-downcase (number->string (char->integer c) 16))))

  (apply string-append (map char->string (string->list s))))
