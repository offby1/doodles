(define (find-char s pred from-front)
  (let loop ((chars-to-examine (string-length s)))
    (if (zero? chars-to-examine)
        #f
      (let* ((index (if from-front
                        (- (string-length s)
                           chars-to-examine)
                      (- chars-to-examine 1)))
             (char (string-ref s index)))
        
        (if (pred char)
            index
          (loop (- chars-to-examine 1)))))))

(define (strip-whitespace s)
  
  (define (strip-leading-whitespace-and-reverse s)
    (if (or
         (zero? (string-length s))
         (not (char-whitespace? (string-ref s 0)))) 
        (list->string (reverse (string->list s)))    
      (strip-leading-whitespace-and-reverse (substring s 1 (string-length s)))))
  
  (strip-leading-whitespace-and-reverse (strip-leading-whitespace-and-reverse s)))

(provide 'whitespace)