(define integer->my-char #f)

(define my-char? #f)

(define char-set-size #f)

(define my-char->integer #f)

(let ()
  (define my-chars
    (list #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o
          #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5
          #\6 #\7 #\8 #\9 #\space #\, #\.  #\?  ))
  
  (set! char-set-size (length my-chars))

  (set! integer->my-char 
        (let ((my-chars-vector (list->vector my-chars)))
          (lambda (n)
            (vector-ref my-chars-vector n))))

  (set! my-char->integer
        (lambda (ch)
          (if (not (my-char? ch))
              (error ch "is not one of my-chars"))
          (- char-set-size
             (length (memq ch my-chars)))))

  (set! my-char? 
        (lambda (thing)
          (not (not (memq thing my-chars))))))
