;;   -*- mode: scheme48; scheme48-package: boink -*-
(define (boink)
  (display "Boink!!") (newline)
  (for-each (lambda (p) (table-set! *the-hash-table*
                                    p
                                    #t))
            *the-list*)
  (display "Table has ")
  (display (length *the-list*))
  (display " keys")
  (newline)
  )

(define *the-hash-table*
  (make-string-table))

(define (read-line port)
  (iterate loop ((input* c port read-char))
           ((chars '()))
    (if (char=? c #\newline)
        (list->string (reverse chars))
      (loop (cons c chars)))
    (if (null? chars)
        (eof-object)                   ; from the PRIMITIVES structure
      (list->string (reverse chars)))))

(define *the-list*
  (call-with-input-file
      "/usr/share/dict/words"
    (lambda (p)
      (let loop ((word (read-line p))
                 (result '()))
        (if (eof-object? word)
            result
          (begin
            (loop (read-line p)
                  (cons word result)))
          )))))

