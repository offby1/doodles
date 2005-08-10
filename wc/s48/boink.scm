;;   -*- mode: scheme48; scheme48-package: boink -*-
(define (boink)
  (display "Boink!!")
  (let ((table '()))
    (table-walk (lambda (k v)
                  (set! table (cons (cons k v) table))) *the-hash-table*)
    (write table)
    (newline))
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

(call-with-input-file
    "ten-words"
  (lambda (p)
    (let loop ((c (read-char p)))
      (if (not (eof-object? c))
          (let* ((key (string c))
                 (probe (table-ref *the-hash-table* key)))
            (table-set! *the-hash-table* key (if probe (+ 1 probe) 1))
            (loop (read-char p)))))))
