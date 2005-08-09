;;   -*- mode: scheme48; scheme48-package: boink -*-
(define (boink)
  (display "Boink!!")
  (table-walk (lambda (k v)
                (display k)
                (newline))
              *the-hash-table*)
  (newline))

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

(if #f
    (call-with-input-file
        "boink.scm"
                                        ;"/usr/share/dict/words"
      (lambda (p)
        (let loop ((word (read-line p)))
          (if (not (eof-object? word))
              (begin
                (table-set! *the-hash-table* word #t)
                (loop (read-line p))))))))

(table-set! *the-hash-table* "hey" 'is-for-horses)
(table-set! *the-hash-table* "you" 'why-because-we-LIKE-you)
