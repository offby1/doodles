;;   -*- mode: scheme48; scheme48-package: boink -*-
(define (boink)
  (display "Table has ")
  (let ((alist '()))
    (table-walk (lambda (k v)
                  (set! alist (cons (cons k v) alist)))
                *the-hash-table*)
    (display (length alist)))
  (display " entries")
  (newline)

  (let ((t22 (table-ref *the-hash-table* 22))
        (alist '()))
    (table-walk (lambda (k v)
                  (set! alist (cons (cons k v)
                                    alist)))
                t22)
    (write alist)
    (newline))
  )

(define *the-hash-table*
  (make-integer-table))

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
    "/usr/share/dict/words"

  (lambda (p)
    (let loop ((word (read-line p))
               (result '()))
      (if (eof-object? word)
          result
        (let* ((wl (string-length word))
               (sub-table (table-ref *the-hash-table* wl)))
          (if (not sub-table)
              (begin
                (set! sub-table (make-string-table))
                (table-set! *the-hash-table* wl sub-table)))
          (table-set! sub-table word #t)
          (loop (read-line p)
                (cons word result)))
        ))))

