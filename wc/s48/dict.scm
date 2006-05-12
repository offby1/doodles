;;   -*- mode: scheme48; scheme48-package: dict -*-

;; each entry is a "sub" hash table mapping words to #t.  (So I'm
;; actually using it as a set of words.)  In each sub-table, all the
;; words are the same length, and that length is the key in the main
;; table for that sub-table.

;; Thus we have about 20-some sets of words: one-letter words,
;; two-letter words, and so on up to twenty-letter words.
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
    "/usr/share/dict/american-english"


  (lambda (p)
    (display "Oh, I'm reading that dictionary ... ")
    (let loop ((result '()))
      (let ((word (read-line p)))
        (if (eof-object? word)
            result
          (let* ((wl (string-length word))
                 (sub-table (table-ref *the-hash-table* wl)))
            (if (not sub-table)
                (begin
                  (set! sub-table (make-string-table))
                  (table-set! *the-hash-table* wl sub-table)))
            (table-set! sub-table word #t)
            (loop (cons word result)))
          )))

    (display "done.")
    (newline)))
