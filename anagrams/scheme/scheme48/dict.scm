;;   -*- mode: scheme48; scheme48-package: dict -*-

(define (read-line port)
  (iterate loop ((input* c port read-char))
           ((chars '()))
    (if (char=? c #\newline)
        (list->string (reverse chars))
      (loop (cons c chars)))
    (if (null? chars)
        (eof-object)                   ; from the PRIMITIVES structure
      (list->string (reverse chars)))))

(define word-acceptable?
  (let ((has-vowel-regexp (set "aeiouAEIOU"))
        (has-non-ASCII-regexp (negate (ascii-ranges #\a #\z #\A #\Z))))
    (lambda (word)
      (let ((l (string-length word)))
        (and (not (zero? l))
             
             ;; it's gotta have a vowel.
             (match has-vowel-regexp word)
             
             ;; it's gotta be all ASCII, all the time.
             (not (match has-non-ASCII-regexp word))
             
             ;; it's gotta be two letters long, unless it's `i' or `a'.
             (or (string=? "i" word)
                 (string=? "a" word)
                 (< 1 l)))))))

;; return a dictionary of words that can be made from CRITERION-BAG.
;; The dictionary is a list of entries; each entry is (cons key words)
(define (snarf-dictionary criterion-bag)
  (let ((*the-hash-table* (make-integer-table))
        (rv '()))
    (call-with-input-file
        "/usr/share/dict/words"
      (lambda (p)
        (display "Reading dictionary ... ")
        (let loop ((word (read-line p)))
          (if (eof-object? word)
              (begin
                (table-walk (lambda (number words)
                              (if (subtract-bags criterion-bag number)
                                  (set! rv (cons (cons number words)
                                                 rv))))
                            *the-hash-table*)
                (display "done")
                (newline)
                rv)
            (begin
              (if (word-acceptable? word)
                  (let* ((num  (bag word))
                         (prev (table-ref *the-hash-table* num)))
                    (begin
                      (if (not prev)
                          (set! prev '()))
                      (set! prev (cons word prev))
                      (table-set! *the-hash-table* num prev))))
              (loop (read-line p)))))))))
