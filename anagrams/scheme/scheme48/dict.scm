;;   -*- mode: scheme48; scheme48-package: dict -*-

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

(define (snarf-dictionary)
  (call-with-input-file
      "/usr/share/dict/words"
    ;;"/tmp/a-few-words"
    (lambda (p)
      (let loop ((word (read-line p)))
        (if (eof-object? word)
            *the-hash-table*
          (let* ((num  (bag word))
                 (prev (table-ref *the-hash-table* num)))
            (if (not prev)
                (set! prev '()))
            (set! prev (cons word prev))
            (if (< 1 num)
                (table-set! *the-hash-table* num prev))
            (loop (read-line p)))
          )))))

;; return a dictionary of words that can be made from BAG.  The
;; dictionary is a list of entries; each entry is (cons key words)
(define (prune bag)
  (let ((rv '()))
    (table-walk (lambda (number words)
                  (if (subtract-bags bag number)
                      (set! rv (cons (cons number words)
                                     rv))))
                *the-hash-table*)
    rv))