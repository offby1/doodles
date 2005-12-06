(declare (unit dict))
(declare (uses bag))

(require-extension regex)

(define word-acceptable?
  (let ((has-vowel-regexp (regexp "[aeiou]" #t))
        (has-non-ASCII-regexp (regexp "[^a-zA-Z]" #t)))
    (lambda (word)
      (let ((l (string-length word)))
        (and (not (zero? l))
             
             ;; it's gotta have a vowel.
             (string-match has-vowel-regexp word)
             
             ;; it's gotta be all ASCII, all the time.
             (not (string-match has-non-ASCII-regexp word))
             
             ;; it's gotta be two letters long, unless it's `i' or `a'.
             (or (string=? "i" word)
                 (string=? "a" word)
                 (< 1 l)))))))

;; return a dictionary of words that can be made from CRITERION-BAG.
;; The dictionary is a list of entries; each entry is (cons key words)
(define (snarf-dictionary criterion-bag)
  (let ((*the-hash-table* (make-hash-table =))
        (rv '()))
    (call-with-input-file
        "words"
      (lambda (p)
        (display "Reading dictionary ... ") (flush-output)
        (let loop ((word (read-line p))
                   (words-read 0))
          (if (eof-object? word)
              (begin
                (display "done")
                (newline)
                (hash-table->alist *the-hash-table*))
            (begin
              (when (zero? (remainder words-read 100))
                (display "Read ")
                (display words-read)
                (display " words ...")
                (newline))
              (when (word-acceptable? word)
                (let* ((num  (bag word))
                       (prev (hash-table-ref *the-hash-table* num (lambda () #f))))
                  (when (not prev)
                    (set! prev '()))
                  (set! prev (cons word prev))
                  (hash-table-set! *the-hash-table* num prev)))
              (loop (read-line p)
                    (+ 1 words-read)))))))))
