(declare (unit dict))
(declare (uses bag))

(require-extension regex)
(require-extension numbers)
(require-extension srfi-1)

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

(define *the-dictionary* #f)
(define *dict-cache-file-name* "cached-dictionary.scm")
(if (file-exists? *dict-cache-file-name*)
    (begin
      (display "Reading ")(write *dict-cache-file-name*) (display " ... ") (flush-output)
      (set! *the-dictionary* (with-input-from-file *dict-cache-file-name* read))
      (display (length *the-dictionary*))
      (display " entries")
      (newline))
  (let ((ht  (make-hash-table)))
    (call-with-input-file
        "words"
      (lambda (p)
        (display "Reading dictionary ... ")
        (newline)
        (let loop ((word (read-line p))
                   (words-read 0)
                   )
          (when (not (eof-object? word))
            (when (zero? (remainder words-read 1000))
              (display "Read ")
              (display words-read)
              (display " words ...")
              (newline))
            (if (word-acceptable? word)
                (let* ((num  (bag word))
                       (prev (hash-table-ref ht num (lambda () #f))))
                  (if (not prev)
                      (set! prev '()))
                  (set! prev (cons word prev))
                  (hash-table-set! ht num prev)))
            (loop (read-line p)
                  (+ 1 words-read)
                  )))))
    (set! *the-dictionary*
          ;; put longest words first.
          (sort
           (hash-table->alist ht)
           (lambda (e1 e2)
             (> (string-length (cadr e1))
                (string-length (cadr e2))))))
    (with-output-to-file *dict-cache-file-name*
      (lambda ()
        (write *the-dictionary*)))
    (display "Wrote " )
    (write *dict-cache-file-name*)
    (newline)))

;; return a dictionary of words that can be made from CRITERION-BAG.
;; The dictionary is a list of entries; each entry is (cons key words)
(define (dictionary-for criterion-bag)
  (filter (lambda (p)
            (subtract-bags criterion-bag (car p)))
          *the-dictionary*))
