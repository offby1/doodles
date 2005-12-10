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

(define *the-dictionary*
  (call-with-input-file
      "words"
    (lambda (p)
      (display "Reading dictionary ... ")
      (newline)
      (let loop ((word (read-line p))
                 (words-read 0)
                 ;; the CAR is a bag; the CDR is a word.  One entry per
                 ;; dictionary word.
                 (pairs '()))
        (if (eof-object? word)
            pairs
          (begin
            (when (zero? (remainder words-read 1000))
              (display "Read ")
              (display words-read)
              (display " words ...")
              (newline))
            (loop (read-line p)
                  (+ 1 words-read)
                  (let ((b (bag word)))
                    (if (and #f (word-acceptable? word))
                        (cons (cons b word)
                              pairs)
                      pairs)))))))))

;; return a dictionary of words that can be made from CRITERION-BAG.
;; The dictionary is a list of entries; each entry is (cons key words)
(define (dictionary-for criterion-bag)
  (filter (lambda (p)
            (subtract-bags criterion-bag (car p)))
          *the-dictionary*))
