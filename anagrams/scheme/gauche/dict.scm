(define-module dict
  (use bag)
  (use srfi-1))

(select-module dict)

(define word-acceptable?
  (let ((has-vowel-regexp #/[aeiou]/i)
        (has-non-letter-regexp #/[^a-z]/i))
    (lambda (word)
      (let ((l (string-length word)))
        (and (not (zero? l))

             ;; it appears that word will be "incomplete" if and only
             ;; if it contains non-ASCII characters.
             (not (string-incomplete? word))
             
             ;; it's gotta have a vowel.
             (rxmatch has-vowel-regexp word)
             
             ;; it's gotta be all ASCII, all the time.
             (not (rxmatch has-non-letter-regexp word))
             
             ;; it's gotta be two letters long, unless it's `i' or `a'.
             (or (string=? "i" word)
                 (string=? "a" word)
                 (< 1 l)))))))
(define *ht* (make-hash-table 'eqv?))
(call-with-input-file "/usr/share/dict/words"
  (lambda (p)
    (let loop ((line (read-line p #t)))
      (when (not (eof-object? line))
        ;; BUGBUG -- lower-casify the string before sticking it in the table.
        (when (word-acceptable? line)
          (let* ((num (bag line))
                 (prev (hash-table-get *ht* num #f)))
            (if (not prev)
                (set! prev '()))
          (hash-table-put! *ht*  num (cons line prev))))
        (loop (read-line p #t))))))

(display "Dictionary has ")
(display (hash-table-num-entries *ht*))
(display " entries")
(newline)

;; now convert the hash table to a list
(define (dictionary-for criterion-bag)
  (filter 
   (lambda (pair) (subtract-bags criterion-bag (car pair)))
   (hash-table-map *ht* cons)))

(provide "dict")
