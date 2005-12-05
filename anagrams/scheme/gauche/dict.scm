(define-module dict
  (use bag))

(select-module dict)

(define *ht* (make-hash-table 'string=?))
(define word-acceptable?
  (let ((has-vowel-regexp #/[aeiou]/i)
        (has-non-letter-regexp #/[^a-z]/i))
    (lambda (word)
      (let ((l (string-length word)))
        (and (not (zero? l))

             (not (string-incomplete? word))
             
             ;; it's gotta have a vowel.
             (rxmatch has-vowel-regexp word)
             
             ;; it's gotta be all ASCII, all the time.
             (not (rxmatch has-non-letter-regexp word))
             
             ;; it's gotta be two letters long, unless it's `i' or `a'.
             (or (string=? "i" word)
                 (string=? "a" word)
                 (< 1 l)))))))
(call-with-input-file "/usr/share/dict/words"
  (lambda (p)
    (let loop ((line (read-line p #t)))
      (when (not (eof-object? line))
        (when (word-acceptable? line)
          (hash-table-put! *ht*  line (bag line)))
        (loop (read-line p #t))))))

(display "Dictionary has ")
(display (hash-table-num-entries *ht*))
(display " entries")
(newline)
(provide "dict")
