(define word-acceptable?
  (let ((has-vowel-regexp (make-regexp "[aeiouAEIOU]"))
        (has-non-ASCII-regexp (make-regexp "[^a-zA-Z]")))
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
  (let ((*the-hash-table* (make-integer-table))
        (rv '()))
    (call-with-input-file
        ;;"words"
        "/usr/share/dict/words"
      (lambda (p)
        (display "Reading dictionary ... ")
        (let loop ((words-read 0)
                   (next-power-of-two 1) ;for diagnostics
                   )
          
          (let ((word (read-line p)))
            (if (eof-object? word)
                (let ((words-kept 0))
                  (table-walk (lambda (number words)
                                (if (subtract-bags criterion-bag number)
                                    (begin
                                      (set! words-kept (+ 1 words-kept))
                                      (set! rv (cons (cons number words)
                                                     rv)))))
                              *the-hash-table*)
                  (display words-read)
                  (display " words; kept ")
                  (display words-kept)
                  (newline)
                  rv)
              (begin
                (if (= words-read next-power-of-two)
                    (begin
                      (for-each display (list "Read " words-read " words: " word) )
                      (newline)
                      ))
                (if (word-acceptable? word)
                    (let* ((num  (bag word))
                           (prev (table-ref *the-hash-table* num)))
                      (if (not prev)
                          (set! prev '()))
                      (set! prev (cons word prev))
                      (table-set! *the-hash-table* num prev)))
                (loop (+ 1 words-read)
                      (if (= words-read next-power-of-two)
                          (* 2 next-power-of-two)
                        next-power-of-two))))))))))
