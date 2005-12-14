(load-option 'regular-expression)
(load-option 'rb-tree)
(define word-acceptable?
  (let ((has-vowel-regexp (rexp-compile (rexp-case-fold (string->char-set "aeiou"))))
        (has-non-ASCII-regexp (rexp-compile (char-set-invert  (ascii-range->char-set
                                                               (char->ascii #\a)
                                                               (+ 1 (char->ascii #\z)))))))
    (lambda (word)
      (let ((l (string-length word)))
        (and (not (zero? l))
             
             ;; it's gotta have a vowel.
             (re-string-search-forward has-vowel-regexp word)
             
             ;; it's gotta be all ASCII, all the time.
             (not (re-string-search-forward has-non-ASCII-regexp word))
             
             ;; it's gotta be two letters long, unless it's `i' or `a'.
             (or (string=? "i" word)
                 (string=? "a" word)
                 (< 1 l)))))))

;; return a dictionary of words that can be made from CRITERION-BAG.
;; The dictionary is a list of entries; each entry is (cons key words)
(define (snarf-dictionary criterion-bag)
  (let ((*the-table* (make-eqv-hash-table)))
    (call-with-input-file
        "/usr/share/dict/words"
      (lambda (p)
        (display "Reading dictionary ... ")
        (let loop ((word (read-line p))
                   (words-read 0))
          (if (eof-object? word)
              (filter (lambda (number words)
                        (subtract-bags criterion-bag number)) 
                      (hash-table->alist *the-table*))
            (begin
              (if (word-acceptable? word)
                  (let* ((num  (bag word))
                         (prev (hash-table/get *the-table* num '())))
                    (set! prev (cons word prev))

                    (hash-table/put! *the-table* num prev)))
              
              (loop (read-line p)
                    (+ 1 words-read)))))))))
