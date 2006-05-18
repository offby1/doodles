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

;; returns a list of words that can be made from CRITERION-BAG.
(define (prune-dictionary criterion-bag)
  (let ((rv '()))
    (table-walk (lambda (number words)
                  (if (subtract-bags criterion-bag number)
                      (set! rv (cons (cons number words)
                                     rv))))
                *the-dictionary*)
    rv))

(define *the-dictionary* (make-integer-table))

(define (snarf-dictionary)
  (display "Reading dictionary; this takes a long time ... ")
  (let loop ((wordlist (with-cwd
                        "/usr/share/dict"
                        (call-with-input-file (find file-readable? (list "words" "american-english") )
                          (lambda (p)
                            (port->string-list p))))))
    (if (pair? wordlist)
        (let ((word (car wordlist)))
          (if (word-acceptable? word)
              (let* ((num  (bag word))
                     (prev (table-ref *the-dictionary* num)))
                (if (not prev)
                    (set! prev '()))
                (set! prev (cons word prev))
                (table-set! *the-dictionary* num prev)))
          (loop (cdr wordlist)))))
  (display "done")
  (newline))
