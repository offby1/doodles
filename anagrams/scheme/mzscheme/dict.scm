(module dict
mzscheme
(require "bag.scm"
         (lib "trace.ss")
         (only (lib "list.ss") quicksort)
         (only (lib "1.ss" "srfi") filter))
(provide init)

(define (wordlist->hash fn)
  (with-input-from-file fn
    (lambda ()
      (let ((dict (make-hash-table 'equal))
            (trailing-whitespace-re (regexp "[ \r]+$")))
        (fprintf (current-error-port) "Reading dictionary ~s ... " fn)
        (let loop ((words-read 0))
          (let ((word (read-line)))
            (when (not (eof-object? word))
              (let ((word (regexp-replace trailing-whitespace-re (string-downcase word) "")))
                (when (word-acceptable? word)
                  (adjoin-word! dict word))
                (loop (+ 1 words-read))))))
        (fprintf (current-error-port) "done; ~s entries~%" (length (apply append (map cdr (hash-table-map dict cons)))))
        dict))))

(define (adjoin-word! dict word)
  (let* ((this-bag (bag word))
         (probe (hash-table-get dict this-bag (lambda () #f))))
    (cond
     ((not probe)
      (hash-table-put! dict this-bag  (list word)))
     ((not (member word probe))
      (hash-table-put! dict this-bag (cons word probe)))
     )))

(define word-acceptable?
  (let ((has-vowel-regexp (regexp "[aeiouAEIOU]"))
        (has-non-ASCII-regexp (regexp "[^a-zA-Z]"   )))
    (lambda (word)
      (let ((l (string-length word)))
        (and (not (zero? l))

             ;; it's gotta have a vowel.
             (regexp-match has-vowel-regexp word)

             ;; it's gotta be all ASCII, all the time.
             (not (regexp-match has-non-ASCII-regexp word))

             ;; it's gotta be two letters long, unless it's `i' or `a'.
             (or (string=? "i" word)
                 (string=? "a" word)
                 (< 1 l)))))))

(define (bag-acceptable? this bag-to-meet)
  (and (or (bags=? bag-to-meet this)
           (subtract-bags bag-to-meet this))
       this))

(define (init bag-to-meet dict-file-name)
  (define (biggest-first e1 e2)
    (let* ((s1 (cadr e1))
           (s2 (cadr e2))
           (l1 (string-length s1))
           (l2 (string-length s2)))
      (or (> l1 l2)
          (and (= l1 l2)
               (string<? s1 s2)))))

  (quicksort
   (let ((result (filter (lambda (entry)
                           (bag-acceptable? (car entry) bag-to-meet))
                         (hash-table-map (wordlist->hash dict-file-name) cons))))
     (printf
      "Pruned dictionary now has ~a elements~%"
      (apply + (map (lambda (seq)
                      (length (cdr seq)))
                    result)))
     result)
   biggest-first)

  ))
