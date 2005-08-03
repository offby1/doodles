;;; Reading the word list
(define (read-line port)
  (let loop ((l '()))
    (let ((c (read-char port)))
      (if (eof-object? c)
          c
          (if (char=? c #\newline)
              (list->string (reverse l))
              (loop (cons c l)))))))

(define (read-words port)
  (let loop ((words '())
             (line (read-line port)))
    (if (eof-object? line)
        words
        (loop (cons line words)
              (read-line port)))))


;;; Two words are roughly equal if they're the same length and differ
;;; only by a single character. We might forbid "no difference" for
;;; a slight improvement in performance, too.
(define (roughly-equal? s1 s2)
  (if (not (= (string-length s1)
              (string-length s2)))
      #f
      (let ((len (string-length s1)))
        (let loop ((i 0)
                   (diffs 0))
          (cond
           ((> diffs 1)
            #f)
           ((>= i len)
            #t)
           (else
            (loop (+ i 1)
                  (+ diffs (if (char=? (string-ref s1 i)
                                       (string-ref s2 i))
                               0
                               1)))))))))

;;; The search itself. Basic breadth-first algorithm.
;;; Since we check for cycles, we don't expand a word multiple times.
(define (search-path words from to)
  (let loop ((agenda (list (list from)))
             (done '()))
    (cond
     ((null? agenda)
      #f)
     ((string=? (caar agenda)
                to)
      (reverse (car agenda)))
     ((memq (caar agenda)
            done)
      (loop (cdr agenda)
            done))
     (else
      (loop (append (cdr agenda)
                    (expand (caar agenda)
                            (cdar agenda)
                            words))
            (cons (caar agenda)
                  done))))))

;;; Dumb, bad, and horribly inefficient word expansion.
;;; Could be improved considerably if the words were indexed by length.
(define (expand word history words)
  (map (lambda (child)
         (cons child (cons word history)))
       (filter (lambda (candidate)
                 (roughly-equal? word candidate))
               words)))

;;; Documentation left as an exercise for the reader.
(define (main wordlist from to)
  (write (search-path (read-words (open-input-file wordlist))
                      from
                      to))
  (newline))

(apply main (cdr (command-line)))
