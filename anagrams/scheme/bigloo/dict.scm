(module dict
    (import (bag "bag.scm"))
  (export dictionary-for)
  )

(define word-acceptable?
  (let ((has-vowel-regexp (pregexp "[aeiou]"))
        (has-non-ASCII-regexp (pregexp "[^a-z]")))
    (lambda (word)
      (let ((l (string-length word)))
        (and (not (zero? l))

             ;; it's gotta have a vowel.
             (pregexp-match has-vowel-regexp word)

             ;; it's gotta be all ASCII, all the time.
             (not (pregexp-match has-non-ASCII-regexp word))

             ;; it's gotta be two letters long, unless it's `i' or `a'.
             (or (string=? "i" word)
                 (string=? "a" word)
                 (< 1 l)))))))

;; helper functions to work around my inability to use alists as keys
;; in bigloo's hash tables
(define (bag->string b)
  (apply string-append (map (lambda (p) (make-string (cdr p)
                                                     (car p))) b)))

(define (string->bag s)

  (define (merge-into-alist pair alist)
    (define (increment-cdr! p delta)
      (set-cdr! p (+ delta (cdr p))))
    (cond
     ((null? alist)
      (list pair))

     ((char>? (car pair)
              (caar alist))
      (error
       "merge-into-alist"
       "pair's key is lexicographically greater than key of alist's first entry"
       (format "pair: ~s; alist: ~s" pair alist)))

     ((char=? (car pair)
              (caar alist))
      (increment-cdr! (car alist) (cdr pair))
      alist)

     (else
      (cons pair alist))))

  (define (internal s)
    (cond
     ((null? s)
      '())
     ((null? (cdr s))
      (list (cons (car s) 1)))
     (else
      (merge-into-alist (cons (car s) 1)
                        (internal (cdr s))))))

   (internal (string->list s)))


(define *the-dictionary* #f)
(define *dict-cache-file-name* "cached-dictionary.scm")
(if (file-exists? *dict-cache-file-name*)
    (begin
      (display "Reading ")(write *dict-cache-file-name*) (display " ... ")
      (set! *the-dictionary* (with-input-from-file *dict-cache-file-name* read))
      (display (length *the-dictionary*))
      (display " entries")
      (newline))

  ;; BUGBUG -- this hash table, despite what the docs say, isn't using
  ;; "equal?" to compare keys.  Unfortunately I don't know how to fix
  ;; it with 2.6e.  Perhaps a newer build of bigloo will work better.
  (let ((ht  (make-hashtable)))
    (call-with-input-file
        "words"
      (lambda (p)
        (display "Reading dictionary ... ")
        (newline)
        (let loop ((words-read 0))
          (let ((word  (read-line p)))
            (when (not (eof-object? word))
              (let ((word (string-downcase word)))
                (when (zero? (remainder words-read 1000))
                  (display "Read ")
                  (display words-read)
                  (display " words ...")
                  (newline))
                (if (word-acceptable? word)
                    (let* ((key  (bag->string (bag word)))
                           (prev (or (hashtable-get ht key) '())))
                      (set! prev (cons word prev))
                      (hashtable-put! ht key prev)))
                (loop (+ 1 words-read))))))))
    (display "done") (newline)
    (set! *the-dictionary*
          ;; put longest words first.
          (sort
           (hashtable-map ht (lambda (k v)
                               (cons (string->bag k)
                                     v)))
           (lambda (e1 e2)
             (> (string-length (cadr e1))
                (string-length (cadr e2)))))
          )
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
