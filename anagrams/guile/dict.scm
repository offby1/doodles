(define-module (dict))

(use-modules (ice-9 rdelim)
             (ice-9 pretty-print)
             (srfi srfi-1)
             (bag))

(define-public *dict* #f)

(define (adjoin-word dict word)
  (let* ((this-bag (bag word))
         (probe (hash-ref dict this-bag)))
    (cond
     ((not probe)
      (hash-set! dict this-bag (list word)))
     ((not (member word probe))
      (hash-set! dict this-bag (cons word probe)))
     )))

(define word-acceptable?
  (let ((has-vowel-regexp     (make-regexp "[aeiou]" regexp/icase))
        (has-non-ASCII-regexp (make-regexp "[^a-z]"  regexp/icase )))
    (lambda (word)
      (let ((l (string-length word)))
        (and (not (zero? l))

             ;; it's gotta have a vowel.
             (regexp-exec has-vowel-regexp word)

             ;; it's gotta be all ASCII, all the time.
             (not (regexp-exec has-non-ASCII-regexp word))
             
             ;; it's gotta be two letters long, unless it's `i' or `a'.
             (or (string=? "i" word)
                 (string=? "a" word)
                 (< 1 l)))))))

(define (wordlist->hash)
  (with-input-from-file
      "/usr/share/dict/words"
    (lambda ()
      (let ((dict (make-hash-table 10000)))
        (format #t "Reading dictionary ... ") (force-output)
        (let loop ((word  (read-delimited "\n")))
          (if (eof-object? word)
              (format #t "done: ~a~%" dict)
            (begin
              (if (word-acceptable? word)
                  (adjoin-word dict (string-downcase word)))
              (loop (read-delimited "\n")))
            ))
        dict)
      )))

(define *wordhash-name* "wordhash.scm")

(if (not (access? *wordhash-name* R_OK))
    (call-with-output-file *wordhash-name* 
      (lambda (port)
        ;; use `pretty-print' rather than `write' so that it's not all
        ;; on on big line.
        (pretty-print (hash-fold (lambda (key value prior)
                                   (cons (cons key value) prior)) 
                                 '()
                                 (wordlist->hash)) port))))

(define (bag-acceptable? this bag-to-meet)
  (and (or (bags=? bag-to-meet this)
           (subtract-bags bag-to-meet this))
       this))

(define-public (init bag-to-meet)
  (set! *dict* (make-hash-table 10000))
  (format #t "Pruning dictionary ... ") (force-output)
  (for-each
   (lambda (pair)
     (let ((bag (car pair))
           (words (cdr pair)))
       (let ((this-bag (bag-acceptable? bag bag-to-meet)))
         (if this-bag
             (hash-set! *dict* bag words)))))
   (with-input-from-file *wordhash-name* read))
  (format #t "done~%"))

(define-public (test-init)
  (set! *dict* (make-hash-table 26))
  (for-each (lambda (word)
              (hash-set!
               *dict*
               (bag word)
               (list word)))
            (map (lambda (i)
               (string (integer->char (+ i (char->integer #\a))))) (iota 26))))