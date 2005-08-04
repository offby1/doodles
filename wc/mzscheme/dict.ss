#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module dict mzscheme
  (require
   (lib "file.ss")
   (lib "trace.ss")
   "persist.ss")

  (provide all-neighbors)

  (define *dictionary-file-name*
    (let ((t (get-preference 'anagrams-dictionary-file-name)))
      (or (and t (bytes->path t))
          (bytes->path "/usr/share/dict/words"))))
  
  (define-persistent *words-by-length* "word-list.dat"
    (let ((tmp  (make-hash-table)))
      (define (note word)
        (let* ((l (string-length word))
               (set (hash-table-get tmp l (lambda () (make-hash-table 'equal)))))
          (hash-table-put! set word #t)
          (hash-table-put! tmp l set)))
      (with-input-from-file *dictionary-file-name*
        (lambda ()
          ;; each entry is now a 'set' of words of the same length.
          ;; convert the set (really another hash table) to a simple list.
          (let loop ((word  (read-line)))
            (if (eof-object? word)
                (begin
                  (hash-table-for-each
                   tmp
                   (lambda (length set)
                     (let ((words (hash-table-map set (lambda (k v) k))))
                       (hash-table-put! tmp length words))))
                  tmp)
              (begin
                (note word)
                (loop (read-line)))))))))

  (define (words-of-length n)
    (hash-table-get *words-by-length* n))

  (define (neighbors? w1 w2)
    (let loop ((chars-to-examine (string-length w1))
               (differences-found 0))
      (if (or (< 1 differences-found)
              (zero? chars-to-examine))
          (= 1 differences-found)
        (let ((l1 (string-ref w1 (sub1 chars-to-examine)))
              (l2 (string-ref w2 (sub1 chars-to-examine))))
          (loop (sub1 chars-to-examine)
                (+ differences-found (if (char=? l1 l2) 0 1)))))))

  (define (all-neighbors w)
    (let loop ((dict (words-of-length (string-length w)))
               (result '()))
      (if (null? dict)
          result
        (loop (cdr dict)
              (if (neighbors? w (car dict))
                  (cons (car dict)
                        result)
                result)))))

  (fprintf (current-error-port)
           "~a"
           (map (lambda (word)
                  (format
                   "all neighbors of ~s: ~a~n"
                   word
                   (all-neighbors word)))
                (list "giant" "kate" "katie" "buster" "inconceivable")
                ))
  
  )
