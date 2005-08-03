#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module dict mzscheme
  (require
   (lib "file.ss")
   (only (lib "13.ss" "srfi") string-downcase)
   "persist.ss"
   )
  (provide *word-hash*)

  (define *dictionary-file-name*
    (let ((t (get-preference 'anagrams-dictionary-file-name)))
      (or (and t (bytes->path t))
          (bytes->path "/usr/share/dict/words"))))
  
  (define (word-acceptable? w)
    (positive? (string-length w)))

  (define-persistent *word-list* "word-list.dat"
    (with-input-from-file *dictionary-file-name*
      (lambda ()
        (let loop ((word  (read-line))
                   (lines-read 0)
                   (result '()))
          (if (or (= lines-read 1000)
                  (eof-object? word))
              result
            (begin
              (set! word (string-downcase word))
              (loop (read-line)
                    (+ 1 lines-read)
                    (if (word-acceptable? word)
                        (cons word result)
                      result))))))))
  
  (define *word-hash*   (make-hash-table 'equal))
  (for-each (lambda (w)
              (hash-table-put! *word-hash* w #t)
              )
            *word-list*)
  (fprintf (current-error-port)
           "Hash hath ~a entries~n" (hash-table-count *word-hash*))
  )