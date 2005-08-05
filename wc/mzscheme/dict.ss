#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module dict mzscheme
  (require
   (lib "file.ss")
   (lib "trace.ss")
   (only (lib "1.ss" "srfi") filter iota)
   (only (lib "13.ss" "srfi") string-downcase)
   "set.ss"
   "persist.ss")

  (provide all-neighbors)

  (define *dictionary-file-name*
    (let ((t (get-preference 'anagrams-dictionary-file-name)))
      (or (and t (bytes->path t))
          (string->path "/usr/share/dict/words"))))

  (define-persistent *words-by-length* "word-list.dat"
    (let ((w-b-l  (make-hash-table)))
      (define (note word)
        (let* ((l (string-length word))
               (same-length-words (hash-table-get w-b-l l (lambda () (set)))))
          (add! (string-downcase word)
                same-length-words)
          (hash-table-put! w-b-l l same-length-words)))
      (with-input-from-file *dictionary-file-name*
        (lambda ()
          ;; each entry is now a 'set' of words of the same length.
          ;; convert the set (really another hash table) to a simple list.
          (let read-word-loop ((word  (read-line)))
            (if (eof-object? word)
                w-b-l
              (begin
                (note word)
                (read-word-loop (read-line)))))))))

  (define (words-of-length n)
    (hash-table-get *words-by-length* n))

  (define *the-alphabet*
    (list->vector
     (map integer->char
          (iota (- (char->integer #\z)
                   (char->integer #\a)
                   -1)
                (char->integer #\a)
                ))))

  (define (25-varieties word n)
    (let 25-varieties-loop ((letters-to-examine (vector-length *the-alphabet*))
                            (result '()))
      (if (zero? letters-to-examine  )
          result
        (let ((this-letter (vector-ref *the-alphabet* (sub1 letters-to-examine))))
          (if (char=? this-letter (string-ref word n))
              (25-varieties-loop (- letters-to-examine 1)
                                 result)
            (let ((new (string-copy word)))
              (string-set! new n this-letter)
              (25-varieties-loop (- letters-to-examine 1)
                                 (cons new
                                       result))))))))

  (define (olvs word)
    (let olvs-loop ((letters-to-examine (string-length word))
                    (result '()))
      (if (zero? letters-to-examine)
          result
        (olvs-loop (sub1 letters-to-examine)
                   (append (25-varieties word (sub1 letters-to-examine))
                           result)))))

  (define (all-neighbors word)
    (let ((same-size (hash-table-get *words-by-length* (string-length word))))
      (filter (lambda (n) (is-present? n same-size)) (olvs word))))

  ;;(trace all-neighbors)

  )
