(module dict mzscheme
  (require
   (lib "file.ss")
   (only (lib "1.ss" "srfi") filter iota)
   (planet "set.ss"  ("offby1" "offby1.plt"))
   "persist.ss")

  (provide all-neighbors
           with-neato-output
           random-word-pair)

  (define *dictionary-file-name*
    (let ((t (get-preference 'anagrams-dictionary-file-name)))
      (or (and t (bytes->path t))
          (string->path "/usr/share/dict/words"))))

  (define-persistent *words-by-length* "word-list.dat"
    (let ((w-b-l  (make-hash-table)))
      (define (note word)
        (let* ((l (string-length word))
               (same-length-words (hash-table-get w-b-l l (lambda () (make-set)))))
          (add! (string-downcase word)
                same-length-words)
          (hash-table-put! w-b-l l same-length-words)))
      (with-input-from-file *dictionary-file-name*
        (lambda ()
          (let read-word-loop ((word  (read-line)))
            (if (eof-object? word)
                w-b-l
              (begin
                (note word)
                (read-word-loop (read-line)))))))))

  (define *the-alphabet*
    (list->vector
     (map integer->char
          (iota (- (char->integer #\z)
                   (char->integer #\a)
                   -1)
                (char->integer #\a)
                ))))

  (define (25-varieties word index)
    (let 25-varieties-loop ((letters-to-examine (vector-length *the-alphabet*))
                            (result '()))
      (if (zero? letters-to-examine  )
          result
        (let ((this-letter (vector-ref *the-alphabet* (sub1 letters-to-examine))))

          (if (char=? this-letter (string-ref word index))

              ;; don't return the string we were passed in.
              (25-varieties-loop (- letters-to-examine 1)
                                 result)

            (let ((new (string-copy word)))
              (string-set! new index this-letter)
              (25-varieties-loop (- letters-to-examine 1)
                                 (cons new result))))))))

  ;; one-letter variants
  (define (olvs word)
    (let olvs-loop ((letters-to-examine (string-length word))
                    (result '()))
      (if (zero? letters-to-examine)
          result
        (olvs-loop (sub1 letters-to-examine)
                   (append (25-varieties word (sub1 letters-to-examine))
                           result)))))

  (define *graphis-output-port* (make-parameter #f (lambda (v)
                                                     (when (not (output-port? v))
                                                       (raise-type-error '*graphis-output-port* "output-port?" v))
                                                     v)))

  (define (all-neighbors word)
    (let ((rv (filter (lambda (n) (is-present? n  (hash-table-get *words-by-length* (string-length word)))) (olvs word))))
      (when (*graphis-output-port*)
        (for-each (lambda (n)
                    (fprintf (*graphis-output-port*) "~s -- ~s;~n" word n))
                  rv))
      rv))

  (define (random-word-pair wlength)
    (let* ((words (list->vector (hash-table-map (hash-table-get *words-by-length* wlength) (lambda (k v) k))))
           (length (vector-length words)))
      (list (vector-ref words (random length))
            (vector-ref words (random length)))))

  (define (with-neato-output thunk)
    (parameterize ((*graphis-output-port*
                    (open-output-file "network.dot" 'truncate/replace)))
      (fprintf (*graphis-output-port*) "Graph fred{~nsize=\"30,30\"~n")
      (begin0
        (thunk)
        (fprintf (*graphis-output-port*) "}~n")
        (close-output-port (*graphis-output-port*)))))

  )
