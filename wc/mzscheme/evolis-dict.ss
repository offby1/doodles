(module dict mzscheme
  (require
   (lib "file.ss")
   (only (lib "1.ss" "srfi") filter iota)
   (only (lib "13.ss" "srfi") string-downcase)
   "set.ss"
   "persist.ss")

  (provide all-neighbors
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

  (define (same-sex? c1 c2)

    (define (is-vowel? c)
      (memq c (list #\a #\e #\i #\o #\u)))
  
    (define (same a b)
      (eq? (not (not a))
           (not (not b))))

    (same (is-vowel? c1)
          (is-vowel? c2)))

  (define (25-varieties word index)
    (let ((original-letter (string-ref word index)))
      (let 25-varieties-loop ((letters-to-examine (vector-length *the-alphabet*))
                              (same-sex-results '())
                              (opposite-sex-results '()))
        (if (zero? letters-to-examine  )
            (values same-sex-results
                    opposite-sex-results)
          (let ((this-letter (vector-ref *the-alphabet* (sub1 letters-to-examine))))

            (if (char=? this-letter original-letter)

                ;; don't return the string we were passed in.
                (25-varieties-loop (- letters-to-examine 1)
                                   same-sex-results
                                   opposite-sex-results)

              (let ((new (string-copy word)))
                (string-set! new index this-letter)
                (if (same-sex? original-letter this-letter)
                    (25-varieties-loop (- letters-to-examine 1)
                                       (cons new same-sex-results)
                                       opposite-sex-results)
                  (25-varieties-loop (- letters-to-examine 1)
                                     same-sex-results
                                     (cons new opposite-sex-results))))))))))

  ;; one-letter variants
  (define (olvs word)
    (let olvs-loop ((letters-to-examine (string-length word))
                    (same-sex-result '())
                    (opposite-sex-result '()))
      (if (zero? letters-to-examine)
          (append same-sex-result opposite-sex-result)
        (let-values (((same oppo)
                      (25-varieties word (sub1 letters-to-examine))))
          (olvs-loop (sub1 letters-to-examine)
                     (append same same-sex-result)
                     (append oppo opposite-sex-result))))))

  (define (all-neighbors word)
    (let ((same-size (hash-table-get *words-by-length* (string-length word))))
      (filter (lambda (n) (is-present? n same-size)) (olvs word))))

  (define (random-word-pair wlength)
    (let* ((words (list->vector (hash-table-map (hash-table-get *words-by-length* wlength) (lambda (k v) k))))
           (length (vector-length words)))
      (list (vector-ref words (random length))
            (vector-ref words (random length)))))
  
  )
