;;   -*- mode: scheme48; scheme48-package: wc -*-
(define (iota how-many start)
    (let loop ((how-many how-many)
               (result '()))
      (if (positive? how-many)
          (loop (- how-many 1)
                (cons (+ how-many start -1)
                      result))
        result)))

(define *the-alphabet*
  (list->vector
   (map integer->char
        (iota (- (char->integer #\z)
                 (- (char->integer #\a)
                    1))
              (char->integer #\a)
              ))))

(define (twenty-five-varieties word index)
  (let twenty-five-varieties-loop ((letters-to-examine (vector-length *the-alphabet*))
                          (result '()))
    (if (zero? letters-to-examine  )
        result
      (let ((this-letter (vector-ref *the-alphabet* (- letters-to-examine 1))))

        (if (char=? this-letter (string-ref word index))

            ;; don't return the string we were passed in.
            (twenty-five-varieties-loop (- letters-to-examine 1)
                               result)

          (let ((new (string-copy word)))
            (string-set! new index this-letter)
            (twenty-five-varieties-loop (- letters-to-examine 1)
                               (cons new result))))))))

(define (olvs word)
  (let olvs-loop ((letters-to-examine (string-length word))
                  (result '()))
    (if (zero? letters-to-examine)
        result
      (olvs-loop (- letters-to-examine 1)
                 (append (twenty-five-varieties word (- letters-to-examine 1))
                         result)))))

(define (all-neighbors word)
   (filter (lambda (n)
             (table-ref (table-ref *the-hash-table*
                                   (string-length word))
                        n))
           (olvs word)))

(define (wc words)
  (for-each (lambda (word)
              (display "All neighbors of ")
              (write word)
              (display ": ")
              (write (all-neighbors word))
              (newline))
            words))
