(require 'random)
(require 'sort)
(require 'filter)

(load "my-chars.scm")

(define (jefferson-wheel-create number-of-discs)

  (define the-discs
    (let ()
      (define all-chars
        (let loop ((chars '()))
          (if (= (length chars) char-set-size)
              (reverse chars)
            (loop (cons (integer->my-char (length chars))
                        chars)))))
      (let loop ((discs '()))

        (define (make-disk)

          (define random-char-ordering
            (let ()

              (define (shuffle-list l)
                (map
                 car
                 (sort
                  (map
                   (lambda (entry)
                     (cons entry (random most-positive-fixnum)))
                   l)
                  (lambda (entry1 entry2)
                    (< (cdr entry1)
                       (cdr entry2))))))

              (lambda () (shuffle-list all-chars))))

          (define (advance-char ch offset ordering)
            (let* ((index (- (length ordering)
                             (length (memq ch ordering))))
                   (new-index (remainder (+ index offset)
                                         (length ordering))))
              (list-ref ordering new-index)))

          (let ((ordering (random-char-ordering)))
            (lambda (ch offset)
              (advance-char ch offset ordering))))

        (if (= (length discs)
               number-of-discs)
            discs
          (loop (cons (make-disk)
                      discs))))))

  (lambda (cleartext)
    (let loop ((offset 0)
               (results '()))
      (if (= offset (- char-set-size 1))
          (reverse results)
        (loop (+ offset 1)
              (cons
               (let* ((filtered-cleartext (list->string (filter my-char? (map
                                                                          char-downcase (string->list cleartext)))) )
                      (ciphertext (make-string (string-length filtered-cleartext))))
                 
                 (let loop ((chars-encrypted 0))
                   (if (= chars-encrypted (string-length filtered-cleartext))
                       ciphertext
                     (begin
                       (string-set! ciphertext
                                    chars-encrypted 
                                    ((list-ref the-discs (remainder chars-encrypted (length the-discs)))
                                     (string-ref filtered-cleartext chars-encrypted)
                                     offset))
                       (loop (+ 1 chars-encrypted))))))
               results))))))

(define (random-elt seq)
  (list-ref seq (random (length seq))))

(define (encrypt str j)
  (random-elt (j str)))
