(module wheel-test mzscheme
(require (only (lib "1.ss" "srfi") drop every filter iota take)
         "spindle.scm")

(print-struct #t)

(define *wheels-per-spindle* 10)

(define (split-into-lists input)
  (unless (and (list? input)
               (every char? input))
    (raise-type-error 'split-into-lists "list of chars" input))
  (let loop ((input input)
             (output '()))
    (cond
     ((null? input)
      (reverse output))
     ((< (length input) *wheels-per-spindle*)
      (reverse (cons input output)))
     (else
      (loop (drop input *wheels-per-spindle*)
            (cons (take input *wheels-per-spindle*)
                  output))))))

(define (lc-letters-only string)
  (map char-downcase (filter char-alphabetic? (string->list string))))

(let ((s (make-spindle *wheels-per-spindle*)))
  (let ((encrypted
         (map (lambda (letter-list)
                (rotate-to-display! s (list->string letter-list))
                (string-at-offset s
                                  (random *alphabet-length*)
                                  ))
              (split-into-lists (lc-letters-only "A pleasantly long text which we shall encrypt")))))
    (printf "Long encryption: ~s~%"  encrypted)
    (for-each (lambda (ciphertext)
                (rotate-to-display! s ciphertext)
                (printf "Possible cleartexts: ~s~%"
                        (map (lambda (i)
                               (string-at-offset s i))
                             (iota *alphabet-length*))))
              encrypted)))

)
