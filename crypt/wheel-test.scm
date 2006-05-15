(module wheel-test mzscheme
(require (only (lib "1.ss" "srfi") drop every filter iota take)
         (only (lib "13.ss" "srfi") string-filter)
         "spindle.scm")

(print-struct #t)

(define *wheels-per-spindle* 10)

(define (split-into-short-strings input max-length)
  (let loop ((input (string-downcase (string-filter char-alphabetic? input)))
             (output '()))
    (if (zero? (string-length input))
        (reverse output)
      (let* ((input-length (string-length input))
             (left (min input-length max-length))
             (right (- input-length left)))
        (loop (string-take-right input right)
              (cons (string-take input left) output))))))

(let ((s (make-spindle *wheels-per-spindle*)))
  (let ((encrypted
         (map (lambda (str)
                (rotate-to-display! s str)
                (string-at-offset s
                                  (random *alphabet-length*)
                                  ))
              (split-into-short-strings  "A pleasantly long text which we shall encrypt" *wheels-per-spindle*))))
    (printf "Long encryption: ~s~%"  encrypted)
    (for-each (lambda (ciphertext)
                (rotate-to-display! s ciphertext)
                (printf "Possible cleartexts: ~s~%"
                        (map (lambda (i)
                               (string-at-offset s i))
                             (iota *alphabet-length*))))
              encrypted)))

)
