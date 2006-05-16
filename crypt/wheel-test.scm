(module wheel-test mzscheme
(require (only (lib "1.ss" "srfi")
               drop
               every
               filter
               iota
               take)
         (only (lib "13.ss" "srfi")
               string-filter
               string-take
               string-take-right)
         "spindle.scm")

(print-struct #t)

(define *wheels-per-spindle* 30)

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

(define (counted-string s)
  (format "~a:~s"(string-length s) s))

(let ((s (make-spindle *wheels-per-spindle*)))
  (let* ((plain   "A pleasantly long text which we shall encrypt")
         (encrypted
          (map (lambda (str)
                 (rotate-to-display! s str)
                 (printf "Encrypting:~%~a~%" (tableaux s))
                 (string-at-offset
                  s
                  (random *alphabet-length*)))
               (split-into-short-strings plain *wheels-per-spindle*))))
    (printf "~a~%=> Long encryption: ~a ~%"
            (counted-string plain)
            (map counted-string encrypted)
            )
    (for-each (lambda (short-ciphertext)
                (rotate-to-display! s short-ciphertext)
                (printf "Possible cleartexts: ~%~a~%" (tableaux s)))
              encrypted)))

)
