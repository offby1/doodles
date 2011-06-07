#lang racket

(define (call-wif-input-fyle filename consumer)
  (let ([inp (open-input-file filename)])
    (define (cleanup)
      (close-input-port inp)
      (fprintf (current-error-port)
               "Closed ~a~%" inp))
    (with-handlers ([exn? (lambda (e)
                            (cleanup)
                            (raise e))])
      (consumer inp))
    (cleanup)))

(call-wif-input-fyle "/etc/passwd"
               (lambda (inp)
                 (for ([(l i) (in-indexed (in-lines inp))])
                   (printf "A line from ~a: ~s~%" inp l)
                   (when (= i 10)
                     (error "Oh shit, it's the dreaded tenth line!")))))

