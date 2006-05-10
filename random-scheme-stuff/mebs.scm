(module mebs
mzscheme

(require (lib "contract.ss"))

(provide/contract [modular-exponent-by-successive-squares
                   (->d
                    number?
                    (and/c exact? positive? integer?)
                    (and/c exact? integer?)
                    (lambda (base exp mod)
                      ;; this looks rather silly -- we define a
                      ;; variable and then use it, once, immediately
                      ;; thereafter.  That's so that if the contract
                      ;; breaks, the error message can include the
                      ;; name of the function that detected the error.
                      (define less-than-modulus
                        (lambda (result)
                          (and (number? result)
                               (< result mod))))
                      less-than-modulus)
                    )])

(define (modular-exponent-by-successive-squares base exponent modulus)
  (define (->m x) (modulo x modulus))
  (let loop ((exponent exponent)
             (accumulator 1)
             (current-factor base))
    (cond
     ((= 1 exponent)
      (->m (* current-factor accumulator)))
     ((odd? exponent)
      (loop (- exponent 1)
            (->m (* current-factor accumulator ))
            current-factor))
     (#t
      (loop (/ exponent 2)
            accumulator
            (->m (* current-factor current-factor))))))))

