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


                      ;; It might also be nice to assert that the
                      ;; result is at least as inexact as the base.
                      ;; Too lazy to do that, though.
                      (define positive-but-less-than-modulus
                        (lambda (result)
                          (and (number? result)
                               (positive? result)
                               (< result mod))))
                      positive-but-less-than-modulus)
                    )])

(define (modular-exponent-by-successive-squares base exponent modulus)
  (define (->m x) (modulo x modulus))
  (let loop ((exponent exponent)
             (accumulator 1)
             (current-factor base))
    (define (*cf x) (->m (* current-factor x)))
    (cond
     ((= 1 exponent)
      (*cf accumulator))
     ((odd? exponent)
      (loop (- exponent 1)
            (*cf accumulator )
            current-factor))
     (#t
      (loop (/ exponent 2)
            accumulator
            (*cf current-factor)))))))

