;; The sieve of Eratosthenes

(let loop ((candidate 2)
           (known-primes '()))

  (define (first-factor-in numbers)
    (if (null? numbers)
        #f
      (if (zero? (remainder candidate (car numbers)))
          (car numbers)
        (first-factor-in (cdr numbers)))))

  (if (< (length known-primes) 10)
      (loop (+ 1 candidate)
            (if 
                                        ; this is less than ideal --
                                        ; we're searching through the
                                        ; known primes, from
                                        ; largest to smallest.  But
                                        ; it'd almost surely be
                                        ; faster to search in the
                                        ; other direction.
                (first-factor-in known-primes)
                
                known-primes
              (cons candidate known-primes)))
    (reverse known-primes)))
