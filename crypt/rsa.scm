;; see http://www.rsasecurity.com/rsalabs/faq/3-1-1.html

(require 'primes)
(require 'random)
(require 'modular)

(define rsa-system #f)
(define encrypt    #f)
(define decrypt    #f)
(define modulus    #f)

(let ()
  
  ;; returns the exponent part of the private key
  (define (rsa-system:d r) (vector-ref r 0))
  
  ;; returns the exponent part of the public key
  (define (rsa-system:e r) (vector-ref r 1))
  
  ;; returns the modulus part of the keys
  (define (rsa-system:n r) (vector-ref r 2))

  ;; bitch if user tries to encrypt or decrypt a number that's too
  ;; big, or not a positive integer
  (define (check-size rsa-system n)
    (if (or (not (integer? n))
            (not (positive? n))
            (>= n (rsa-system:n rsa-system)))
        (error "You may encrypt or decrypt only positive integers less than"
               (rsa-system:n rsa-system))))

  ;; return a public exponent, private exponent, and modulus with
  ;; which you can do RSA public-key encryption and decryption.
  ;; SMALLEST-PRIME determines how secure the returned system is --
  ;; bigger is more secure.  SMALLEST-PRIME doesn't actually have to
  ;; be prime.

  (set! rsa-system 
        (lambda (smallest-prime)

          ;; This function is very slow when SMALLEST-PRIME is large.
          (define (random-big-prime)
            (car (primes> (random (- smallest-prime 1)) 1)))

          ;; (define (random-small-prime too-big)
          ;;   (let* ((all-small-primes (primes< too-big too-big))
          ;;          (how-many (length all-small-primes)))
          ;;     (list-ref all-small-primes (random how-many))))

          (let* ((p (random-big-prime))
                 (q (random-big-prime))
                 (phi (* (- q 1)
                         (- p 1)))

                 ;; a small random number relatively prime to phi.
                 (e (let ((small-random 
                           (lambda () (random (* p q))))) 

                      (let loop ((guess (small-random)))
                        (if (= 1 (gcd guess phi))
                            guess
                          (loop (small-random))))))
         
                 (d (modular:invert phi e)))
            (display "is (ed - 1) divisible by (p - 1) (q - 1)? ")
            (display (if (zero? (remainder
                                 (- (* e d)
                                    1)
                                 (* (- p 1)
                                    (- q 1))))
                         "yes" "no"))
            (newline)
            (for-each display
                      (list "p:   " p   #\newline
                            "q:   " q   #\newline
                            "phi: " phi #\newline
                            "e:   " e   #\newline
                            "d:   " d   #\newline))
            (vector d e (* p q)))))

  (set! encrypt
        (lambda (r plaintext)
          (check-size r plaintext)
          (modular:expt (rsa-system:n r)
                        plaintext
                        (rsa-system:e r))))

  (set! decrypt 
        (lambda (r ciphertext)
          (check-size r ciphertext)
          (modular:expt (rsa-system:n r)
                        ciphertext
                        (rsa-system:d r))))

  (set! modulus rsa-system:n))
