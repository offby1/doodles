;; see http://www.rsasecurity.com/rsalabs/faq/3-1-1.html

(require 'primes)
(require 'random)
(require 'modular)

(define make-rsa-system #f)

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

  ;; Like the above, but disallows 1 and n - 1 as well, since those
  ;; numbers seem to encrypt predictably -- 1 => 1 and n - 1 => n - 2

  (define (strict-check rsa-system n)
    (if (or (not (integer? n))
            (not (> n 1))
            (not (> (rsa-system:n rsa-system) n)))
        (error "You may encrypt or decrypt only integers greater than "
               "one and less than "
               (- (rsa-system:n rsa-system) 1))))

  ;; return a public exponent, private exponent, and modulus with
  ;; which you can do RSA public-key encryption and decryption.
  ;; SMALLEST-PRIME determines how secure the returned system is --
  ;; bigger is more secure.  SMALLEST-PRIME doesn't actually have to
  ;; be prime.

  (define rsa-system 
        (lambda (smallest-prime)

          ;; This function is very slow when SMALLEST-PRIME is large.
          (define (random-big-prime)
            (car (primes> (random (- smallest-prime 1)) 1)))

          (define (invoke-verbosely thunk message)
            (display message)
            (let ((return (thunk)))
              (display "done")
              (newline)
              return))
          
          (let* ((p (invoke-verbosely random-big-prime "Finding first big prime..."))
                 (q (invoke-verbosely random-big-prime "Finding second big prime..."))
                 (phi (* (- q 1)
                         (- p 1)))

                 ;; a random number less than pq and relatively prime
                 ;; to phi. 
                 (e (invoke-verbosely 
                     (lambda ()
                       (let ((small-random 
                              (lambda () (random (* p q))))) 

                         (let loop ((guess (small-random)))
                           (if (= 1 (gcd guess phi))
                               guess
                             (loop (small-random))))))
                     "Finding e..."))
         
                 (d (modular:invert phi e)))

            (vector d e (* p q)))))

  (define encrypt
        (lambda (r plaintext)
          (strict-check r plaintext)
          (modular:expt (rsa-system:n r)
                        plaintext
                        (rsa-system:e r))))

  (define decrypt 
        (lambda (r ciphertext)
          (strict-check r ciphertext)
          (modular:expt (rsa-system:n r)
                        ciphertext
                        (rsa-system:d r))))

  (set! make-rsa-system
    (lambda (size)
      (let ((system (rsa-system size)))
        (lambda (action argument)
          (cond
            ((eq? action 'encrypt) 
             (encrypt system argument))

            ((eq? action 'decrypt)
             (decrypt system argument))

            ((eq? action 'size)
             (rsa-system:n system))
            (#t
             (error "I don't understand this: " action))))))))

;; Usage:
;; (define r (make-rsa-system (expt 2 64))) => undefined
;; (r 'encrypt 1234567)                     => some-big-number
;; (r 'decrypt some-big-number)             => 1234567
;;
;; or if you prefer to encrypt and decrypt strings:
;;
;; (convert (r 'decrypt (convert (convert (r 'encrypt (convert "bob"))))))
;;                                          => "bob"

(define convert #f)

(let ((char-set-size 256))
  
  (define (integer->string n)
    (let loop ((n n)
               (chars '()))
      (if (< 0 n)
          (loop (quotient n char-set-size)
                (cons (integer->char (remainder n char-set-size))
                      chars))
        (list->string chars))))

  (define (string->integer s)
    (let loop ((return 0)
               (chars-processed 0)
               (chars-to-process (string-length s)))
      (if (< 0 chars-to-process)
          (loop (+ (* return char-set-size)  (char->integer (string-ref s chars-processed)))
                (+ 1 chars-processed)
                (- chars-to-process 1))
        return)))
                    
  (set! convert
        (lambda (arg)
          (cond
           ((string? arg)
            (string->integer arg))
           ((integer? arg)
            (integer->string arg))
           (#t
            (error "I only know how to convert strings and integers."))))))