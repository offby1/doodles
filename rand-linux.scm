;; Generate truly random (i.e., cryptographically strong) numbers, by
;; using Linux's `/dev/random' device.  See `random (4)'.

;; Returns LENGTH random characters.

;; If STRONG is #f, generate cryptographically weak random numbers by
;; reading "/dev/urandom".  Otherwise, generate cryptographically
;; strong random numbers by reading "/dev/random".

;; If STRONG is not #f, might not return for a while, depending on how
;; much entropy is in the "entropy pool".  If it hangs, wiggle the
;; mouse, or hit the shift key a few times; that will probably give it
;; enough entropy to complete.

(define (random-character-stream length strong)
  (with-input-from-file (if strong
                            "/dev/random"
                          "/dev/urandom")
    (lambda ()
      (let ((so-far (make-string length)))
        (let loop ((char (read-char))
                   (chars-read 0))
          (if (= chars-read length)
              so-far
            (begin
              (string-set! so-far chars-read char)
              (loop (read-char)
                    (+ 1 chars-read)))))))))

;; Generates a random integer by converting a random string.  It's
;; quite inefficient -- each character in the string only contributes
;; one bit to the random integer.  It might seem more reasonable for
;; each character to contribute, say, eight bits, but that would
;; require that I know the number of bits in a character, and in
;; general, I don't know that.  Of course there are seven bits in each
;; ASCII character, but how do I know that the character set in use is
;; ASCII?

;; This also assumes that half the characters in the character set are
;; "odd", in the sense of `(odd? (char->integer c))'.  This of course
;; is true for ASCII, and is probably true for other character sets,
;; but is not guaranteed.  If in fact half the characters are *not*
;; odd, then the numbers will still be random -- i.e., unpredictable
;; -- but they will not be uniformly distributed.

(define random-integer
  (lambda (bits)
    (define (char->digit c)
      (if (odd? (char->integer c))
          #\1
        #\0))
    (string->number
     (list->string
      (map
       char->digit
       (string->list
        (random-character-stream bits #f))))
     2)))

;; 0 <= (random-float) < 1, uniformly distributed
(define random-float
  (let ((two-to-the-128 (expt 2 128)))
    (lambda ()
      (/ (random-integer 128)
         two-to-the-128))))

(define normal-float
  (let ((two-pi (* 4 (acos 0))))
    (lambda ()
      (let ((r (sqrt (* -2 (log (random-float)))))
            (t (* two-pi (random-float))))
        (* r (cos t))))))

(define (call-repeatedly thunk n)
  (let loop ((so-far '())
             (length 0))
    (if (= length n)
        so-far
      (loop (cons (thunk)
                  so-far)
            (+ 1 length)))))

(define (read-whats-available filename)
  (with-input-from-file filename
    (lambda ()
      (define (my-append str used ch)
        (define (larger str)
          (string-append str (make-string (+ 1 (string-length str)))))
        (if (= used (string-length str))
            (set! str (larger str)))
        (string-set! str used ch)
        str)
      (let loop ((result "")
                 (chars 0))
        (if (not (char-ready?))
            (make-shared-substring result 0 chars)
          (loop (my-append result chars (read-char))
                (+ 1 chars)))))))