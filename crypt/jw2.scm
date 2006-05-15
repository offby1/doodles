(require (only (lib "1.ss" "srfi") filter iota take drop)
         (lib "trace.ss"))

;; #\A => 0
;; #\a => 0
;; #\z => 25 etc
(define (letter->integer c)
  (unless (char-alphabetic? c)
    (raise-type-error 'letter->integer "alphabetic" c))
  (- (char->integer (char-downcase c)) (char->integer #\a)))

;; inverse of the lower-case part of the above
(define (integer->letter i)
  (unless (and (exact? i)
               (not (negative? i))
               (< i *alphabet-length*))
    (raise-type-error 'integer->letter (format "exact, non-negative integer less than ~a" *alphabet-length*) i))
  (integer->char (+ i (char->integer #\a))))

(define *alphabet-length* 26)

;; shuffles in-place.  Destructive.  Got it?
(define (fisher-yates-shuffle! v)
  (define (swap! i1 i2)
    (let ((tmp (vector-ref v i1)))
      (vector-set! v i1 (vector-ref v i2))
      (vector-set! v i2 tmp)))
  (let ((l (vector-length v)))
    (do ((top-index (sub1 l) (sub1 top-index)))
        ((zero? top-index) v)
      (let ((bottom-index (random top-index)))
        (swap! bottom-index top-index))))
  v)

(define (find-index object vector)
  (let loop ((slots-to-examine (vector-length vector)))
    (if (positive? slots-to-examine)
        (let* ((this-index (- slots-to-examine 1))
               (this-item (vector-ref vector this-index)))
          (if (eq? this-item object)
              this-index
            (loop (- slots-to-examine 1))))
      (error "Couldn't find" object 'in vector))))

(define (make-one-wheel)
  (let ((permutation (
                      ;;values
                      fisher-yates-shuffle!
                      (list->vector (iota *alphabet-length*)))))
    (cons
     ;; encrypt
     (lambda (c offset)
       (let ((result (integer->letter (vector-ref permutation (modulo (+ offset (letter->integer c)) *alphabet-length*)))))

         result))
     ;; decrypt
     (lambda (c offset)
       (let* ((index (find-index (letter->integer c) permutation))
              (result (integer->letter (modulo (- index offset) *alphabet-length*))))

         result)))))

(define (lc-letters-only string)
  (map char-downcase (filter char-alphabetic? (string->list string))))

(define (string->integers string)
  (map letter->integer (lc-letters-only string)))

(define (random-offset)
   (random *alphabet-length*))

(define (general-purpose input-string spindle encrypting? offset)
  (let ((original-spindle spindle))
    (define (loop letters spindle offset result)
      (cond
       ((null? letters)
        (list->string (reverse result)))
       ((null? spindle)
        (loop letters
              original-spindle
              (random-offset)
              result))
       (else
        (let ((this-wheel (car spindle)))
          (loop (cdr letters)
                (cdr spindle)
                offset
                (cons ((
                        (if encrypting? car cdr)
                        (car spindle)) (car letters) offset) result))))))

    (loop
     (lc-letters-only input-string)
     original-spindle
     offset
     '())))

(define (split-into-lists-of-size size input)
  (let loop ((input input)
             (output '()))
    (cond
     ((null? input)
      (reverse output))
     ((< (length input) size)
      (reverse (cons input output)))
     (else
      (loop (drop input size)
            (cons (take input size)
                  output))))))

;; if offset is not #f, then encrypt, using a that offset.
;; otherwise, decrypt by returning possible offsets.
(define (hmm input-string spindle offset)
  (unless (or (not offset)
              (and (integer? offset)
                   (exact? offset )
                   (not (negative? offset))))
    (raise-type-error 'hmm "#f, or exact non-negative integer" offset))
  (unless (string? input-string)
    (raise-type-error 'hmm "string" input-string))
  (if offset
      (map (lambda (letters)  (general-purpose (list->string letters) spindle #t offset))
           (split-into-lists-of-size (length spindle)
                                     (lc-letters-only input-string)))
    (map (lambda (offset)
           (general-purpose input-string spindle #f offset))
         (iota *alphabet-length*))))

(define (encrypt input spindle)
  (hmm input spindle (random-offset)))

(define (decrypt input spindle)
  (hmm input spindle #f))

(define (make-spindle-from-string input)
  (map (lambda ignored (make-one-wheel)) (string->integers input)))
(define (test spindle plaintext)
  (define ciphers (encrypt  plaintext spindle))
  (define recovereds (map (lambda (c) (decrypt c spindle)) ciphers))
  (printf "~s encrypts to ~s~%" plaintext ciphers)
  (printf "~s decrypts to ~s~%" ciphers recovereds)
  )

(define plain "Hello")
(define spindle (make-spindle-from-string plain))
(test spindle plain)
(test spindle "More of the same")

