;; used by jw2
(module spindle mzscheme
(require (only (lib "1.ss" "srfi") circular-list filter iota))
(provide

 *alphabet-length*
 (rename public-make-spindle make-spindle)
 rotate-to-display!
 string-at-offset
 )

(define *alphabet-length* 26)

(define (shuffled-list seq)
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
  (vector->list (fisher-yates-shuffle! (list->vector seq))))

;; #\A => 0
;; #\a => 0
;; #\z => 25 etc
(define (letter->integer c)
  (unless (char-alphabetic? c)
    (raise-type-error 'letter->integer "alphabetic" c))
  (- (char->integer (char-downcase c)) (char->integer #\a)))

(define (integer->letter i)
  (unless (and (exact? i)
               (not (negative? i))
               (< i *alphabet-length*))
    (raise-type-error 'integer->letter (format "exact, non-negative integer less than ~a" *alphabet-length*) i))
  (integer->char (+ i (char->integer #\a))))


;; a wheel with the letters of the alphabet distributed randomly about
;; its circumference.
(define-struct wheel (cl) #f)

(define (public-make-wheel)
  (make-wheel (apply circular-list (shuffled-list (map integer->letter (iota *alphabet-length*))))))

(define (letter-at wheel offset)
  (list-ref (wheel-cl wheel) offset))

;; rotate the wheel until the given letter is at the front
(define (rotate-until! wheel letter)
  (unless (< (letter->integer letter) *alphabet-length*)
    (raise-type-error 'rotate-until! "alphabetic" letter))
  (set-wheel-cl! wheel (member letter (wheel-cl wheel))))


(define-struct spindle (wheels) #f)
(define (public-make-spindle num-wheels)
  (make-spindle
   ;; this should probably be a vector, not a list
   (map
    (lambda ignored
      (public-make-wheel))
    (iota num-wheels))))

(define (spindle-size s)
  (length (spindle-wheels s)))

;; s is a spindle
;; 0 <= index < size of s
;; 0 <= another-offset < *alphabet-length*
(define (read-wheel s index another-offset)
  (letter-at
   (list-ref (spindle-wheels s) index)
   another-offset))

(define (read-spindle s another-offset)
  (list->string
   (map (lambda (index)
          (read-wheel s index another-offset))
        (iota 0 (spindle-size s)))))

(define (rotate-to-display! s string)
  (unless (string? string)
    (raise-type-error 'rotate-to-display! "string" string))
  (let  ((chars  (map char-downcase (filter char-alphabetic? (string->list string))))
         (wheels (spindle-wheels s)))
    (if (not (<= (length chars) (length wheels)))
        (error 'rotate-to-display! (format "~a wheels but ~a chars in input string"
                                           (length wheels)
                                           (length chars))))
    (let loop ((chars chars)
               (wheels wheels))
      (if (not (null? chars))
          (let ((this-char (car chars))
                (this-wheel (car wheels)))
            (rotate-until! this-wheel this-char)
            (loop (cdr chars)
                  (cdr wheels)))))))

(define (string-at-offset s offset)
  (list->string (map (lambda (w)
                       (letter-at w offset))
                     (spindle-wheels s))))

)