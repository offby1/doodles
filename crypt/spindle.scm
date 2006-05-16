;; a "Jefferson Wheel" (http://en.wikipedia.org/wiki/Jefferson_disk)
(module spindle mzscheme
(require (only (lib "1.ss" "srfi") circular-list filter iota)
         (only (lib "13.ss" "srfi") string-join)
         (only (lib "43.ss" "srfi" ) vector-fold vector-map)
         (lib "trace.ss")
         )
(provide

 *alphabet-length*
 (rename public-make-spindle make-spindle)
 rotate-to-display!
 string-at-offset
 tableaux
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
   (apply vector
    (map
     (lambda ignored
       (public-make-wheel))
     (iota num-wheels)))))

;;(trace public-make-spindle)
(define (num-wheels s)
  (vector-length (spindle-wheels s)))

(define (rotate-to-display! s string)
  (unless (string? string)
    (raise-type-error 'rotate-to-display! "string" string))
  (let  ((chars  (map char-downcase (filter char-alphabetic? (string->list string))))
         (wheels (spindle-wheels s)))
    (if (not (<= (length chars) (num-wheels s)))
        (error 'rotate-to-display! (format "~a wheels but ~a chars in input string"
                                           (num-wheels s)
                                           (length chars))))
    (let loop ((chars chars)
               (wheels (vector->list wheels)))
      (cond
       ((not (null? chars))
        (let ((this-char (car chars)))
          (rotate-until! (car wheels) this-char)
          (loop (cdr chars)
                (cdr wheels))))
       ((not (null? wheels))
        (rotate-until! (car wheels) #\a)
        (loop chars
              (cdr wheels))))
      )))

(define (vector->string v)
  (vector-fold (lambda (i state char)
                 (string-append state (string char)))  "" v))

(define (string-at-offset s offset)
  (vector->string (vector-map (lambda (index wheel)
                                (letter-at wheel offset))
                              (spindle-wheels s))))

(define (tableaux s)
  (string-join (map (lambda (i) (string-at-offset s i)) (iota *alphabet-length*)) (string #\newline) 'infix))
)