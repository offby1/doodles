;;hope this is prime!
(define *hash-table-size* 7919)

(define (number->hashcode n)
  (modulo n *hash-table-size*))

;; boneheaded but perhaps useful
(define (string->hashcode s)
  (with-input-from-string 
   s 
   (lambda ()
     (let loop ((ch (read-char))
                (base 1)
                (result 0))
       (if (eof-object? ch)
           (modulo result *hash-table-size*)
         (loop (read-char)
               (* 101 base)
               (+ result (* base (char->integer ch))))))
     )))

(define (object->hashcode o)
  (cond
   ((number? o) (number->hashcode o))
   ((string? o) (string->hashcode o))
   (else
    (error "I don't know how to make a hash code for " o))))

(define (make-hash-table)
   (make-vector *hash-table-size* '()))

(define (hash-get ht key)
  (let ((hit (assoc key (vector-ref ht (object->hashcode key)))))
    (and hit (cdr hit))
  ))

(define (hash-set! ht key value)
  (let* ((code (object->hashcode key))
         (p (vector-ref ht code)))
    (if (pair? p)
        (let ((hit (assoc key p)))
          (if hit
              (set-cdr! hit value)
            (vector-set! ht code (cons (cons key value) p))))
      (vector-set! ht code (list (cons key value))))))

(define (hash-table-map ht proc)
  (map (lambda (p)
         (proc (car p)
               (cdr p)))
       (apply append (vector->list ht))))
