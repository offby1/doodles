#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module hash-counter mzscheme
(provide
 get-count
 inc-count!
 (rename my-make-char-counts make-count)
 char-counts->string
 add-counts
 random-progress
 counts-equal?
)

(define (random-inclusively-between a b)
  (let ((min (min a b)))
    (+ min (random (- (max a b) min -1)))))

(define-struct char-counts (ht) #f)

(define (get-count char counter)
  (hash-table-get (char-counts-ht counter) char 0))

(define (inc-count! char counter . amount)
  (if (null? amount)
      (set! amount 1)
    (set! amount (car amount)))
  (hash-table-put! (char-counts-ht counter) char (+ amount (get-count char counter))))

(define (randomly-move-count-towards! char counter target)
  (hash-table-put! (char-counts-ht counter) char (random-inclusively-between (get-count char counter) target)))

(define (char-counts->string cc)
  (format "~a" (hash-table-map (char-counts-ht cc) cons)))

(define (my-make-char-counts)
  (make-char-counts (make-hash-table)))

(define (add-counts c1 c2)
  (combine c1 c2 inc-count!))

(define (random-progress c1 c2)
  (combine c1 c2 randomly-move-count-towards!))

(define (counts-equal? c1 c2)
  (call/ec
   (lambda (return)
     (hash-table-for-each
      (char-counts-ht c1)
      (lambda (left-key left-value)
        (when (not (= left-value (hash-table-get (char-counts-ht c2) left-key 0)))
          (return #f))))
     #t))
  )

(define (combine c1 c2 proc!)
  (let ((rv (make-char-counts (hash-table-copy (char-counts-ht c2)))))
    (hash-table-for-each
     (char-counts-ht c1)
     (lambda (left-key left-value)
       (proc! left-key rv left-value)))
    rv
    ))
)