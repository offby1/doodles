#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module hash-counter mzscheme
(require (lib "trace.ss")
         (only (lib "list.ss") sort))
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

(define (count-print c port write?)
  (when write? (write-string "<" port))
  (write-string (char-counts->string c) port)
  (when write? (write-string ">" port)))

(define-values (s:count make-count count? count-ref count-set!)
  (make-struct-type 'count #f 1 0 #f
                    (list (cons prop:custom-write count-print)) #f))

(define (get-count char counter)
  (hash-table-get (count-ref counter 0) char 0))

(define (inc-count! char counter . amount)
  (if (null? amount)
      (set! amount 1)
    (set! amount (car amount)))
  (hash-table-put! (count-ref counter 0) char (+ amount (get-count char counter))))

(define (randomly-move-count-towards! char counter target)
  (hash-table-put! (count-ref counter 0) char (random-inclusively-between (get-count char counter) target)))

(define (char-counts->string cc)
  (format "~a" (sort (hash-table-map (count-ref cc 0) cons) (lambda (a b)
                                                                 (char<? (car a)
                                                                         (car b))))))

(define (my-make-char-counts)
  (make-count (make-hash-table)))

(define (add-counts c1 c2)
  (combine c1 c2 inc-count!))

(define (random-progress c1 c2 char-to-fiddle)
  (let ((rv (make-count (hash-table-copy (count-ref c2 0)))))
    (randomly-move-count-towards! char-to-fiddle rv (get-count char-to-fiddle c2))
    (when #f
      (printf (string-append
               "random-progress: ~a~%"
               ",                ~a~%"
               " fiddling ~s       ~%"
               "  ------>>>>>    ~a~%")
              c1
              c2
              char-to-fiddle
              rv))
    rv))

(define (counts-equal? c1 c2 keys)
  (call/ec
   (lambda (return)
     (hash-table-for-each
      (count-ref c1 0)
      (lambda (left-key left-value)
        (let ((right-value  (hash-table-get (count-ref c2 0) left-key 0)))
          (when (and

                 ;; this clause is here to make the whole program go
                 ;; faster, but I honestly don't know if it does.
                 (member left-key keys)

                 (not (= left-value right-value)))
            (when #f (printf "Counts differ for character ~s: ~a versus ~a~%"
                    left-key
                    left-value
                    right-value))
            (return #f)))))
     #t))
  )
;(trace counts-equal?)
(define (combine c1 c2 proc!)
  (let ((rv (make-count (hash-table-copy (count-ref c2 0)))))
    (hash-table-for-each
     (count-ref c1 0)
     (lambda (left-key left-value)
       (proc! left-key rv left-value)))
    rv
    ))
)