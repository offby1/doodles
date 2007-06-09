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
 add-counts!
 counts-equal?
)

(define (count-print c port write?)
  (when write? (write-string "<" port))
  (write-string (char-counts->string c) port)
  (when write? (write-string ">" port)))

(define-values (s:count make-count count? count-ref count-set!)
  (make-struct-type 'count #f 1 0 #f
                    (list (cons prop:custom-write count-print)) #f))

(define (get-count char counter)
  (hash-table-get (count-ref counter 0) (char-downcase char) 0))

(define (inc-count-internal! char counter delta)
  (when (char-alphabetic? char)
    (hash-table-put!
     (count-ref counter 0)
     (char-downcase char)
     (+ delta (get-count char counter)))))

(define (inc-count! char counter)
  (inc-count-internal! char counter 1))

(define (char-counts->string cc)
  (format "~a" (sort (hash-table-map (count-ref cc 0) cons) (lambda (a b)
                                                                 (char<? (car a)
                                                                         (car b))))))

(define (my-make-char-counts chars-of-interest)
  (make-count (make-hash-table)))

(define (add-counts c1 c2)
  (combine c1 c2 inc-count!))
;(trace add-counts)

;; modifies the first argument.  Hopefully it's faster than the
;; non-modifying version.
(define (add-counts! victim increments)
  (hash-table-for-each
   (count-ref increments 0)
   (lambda (char number)
     (inc-count-internal! char victim number)))
  victim)
;(trace add-counts!)
(define (counts-equal? c1 c2 keys)
  (let loop ((keys keys)
             (rv #t))

    ;; honey, did you see where I left my car keys?
    (if  (null? keys)
        #t
      (if (not (= (hash-table-get (count-ref c1 0) (car keys) 0)
                  (hash-table-get (count-ref c2 0) (car keys) 0)))
          #f
        (loop (cdr keys)
              rv)))))

;(trace counts-equal?)
(define (combine c1 c2 proc!)
  (let ((rv (make-count (hash-table-copy (count-ref c2 0)))))
    (hash-table-for-each
     (count-ref c1 0)
     (lambda (left-key left-value)
       (proc! left-key rv)))
    rv
    ))
)