#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module byte-vector-counter mzscheme
(require (lib "4.ss" "srfi"))
(provide
 get-count
 inc-count!
 (rename my-make-char-counts make-count)
 char-counts->string
 add-counts
 counts-equal?
)

(define-struct char-counts (bv) #f)

(define (char->index c)
  (- (char->integer (char-downcase c))
     (char->integer #\a)))

(define (get-count char counter)
  (u8vector-ref (char-counts-bv counter) (char->index char)))
(define (inc-count! char counter . amount)
  (if (null? amount)
      (set! amount 1)
    (set! amount (car amount)))
  (u8vector-set! (char-counts-bv counter)
                 (char->index char)
                 (+ amount (get-count char counter))))
(define (char-counts->string cc)
  (format "~s"  (char-counts-bv cc)))
(define (my-make-char-counts)
  (make-char-counts (make-u8vector 26 0)))
(define (add-counts c1 c2)
  (let ((rv (my-make-char-counts)))
    (let loop ((slots-processed 0))
      (if (< slots-processed (u8vector-length (char-counts-bv c1)))
          (begin
            (u8vector-set! (char-counts-bv rv)
                           slots-processed
                           (+ (u8vector-ref (char-counts-bv c1) slots-processed)
                              (u8vector-ref (char-counts-bv c2) slots-processed)))
            (loop (add1 slots-processed)))))
    rv))

(define (counts-equal? c1 c2 keys)
  (let loop ((keys keys)
             (rv #t))

    (if (null? keys)
        #t
      (if (not (= (u8vector-ref  (char-counts-bv c1) (char->index (car keys)))
                  (u8vector-ref  (char-counts-bv c2) (char->index (car keys)))))
          #f
        (loop (cdr keys)
              rv)))))
)
