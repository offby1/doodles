#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module byte-vector-counter mzscheme
(require
 (planet "assert.ss" ("offby1" "offby1.plt"))
 (lib "trace.ss")
 (planet "test.ss"     ("schematics" "schemeunit.plt" 2))
 (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 2))
 (planet "util.ss"     ("schematics" "schemeunit.plt" 2)))
(provide
 get-count
 inc-count!
 (rename my-make-char-counts make-count)
 char-counts->string
 add-counts!
 counts-equal?
)

(define-struct char-counts (bv) #f)

(define *alphabet-length* (add1
                           (- (char->integer #\z)
                              (char->integer #\a))))
(define (char->index c)
  (- (char->integer (char-downcase c))
     (char->integer #\a)))

(define (get-count char counter)
  (let ((index (char->index char)))
    (when (not index)
      (error "Hey!  You're not allowed to ask about" char))
    (bytes-ref (char-counts-bv counter) index)))
(define (inc-count! char counter . amount)
  (if (null? amount)
      (set! amount 1)
    (set! amount (car amount)))
  (bytes-set! (char-counts-bv counter)
                 (char->index char)
                 (+ amount (get-count char counter)))
  counter)
;(trace inc-count!)
(define (char-counts->string cc)
  (char-counts-bv cc))
(define (my-make-char-counts . initial-values)
  (if (not (null? initial-values))
      (make-char-counts (apply bytes initial-values))
    (make-char-counts (make-bytes *alphabet-length* 0)))
  )
(define (add-counts! c1 c2)
  (let loop ((slots-processed 0))
    (if (< slots-processed (bytes-length (char-counts-bv c1)))
        (begin
          (bytes-set! (char-counts-bv c1)
                         slots-processed
                         (+ (bytes-ref (char-counts-bv c1) slots-processed)
                            (bytes-ref (char-counts-bv c2) slots-processed)))
          (loop (add1 slots-processed)))))
  c1)
;(trace add-counts!)
(define (add-counts c1 c2)
  (error "Unimplemented"))

(define (counts-equal? c1 c2 keys)
  (let loop ((keys keys)
             (rv #t))

    (if (null? keys)
        #t
      (if (not (= (bytes-ref  (char-counts-bv c1) (char->index (car keys)))
                  (bytes-ref  (char-counts-bv c2) (char->index (car keys)))))
          #f
        (loop (cdr keys)
              rv)))))
(exit-if-failed
 (test/text-ui
  (test-suite
   "The one and only suite"

   (test-not-false
    "duh"
    (let ((thing (my-make-char-counts )))
      thing))

   (test-equal?
    "initially zero"
    0
    (let ((thing (my-make-char-counts )))
      (get-count #\x thing)))

   (test-equal?
    "counts as expected"
    4
    (let ((c #\z)
          (thing (my-make-char-counts)))
      (inc-count! c thing 3)
      (inc-count! c thing)
      (get-count c thing)))

   (test-case
    "add counts"
    (let ((c1 (my-make-char-counts  1 0 0 0))
          (c2 (my-make-char-counts  0 1 0 0)))
      (add-counts! c1 c2)
      (check-equal? (get-count #\a c1) 1)
      (check-equal? (get-count #\b c1) 1)
      (check-equal? (get-count #\a c2) 0)
      (check-equal? (get-count #\b c2) 1))))))

)
