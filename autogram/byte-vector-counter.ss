#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module byte-vector-counter mzscheme
(require
 (lib "assert.ss" "offby1")
 (lib "trace.ss")
 (lib "4.ss" "srfi")
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
(define *char-indices* (make-vector 26 #f))
(define (internal-offset c)
  (- (char->integer (char-downcase c))
     (char->integer #\a)))
(define (char->index c)
  (vector-ref *char-indices* (internal-offset c)))

(define (get-count char counter)
  (let ((index (char->index char)))
    (when (not index)
      (error "Hey!  You're not allowed to ask about" char))
    (u8vector-ref (char-counts-bv counter) index)))
(define (inc-count! char counter . amount)
  (if (null? amount)
      (set! amount 1)
    (set! amount (car amount)))
  (u8vector-set! (char-counts-bv counter)
                 (char->index char)
                 (+ amount (get-count char counter)))
  counter)
;(trace inc-count!)
(define (char-counts->string cc)
  (format "~s"  (char-counts-bv cc)))
(define (my-make-char-counts chars-of-interest)
  (let loop ((chars-of-interest chars-of-interest)
             (slots-set 0))
    (when (not (null? chars-of-interest))
      (vector-set! *char-indices*
                   (internal-offset (car chars-of-interest))
                   slots-set)
      (loop (cdr chars-of-interest)
            (add1 slots-set))))

  (make-char-counts (make-u8vector (length chars-of-interest) 0))
  )
(define (add-counts! c1 c2)
  (let loop ((slots-processed 0))
    (if (< slots-processed (u8vector-length (char-counts-bv c1)))
        (begin
          (u8vector-set! (char-counts-bv c1)
                         slots-processed
                         (+ (u8vector-ref (char-counts-bv c1) slots-processed)
                            (u8vector-ref (char-counts-bv c2) slots-processed)))
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
      (if (not (= (u8vector-ref  (char-counts-bv c1) (char->index (car keys)))
                  (u8vector-ref  (char-counts-bv c2) (char->index (car keys)))))
          #f
        (loop (cdr keys)
              rv)))))
(let ((chars-of-interest '(#\a #\b #\m #\x)))
  (exit-if-failed
   (test/text-ui
    (test-suite
     "The one and only suite"

     (test-not-false
      "duh"
      (let ((thing (my-make-char-counts chars-of-interest)))
        thing))

     (test-equal?
      "initially zero"
      0
      (let ((thing (my-make-char-counts chars-of-interest)))
        (get-count (car chars-of-interest) thing)))

     (test-equal?
      "counts as expected"
      4
      (let ((c (car chars-of-interest))
            (thing (my-make-char-counts chars-of-interest)))
        (inc-count! c thing 3)
        (inc-count! c thing)
        (printf "~a~%" (char-counts->string thing))
        (get-count c thing)))

     (test-case
      "add counts"
      (let ((c1 (my-make-char-counts chars-of-interest))
            (c2 (my-make-char-counts chars-of-interest)))
        (check-true (counts-equal? c1 c2 chars-of-interest))
        (inc-count! #\a c1)
        (inc-count! #\b c2)
        (add-counts! c1 c2)
        (check-equal? (get-count #\a c1) 1)
        (check-equal? (get-count #\b c1) 1)
        (check-equal? (get-count #\a c2) 0)
        (check-equal? (get-count #\b c2) 1)
        ))

     (test-exn
      "Rejects illegal characters"
      exn?
      (lambda ()
        (let ((thing (my-make-char-counts chars-of-interest)))
          (get-count #\r thing))))
     )

    )))
)
