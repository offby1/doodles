#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require "$0" --main -- ${1+"$@"}
|#

;; http://programmingpraxis.com/2011/05/20/isbn-validation/

#lang racket
(require rackunit rackunit/text-ui)

(define (digitchar->number d)
  (if (char=? (char-downcase d) #\x)
      10
      (- (char->integer d)
         (char->integer #\0))))

(define (groups str)
  (regexp-split #rx"[- \t]+" str))

(define (->digits . strings)
  (map digitchar->number (append* (map string->list strings))))

(define (->checksum constant digits)
  (apply + (map * digits constant)))

(provide validate-ISBN/EAN)
(define (validate-ISBN/EAN str)
  (match (groups str)
    [(list region publisher title check)
     (zero? (remainder
             (->checksum (reverse (build-list 10 add1))
                         (->digits region publisher title check))
             11))]
    [(list "978" region publisher title check)
     (zero? (remainder
             (->checksum (build-list 10 (lambda (i) (if (even? i ) 1 3)))
                         (->digits region publisher title check))
             10))]
    [_ #f]))

(define-test-suite validate-ISBN/EAN-tests
  (check-true  (validate-ISBN/EAN "0-330-28987-X"))
  (check-true  (validate-ISBN/EAN "0- 330 -28987--X"))
  (check-false (validate-ISBN/EAN "1-330-28987-X"))
  (check-false (validate-ISBN/EAN "frotz plotz"))

  (check-true  (validate-ISBN/EAN "978-0-440-22378-8"))
  (check-false (validate-ISBN/EAN "978-0-441-22378-8")))

(define-test-suite all-tests
  validate-ISBN/EAN-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
