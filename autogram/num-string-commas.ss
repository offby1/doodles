#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; http://synthcode.com/scheme/fmt-0.3.tgz does what this does (and
;; tons more) but it had a bug.  Supposedly foof has fixed the bug but
;; I haven't yet checked.

(module num-string-commas mzscheme
(require
 (lib "assert.ss" "offby1")
 (planet "test.ss"     ("schematics" "schemeunit.plt" 2))
 (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 2))
 (planet "util.ss"     ("schematics" "schemeunit.plt" 2)))
(provide num-string-commas)
(define (num-string-commas n)
  (if (zero? n)
      "0"
    (apply
     string
     (let charloop ((digits-emitted 0)
                    (digits (let loop ((n n)
                                       (result '()))
                              (if (zero? n)
                                  (reverse result)
                                (loop (quotient n 10)
                                      (cons (remainder n 10) result)))))
                    (result '()))
       (if (null? digits)
           result
         (let ((d (integer->char (+ (char->integer #\0)
                                    (car digits)))))
           (charloop (add1 digits-emitted)
                     (cdr digits)
                     (if (and (positive? digits-emitted)
                              (zero? (remainder digits-emitted 3)))
                         (cons d (cons  #\, result))
                       (cons d result)
                       ))))

       ))))
(exit-if-failed
 (test/text-ui
  (test-suite
   "The one and only suite"
   (test-equal?
    "zero"
    (num-string-commas 0)
    "0")
   (test-equal?
    "38,400"
    (num-string-commas 38400)
    "38,400")
   (test-equal?
    "384,000"
    (num-string-commas 384000)
    "384,000")
   )
  ))

)
