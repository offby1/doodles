#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module s3 mzscheme
(define a-sentence (list "This sentence contains " (cons #\a 0)))
(define (n->string n)
  "one")
(define (maybe-pluralize c n)
  (let ((s (make-string 1 c)))
    (if (= n 1)
        s
      (string-append s "s"))))
(define (phase1 s)
  (let loop ((s s)
             (result '()))
    (if (null? s)
        (reverse result)
      (loop (cdr s)
            (cons (if (string? (car s))
                      (car s)
                    (let ((n (cdr (car s))))
                      (string-append (n->string n)
                                     " "
                                     (maybe-pluralize (car (car s))
                                                      n))))
                  result)))))
(display (phase1 a-sentence))
)