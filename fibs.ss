#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
;; "Jafet" typed this at rudybot
(define (fix f)
  (delay (f (fix f))))

(define (scanl op init stream)
  (delay
    (cons init
          (if (null? (force stream))
              '()
              (delay
                (scanl op
                       (op init (car (force stream)))
                       (cdr (force stream))))))))

(define (take n stream)
  (if (or (null? stream)
          (= n 0))
      '()
      (cons (car (force stream))
            (take (- n 1)
                  (cdr (force stream))))))

(provide main)
(define (main)
  (take 50 (fix
            (lambda (l) (cons 0 (scanl + 1 l))))))
