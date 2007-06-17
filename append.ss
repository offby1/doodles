#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module append mzscheme

(define (append-2 one-list another-list)
  (let loop ((one-list (reverse one-list))
             (result another-list))
    (if (null? one-list)
        result
      (loop (cdr one-list)
            (cons (car one-list)
                  result)))))

(define (my-append . lists)
  (let loop ((lists (reverse lists))
             (result '()))
    (if (null? lists)
        result
      (loop (cdr lists)
            (append-2 (car lists)
                      result)))))
(printf "~s~%"
        (my-append '(1 2 3) '(a b c) '(foo bar baz)))
)