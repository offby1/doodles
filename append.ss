#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module append mzscheme
(define (append-1 one-list another-list)
  (cond
   ((null? one-list)
    another-list)
   ((null? (cdr one-list))
    (cons (car one-list)
          another-list))
   (else
    (cons (car one-list)
          (append-1 (cdr one-list)
                    another-list)))))
(define (my-append . lists)
  (let loop ((lists (reverse lists))
             (result '()))
    (if (null? lists)
        result
      (loop (cdr lists)
            (append-1 (car lists)
                      result)))))
(printf "~s~%"
        (my-append '(1 2 3) '(a b c) '(foo bar baz)))
)