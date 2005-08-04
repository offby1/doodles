#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#


(module qt mzscheme
  (require "q.ss")
  (parameterize ((print-struct #t)
                 (print-hash-table #t))
    (let ((q (make-queue '(6))))
      (insert-queue! q 3)
      (insert-queue! q 9)
      (printf "Q: ~s~n" q)
      (for-each (lambda (item)
                  (printf "~s ~a in the queue~n"
                          item
                          (if (is-on-queue? item q) "is" "isn't")))
                (list 1 2 3 4 5 6))
      (printf "head: ~s; rest: ~s~n" (front-queue q) (delete-queue! q))
      (printf "head: ~s; rest: ~s~n" (front-queue q) (delete-queue! q))
      (printf "head: ~s; rest: ~s~n" (front-queue q) (delete-queue! q))

      )))