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
      (printf "head: ~s; rest: ~s~n" (front-queue q) (delete-queue! q))
      (printf "head: ~s; rest: ~s~n" (front-queue q) (delete-queue! q))
      (printf "head: ~s; rest: ~s~n" (front-queue q) (delete-queue! q))

      )))