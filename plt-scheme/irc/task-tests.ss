#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module task-tests mzscheme
(require (lib "kw.ss")
         (only (lib "misc.ss" "swindle") dotimes)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         "task.ss")
(define/kw (check-generic-task-thingy
            checker-proc
            #:key [name 'unknown]
            )
  (let* ((times-run (box 0))
         (t (make-task
             name
             1/10
             (lambda ()
               (set-box! times-run (add1 (unbox times-run)))
               (format "I am ~s; my purpose in life is merely to increment a little counter"
                       name))
             #:verbose #t
             )))
    (checker-proc times-run t)
    ))
(define task-tests
  (test-suite
   "do-in-loop"
   (test-case
    "does something"
    (printf "Doing some tediously-long tests; patience~%")
    (check-generic-task-thingy
     (lambda (times-run t)
       (task-unsuspend t)
       (sleep 1)
       (kill t)
       (let ((snapshot (unbox times-run)))
         (check > snapshot 5 (format "loop didn't run many times (only ~a)" (unbox times-run)))
         (sleep 1)
         (check-equal? (unbox times-run) snapshot "thread stopped when we killed it")
         ))
     #:name 'do-in-loop-does-something
     ))
   (test-case
    "do it now!"
    (check-generic-task-thingy
     (lambda (times-run t)
       (dotimes (i 15) (do-it-now! t))
       (sleep 1)
       (kill t)
       (check > (unbox times-run) 15 "loop ran fifteen times"))
     #:name 'do-it-now))

   (test-case
    "sending it POSTPONE slows it down"
    (check-generic-task-thingy
     (lambda (times-run t)
       (dotimes (i 15)
                (printf "~a ... " i) (flush-output)
                (sleep 9/100)
                (postpone t))

       (kill t)
       (printf "~%")
       (check < (unbox times-run) 2 "loop barely ran"))
     #:name 'postpone-test))))
(provide task-tests)
)