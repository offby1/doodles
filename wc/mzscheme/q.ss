#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module q mzscheme
  (require (planet "test.ss"    ("schematics" "schemeunit.plt" 1))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
           (only (lib "1.ss" "srfi") last drop-right!))
  (provide enqueue!
           queue-empty?
           (rename my-make-queue make-queue)
           dequeue!)
  
  (print-struct #t)
  
  (define-struct queue (l) #f)
  
  (define (my-make-queue)
    (make-queue '()))

  (define (queue-empty? q)
    (null? (queue-l q)))

  (define-syntax enqueue!
    (syntax-rules ()
      ((_ thing q)
       (begin
         (printf "enqueing ~a onto ~a~n" thing q)
         (set-queue-l! q (cons thing (queue-l q)))))))

  (define-syntax dequeue!
    (syntax-rules ()
      ((_ q)
       (begin0
         (last (queue-l q))
         (printf "removed something, leaving ~a~n"  q)
         (set-queue-l! q (drop-right! (queue-l q) 1))))))

  (unless
      (test/text-ui
       (let ((q (my-make-queue)))
         (enqueue! 0 q)
         (enqueue! 1 q)
         (enqueue! 2 q)
         (make-test-suite "ya"
                          (make-test-case "boo"
                                          (assert-false (queue-empty? q))
                                          (assert = 0 (dequeue! q))
                                          (assert = 1 (dequeue! q))
                                          (assert = 2 (dequeue! q))
                                          (assert-true (queue-empty? q))
                                          )
                          )))
    (exit 1)))