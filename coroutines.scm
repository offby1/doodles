;; from http://www.cs.smsu.edu/~pete/csc326/Chap12f.html

(define call/cc call-with-current-continuation)

; demo of Scheme coroutines implemented using continuations.
;
; source: Scheme and the Art of Programming,
; George Springer and Daniel Friedman
; McGraw-Hill
; 1992
; ISBN 0-07-060522-X
; see pages 567-571.
 
(define coroutine-maker
  (lambda (proc)
    (let ((saved-continuation "any continuation"))
      (let ((update-continuation! 
             (lambda (v)
               (set! saved-continuation v))))
        (let ((resumer (resume-maker update-continuation!))
              (first-time #t))
          (lambda (value)
            (if first-time
                (begin
                  (set! first-time #f)
                  (proc resumer value))
              (saved-continuation value))))))))
 
(define resume-maker
  (lambda (update-proc!)
    (lambda (next-coroutine value)
      (let ((receiver (lambda (continuation)
			(update-proc! continuation)
			(next-coroutine value))))
	(call/cc receiver)))))
 
 
; helpful procedure to display any number of args on stdout
(define writeln
  (lambda args
    (for-each display args)
    (newline)))
 
 
; Three coroutines are defined: A, B, and C.
; Their interactions demonstrate non-hierarchical flow
 
(define A
  (let ((A-proc 
	 (lambda (resume v)
           (writeln "This is A")
           (writeln "Came from " (resume B "A"))
           (writeln "Back in A")
           (writeln "Came from " (resume C "A")))))
    (coroutine-maker A-proc)))
 
(define B
  (let ((B-proc 
	 (lambda (resume v)
           (writeln " This is B")
           (writeln " Came from " (resume C "B"))
           (writeln " Back in B")
           (writeln " Came from " (resume A "B")))))
    (coroutine-maker B-proc)))
 
(define C
  (let ((C-proc 
	 (lambda (resume v)
           (writeln " This is C")
           (writeln " Came from " (resume A "C"))
           (writeln " Back in C")
           (writeln " Came from " (resume B "C")))))
    (coroutine-maker C-proc)))
 
