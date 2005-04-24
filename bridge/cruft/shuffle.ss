(require  (lib "list.ss" "srfi" "1")
          (lib "27.ss" "srfi"))

;; from riastradh: http://www.bloodandcoffee.net/campbell/code/shuffle.scm

;;; why is it syntax, rather than a procedure?  Here's Riastradh on IRC:
;;;
;;; I use a macro to force integration of VECTOR-LIKE-SHUFFLER.
;;; 
;;; If VECTOR-LIKE-SHUFFLER were not integrated at the two call
;;; sites, the calls to VECTOR-REF & VECTOR-SET! would most likely be
;;; unknown procedure calls, which is absurd.
;;; 
;;; VECTOR-REF, suitably protected by enclosing range & type checks,
;;; can be a single load instruction.  An unknown procedure call
;;; requires shuffling several registers, pushing a stack frame,
;;; jumping to an unknown space in code, causing a context
;;; switch (most likely causing a cache miss), popping a stack frame,
;;; and reshuffling all of the registers.

(define-syntax vector-like-shuffler
  (syntax-rules ()
    ((vector-like-shuffler vlike-length vlike-ref vlike-set!)
     (lambda (vlike)
       (do ((len (vlike-length vlike))
            (i 0 (+ i 1)))
           ((= i len))
         (let ((j (random-integer len)))
           (if (not (= i j))
               (let ((v (vlike-ref vlike i)))
                 (vlike-set! vlike i (vlike-ref vlike j))
                 (vlike-set! vlike j v)))))))))

(define shuffle-vector!
  (vector-like-shuffler vector-length vector-ref vector-set!))

(random-source-randomize! default-random-source)

(let ((v (list->vector (iota 4))))
  (shuffle-vector! v)
  (display v)
  (newline))
