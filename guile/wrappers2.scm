;;; wrappers.scm - Library for wrapping functions & executing forms.
;;; Useful for implementing tracing & profiling (as is
;;; done here).
;;; Version 0.8
;;;
;;; Copyright (c) 1995 Harvey J. Stein (hjstein@math.huji.ac.il)
;;; This code is freely usable and distributable as long as this
;;; heading remains.
;;;
;;; Modified by Eric Hanchrow, for Guile 1.2
;;; Usage:
;;; For tracing:
;;; (with-tracing (foo bar baz) (baz 19))
;;; Executes (baz 19), while tracing entries to & exits from foo,
;;; bar & baz.
;;;
;;; For profiling:
;;; (with-profiling ...)
;;; Same story, but keeps track of execution times and prints
;;; results at end.
;;;
;;; In general:
;;; (with-wrappers wrapper starter ender simlist form)
;;; wrapper should be a function which takes 2 args, a symbol & a
;;; fcn. It should return a fcn. Starter & ender are thunks.
;;; Simlist is a list of symbols. Form is an sexp to evaluate.
;;; Basically, the defn of all symbols s in simlist are replaced with
;;; (wrapper 's s). Then starter is called, form is executed,
;;; ender is called, and the result of form is returned. The
;;; execution of ender is guaranteed by the use of dynamic-wind.
;;; Undoing the damage of wrapper is guaranteed by fluid-let.

;;; Bugs/To do:
;;; -Requires call to clock to be added to Stk - I added it to
;;; posix, even though it's not really posix (but it is ANSI C).
;;; If clock doesn't exist, I use the time from
;;; (get-internal-info). But, this causes lots of extra consing
;;; (and extra time taken up in the profiling).
;;; -How can I wrap symbols defined in children environments?
;;; -Would be nice to add some more statistics to the profiling
;;; (such as # of cells consed, etc).
;;; -Profiling should also figure out how much time of the time
;;; spent in a subroutine is actually spent amongst the children...
;;; -Maybe dynamic-wind should also be used in wrappers - On the one
;;; hand this would enable tracing to track continuation usage, but
;;; on the other hand, it might introduce alot of overhead.

(defmacro with-wrappers (wrapper starter ender simlist form)
  `(dynamic-wind ,starter
                 (lambda () (fluid-let ,(map (lambda (x)
                                               `(,x (,wrapper ',x ,x)))
                                             simlist)
                              ,form))
                 ,ender))

;;; ----------- Tracing code ------------------

(defmacro with-tracing (simlist form)
  `(with-wrappers trace:trace-wrap trace:trace-start (lambda () ())
                  ,simlist ,form))

(define (trace:trace-start)
  (set! *trace-stack* ()))

(define (trace:trace-wrap name func)
  (lambda l
    (trace:start name l)
    (let ((res (apply func l)))
      (trace:end name l res)
         res)))

(define *trace-stack* ())

(define (trace:start name l)
  (trace:header)
  (format #t "~s\n" (cons name l))
  (set! *trace-stack* (cons name *trace-stack*)))

(define (trace:end name l res)
  (set! *trace-stack* (cdr *trace-stack*))
  (trace:header)
  (format #t "~s = ~s\n" (cons name l) res))

(define (trace:header)
  (let loop ((result "")
             (i (length *trace-stack*)))
    (if (not (positive? i))
        result
      (loop (string-append result "| ")
            (- i 1)))))

;;; ----------- Profiling code ------------------

(require "formout")
(require "posix")
(require "hash")

(if (not (symbol-bound? 'posix-clock))
    (define (posix-clock) (vector-ref (get-internal-info) 0)))

(defmacro with-profiling (simlist form)
  `(with-wrappers profile:profile-wrap
                  profile:profile-start
                  profile:profile-end
                  ,simlist ,form))

(define *profile-stack* #f)
(define *profile-times* #f)

(define (profile:profile-start)
  (set! *profile-stack* ())
  (set! *profile-times* (make-hash-table)))

(define (profile:profile-end)
  (define funccol (formout:make-fmt-fcn "~20a"))
  (define calledcol (formout:make-fmt-fcn "~10f"))
  (define timecol (formout:make-fmt-fcn "~10,3f"))
  (format #t "Function Called Time\n")
  (format #t "------------------- ---------- ---------\n")
  (hash-table-for-each *profile-times*
                       (lambda (func prof-data)
                         (funccol #t (symbol->string func))
                         (calledcol #t (profile:times-called prof-data))
                         (timecol #t (/ (profile:elapsed-time prof-data)
                                        |CLOCKS_PER_SEC|))
                         (format #t "\n"))))

(define (profile:times-called v)
  (vector-ref v 0))

(define (profile:elapsed-time v)
  (vector-ref v 1))

(define (profile:profile-wrap name func)
  (let ((results (make-vector 10 0)))
    (let ((res ())
          (time -1))
      (hash-table-put! *profile-times* name results)
         (lambda l
           (cond ((< time 0)
                  (set! time (posix-clock))
                  (set! res (apply func l))
                  (vector-set! results 1 (+ (vector-ref results 1)
                                            (- (posix-clock) time)))
                  (vector-set! results 0 (1+ (vector-ref results 0)))
                  (set! time -1)
                  res)
                 (else
                  (set! res (apply func l))
                  (vector-set! results 0 (1+ (vector-ref results 0)))
                  res))))))
