;;; wrappers.scm - Library for wrapping functions & executing forms.
;;;                Useful for implementing tracing & profiling (as is
;;;                done here).
;;;                Version 0.9
;;;
;;; Copyright (c) 1995, 1996, 1997, 1998 Harvey J. Stein (hjstein@bfr.co.il)
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.GPL.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA

;;;
;;; Usage:
;;;   For tracing:
;;;      (with-tracing (foo bar baz) (baz 19))
;;;      Executes (baz 19), while tracing entries to & exits from foo,
;;;      bar & baz.
;;;
;;;   For profiling:
;;;      (with-profiling ...)
;;;      Same story, but keeps track of execution times and prints
;;;      results at end.
;;;
;;;   In general:
;;;      (with-wrappers wrapper starter ender simlist form)
;;;      wrapper should be a function which takes 2 args, a symbol & a
;;;      fcn.  It should return a fcn.  Starter & ender are thunks.
;;;      Simlist is a list of symbols.  Form is an sexp to evaluate.
;;;      Basically, the defn of all symbols s in simlist are replaced with
;;;      (wrapper 's s).  Then starter is called, form is executed,
;;;      ender is called, and the result of form is returned.  The
;;;      execution of ender is guaranteed by the use of dynamic-wind.
;;;      Undoing the damage of wrapper is guaranteed by fluid-let.

;;; Bugs/To do:
;;;   -Requires call to clock to be added to Stk - I added it to
;;;    posix, even though it's not really posix (but it is ANSI C).
;;;    If clock doesn't exist, I use the time from
;;;    (get-internal-info).  But, this causes lots of extra consing
;;;    (and extra time taken up in the profiling).
;;;   -How can I wrap symbols defined in children environments?
;;;   -Would be nice to add some more statistics to the profiling
;;;    (such as # of cells consed, etc).
;;;   -Profiling should also figure out how much time of the time
;;;    spent in a subroutine is actually spent amongst the children...
;;;   -Maybe dynamic-wind should also be used in wrappers - On the one
;;;    hand this would enable tracing to track continuation usage, but
;;;    on the other hand, it might introduce alot of overhead.
(define-module (wrappers))
(use-modules (ice-9 format))
(export profile:profile-wrap
        profile:profile-start
        profile:profile-end
        with-wrappers
        with-profiling
        timeit)

(define-macro (with-wrappers wrapper starter ender symlist form)
  (let ((oldsym (map (lambda (sym) (gensym)) symlist)))
    `(let ,(map (lambda (old sym)
                  (list old sym))
                oldsym
                symlist)
       (dynamic-wind (lambda ()
                       (,starter)
                       ,@(map (lambda (sym)
                                `(set! ,sym (,wrapper ',sym ,sym)))
                              symlist))
                     (lambda ()
                       ,form)
                     (lambda ()
                       ,@(map (lambda (sym old)
                                `(set! ,sym ,old))
                              symlist oldsym)
                       (,ender))))))

;;; ----------- Tracing code ------------------

(define-macro (with-tracing simlist form)
  `(with-wrappers trace:trace-wrap trace:trace-start (lambda () ())
                  ,simlist ,form))

(define trace:trace-start #f)
(define trace:trace-wrap #f)
(define trace:start #f)
(define trace:end #f)
(define trace:header #f)

;;; Trying to be safe in the face of the user wrapping these fcns, but
;;; I didn't quite make it - it didn't seem to work.
(let ((set! set!)
      (lambda lambda)
      (let let)
      (apply apply)
      (format format)
      (cons cons)
      (+ +)
      (1+ 1+)
      (length length)
      (>= >=)
      (*trace-stack* '()))


(define (loc-trace:trace-start)
  (set! *trace-stack* ()))

(define (loc-trace:trace-wrap name func)
  (lambda l
    (loc-trace:start name l)
    (let ((res (apply func l)))
      (loc-trace:end name l res)
      res)))

(define (loc-trace:start name l)
  (loc-trace:header)
  (format #t "~s\n" (cons name l))
  (set! *trace-stack* (cons name *trace-stack*)))

(define (loc-trace:end name l res)
  (set! *trace-stack* (cdr *trace-stack*))
  (loc-trace:header)
  (format #t "~s = ~s\n" (cons name l) res))

(define (loc-trace:header)
  (do ((i 0 (1+ i))
       (end (length *trace-stack*)))
      ((>= i end))
    (format #t "| ")))

(set! trace:trace-start loc-trace:trace-start)
(set! trace:trace-wrap loc-trace:trace-wrap)
(set! trace:start loc-trace:start)
(set! trace:end loc-trace:end)
(set! trace:header loc-trace:header ))


;;; ----------- Profiling code ------------------

(define (profile:hash-table-for-each table func)
  (for-each (lambda (d) (func (car d) (cdr d))) (cdr table)))

(define (profile:make-hash-table)
  (list 'hash-table))

(define (profile:hash-table-put! table key val)
  (let ((v (assoc key (cdr table))))
    (if v
        (set-cdr! v val)
        (set-cdr! table (cons (cons key val) (cdr table))))))


(define (profile:clock)
  (/ (get-internal-run-time)
     internal-time-units-per-second))

(define-macro (with-profiling simlist form)
  `(with-wrappers profile:profile-wrap
                  profile:profile-start
                  profile:profile-end
                  ,simlist ,form))


(define *profile-stack* #f)
(define *profile-times* #f)

(define (profile:profile-start)
  (set! *profile-stack* '())
  (set! *profile-times* (profile:make-hash-table)))

(define (formout:make-fmt-fcn s)
  (lambda (p . args) (apply format p s args)))

(define-macro (timeit form)
  (let ((g1 (gensym))
        (g2 (gensym)))
    `(let* ((,g2 '())
            (,g1 (profile:clock)))
       ,form
       (set! ,g2 (profile:clock))
       (display "Elapsed time : ")
       (display (- ,g2 ,g1))
       (display "      GC time : ")
       (display (gc-run-time))
       (newline)
       (display "GC Stats : ")
       (display (gc-stats))
       (newline))))

(define (profile:profile-end)
  (define funccol   (formout:make-fmt-fcn "~20a"))
  (define calledcol (formout:make-fmt-fcn "~10f"))
  (define timecol   (formout:make-fmt-fcn "~10,3f"))
  (format #t "Function            Called     Time\n")
  (format #t "------------------- ---------- ---------\n")
  (profile:hash-table-for-each *profile-times*
                               (lambda (func prof-data)
                                 (funccol   #t (symbol->string func))
                                 (calledcol #t (profile:times-called prof-data))
                                 (timecol   #t (exact->inexact (profile:elapsed-time prof-data)))
                                 (format #t "\n")))
  (display "GC time : ")
  (display (gc-run-time))
  (newline)
  (display "GC Stats : ")
  (display (gc-stats))
  (newline))



(define (profile:times-called v)
  (vector-ref v 0))

(define (profile:elapsed-time v)
  (vector-ref v 1))

(define (profile:profile-wrap name func)
  (let ((results (make-vector 10 0)))
    (let ((res '())
          (time -1))
      (profile:hash-table-put! *profile-times* name results)
      (lambda l
        (cond ((< time 0)
               (set! time (profile:clock))
               (set! res (apply func l))
               (vector-set! results 1 (+ (vector-ref results 1)
                                         (- (profile:clock) time)))
               (vector-set! results 0 (1+ (vector-ref results 0)))
               (set! time -1)
               res)
              (else
               (set! res (apply func l))
               (vector-set! results 0 (1+ (vector-ref results 0)))
               res))))))

;;; --------- Fluid-let (for the hell of it)...

;(define-macro (fluid-let args . body)
;  `(with-wrappers (lambda (sym symval) (cadr (assoc sym ',args)))
;                 (lambda () #t)
;                 (lambda () #t)
;                 ,(map car args)
;                 (begin ,@body)))

;(define-macro (fluid-let args . body)
;  (let ((oldvals (gensym)))
;    `(let ((,oldvals ,(cons 'list `,(map car args))))
;       (dynamic-wind (lambda ()
;                      ,@(map (lambda (arg)
;                               `(set! ,(car arg) ,(cadr arg)))
;                             args))
;                    (lambda ()
;                      ,@body)
;                    (lambda ()
;                      ,@(map (lambda (arg val)
;                               `(set! ,(car arg) ,val))
;                             args
;                             `,oldvals))))))

;(define-macro (fluid-let args . body)
;  (let ((oldvals (gensym)))
;    `(let ((,oldvals ,(cons 'list `,(map car args))))
;       ,@(map (lambda (a v)
;               `(set! ,(car a) ,oldvals ,v))
;             args
;             '(3 4)))))
