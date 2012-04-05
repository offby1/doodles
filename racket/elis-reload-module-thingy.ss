(compile-enforce-module-constants #f)

(module foo mzscheme
(define something 10)
(provide foo)
(define (foo x)
  (+ x something)))

(require foo)
(foo 3) ; -> 13
(eval '(define something 100) (module->namespace 'foo))
(foo 3) ; -> 103

;; The Emacs stuff will be inherently inexact, but not much to do
;; about that.  There are some global decisions to make.  For example,
;; if you open a file with a module and send just one definition to
;; the running mzscheme, then supposedly the best behavior is to just
;; create an empty module and evaluate the single definition there.
;; But what if the rest of the module `require's some stuff?  And you
;; get the idea, I hope.
