;; This module wraps a few useful slib functions.  A user of this
;; module doesn't need to call `require', which strikes me as a Good
;; Thing, since Lord only knows what crap `require' pulls into your
;; environment.

(define-module (primes))
(use-modules (ice-9 slib))

(require 'factor)

(define-public factors (lambda args (apply factor args)))
