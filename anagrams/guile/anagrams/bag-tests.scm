(define-module (bag-tests))
(use-modules (bag))
(use-modules (assert))

;; Notes about bags in general:

;; creating a bag from a string needn't be all that fast, since we'll
;; probably only do it a few thousand times per application (namely,
;; reading a dictionary of words), whereas subtracting bags needs to
;; be *really* fast, since I suspect we do this O(n!) times where n is
;; the length of the string being anagrammed.

(assert (bag-empty? (bag "")))
(assert (bags=? (bag "abc")
                (bag "cba")))

(assert (bags=? (bag "a")
                (subtract-bags (bag "ab")
                               (bag "b"))))

(format #t "~a tests passed.~%" (module-name (current-module)))
