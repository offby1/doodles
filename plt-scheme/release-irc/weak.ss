#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui weak-tests 'verbose))"
|#
(module weak mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define strong-ht (make-hash-table 'equal ))
(define weak-ht   (make-hash-table 'equal  'weak))

(define (dump-em)
  (for-each (lambda (ht)
              (printf "~s~%" (hash-table-map ht cons)))
            (list strong-ht weak-ht)))

(define strong-string "I'm a strong string")
(define weak-string   (string-copy "I'm a weak string"))
(hash-table-put! strong-ht strong-string 'foo)
(hash-table-put! weak-ht   weak-string 'foo)

(define strongbox (box strong-string))
(define weakbox   (make-weak-box weak-string))

(define weak-tests

  (test-suite

   "do I understand weak hash tables?"
   ;; obviously at this point both hash tables should still contain
   ;; what we put in them.

   (test-equal?
    "strong table holds what we put in it"
    (hash-table-count strong-ht)
    1)
   (test-equal?
    "weak table holds what we put in it"
    (hash-table-count weak-ht)
    1)
   (test-equal?
    "strong box"
    (unbox strongbox)
    strong-string)
   (test-equal?
    "weak box"
    (weak-box-value weakbox)
    weak-string)

   (test-suite
    "after clobbering the weak key"
    #:before
    (lambda ()
      (dump-em)
      (set! weak-string #f)
      (collect-garbage)
      (dump-em)))

   (test-equal?
    "strong table still holds what we put in it"
    (hash-table-count strong-ht)
    1)
   (test-equal?
    "weak table is now empty"
    (hash-table-count weak-ht)
    0)

   (test-equal?
    "strong box"
    (unbox strongbox)
    strong-string)
   (test-false
    "weak box"
    (weak-box-value weakbox))))

(provide (all-defined))
)
