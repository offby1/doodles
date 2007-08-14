#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui weak-tests 'verbose))"
|#
(module weak mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define strong (make-hash-table 'equal ))
(define weak   (make-hash-table 'equal  'weak))

(define (dump-em)
  (for-each (lambda (ht)
              (printf "~s~%" (hash-table-map ht cons)))
            (list strong weak)))

(define strong-string "I'm a strong string")
(define weak-string   "I'm a weak string")
(hash-table-put! strong strong-string 'foo)
(hash-table-put! weak   weak-string 'foo)

(define weak-tests

  (test-suite

   "do I understand weak hash tables?"
   ;; obviously at this point both hash tables should still contain
   ;; what we put in them.

   (test-equal?
    "strong table holds what we put in it"
    (hash-table-count strong)
    1)
   (test-equal?
    "weak table holds what we put in it"
    (hash-table-count weak)
    1)

   (test-suite
    "after clobbering the weak key"
    #:before
    (lambda ()
      (dump-em)
      (set! weak-string #f)
      (collect-garbage)))

   (test-equal?
    "strong table still holds what we put in it"
    (hash-table-count strong)
    1)
   (test-equal?
    "weak table is now empty"
    (hash-table-count weak)
    0)
   )

  )

(provide (all-defined))
)
