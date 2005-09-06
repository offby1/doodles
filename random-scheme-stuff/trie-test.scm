#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#
(module trie-test mzscheme
  (require (planet "test.ss"    ("schematics" "schemeunit.plt" 1)))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1)))
  (require "trie.scm")
  (print-struct #t)

  (let ((lookup (lambda (t key-string)
                  (trie-lookup t key-string (lambda () #f)))))
    (test/text-ui
     (let ((t (new-trie))
           (a '()))
       (make-test-suite
        "everything"

        (make-test-case
         "yow"
         (assert-true (trie? t))
         (assert-false (lookup t "duh")))

        (make-test-case
         "ugh"
         (trie-add! t "a" 'zap)
         (assert-equal? (lookup t "a") 'zap))

        (make-test-case
         "sam"
         (trie-remove! t "a")
         (assert-false (lookup t "a")))

        (make-test-case
         "bob"
         (trie-add! t "a" 'letter-a)
         (assert-equal? (lookup t "a"  ) 'letter-a))

        (make-test-case
         "tim"
         (trie-add! t "abc" 'abc)
         (assert-equal? (lookup t "a"  ) 'letter-a)
         (assert-equal? (lookup t "abc") 'abc))

        (make-test-case
         "common prefixes"
         (trie-add! t "allen" 'funt)
         (assert-equal? (lookup t "a"  ) 'letter-a)
         (assert-equal? (lookup t "abc") 'abc)
         (assert-equal? (lookup t "allen") 'funt))
        ))))
  )
