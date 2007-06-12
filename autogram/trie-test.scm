#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#
(module trie-test mzscheme
  (require (planet "test.ss"    ("schematics" "schemeunit.plt" 2)))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
  (require "trie.scm")
  (print-struct #t)

  (let ((lookup (lambda (t key-string)
                  (trie-lookup t key-string (lambda () #f)))))
    (test/text-ui
     (let ((t (new-trie))
           (a '()))
       (test-suite
        "everything"

        (test-case
         "yow"
         (check-true (trie? t))
         (check-false (lookup t "duh")))

        (test-case
         "ugh"
         (trie-add! t "a" 'zap)
         (check-equal? (lookup t "a") 'zap))

        (test-case
         "sam"
         (trie-remove! t "a")
         (check-false (lookup t "a")))

        (test-case
         "bob"
         (trie-add! t "a" 'letter-a)
         (check-equal? (lookup t "a"  ) 'letter-a))

        (test-case
         "tim"
         (trie-add! t "abc" 'abc)
         (check-equal? (lookup t "a"  ) 'letter-a)
         (check-equal? (lookup t "abc") 'abc))

        (test-case
         "common prefixes"
         (trie-add! t "allen" 'funt)
         (check-equal? (lookup t "a"  ) 'letter-a)
         (check-equal? (lookup t "abc") 'abc)
         (check-equal? (lookup t "allen") 'funt))
        ))))
  )
