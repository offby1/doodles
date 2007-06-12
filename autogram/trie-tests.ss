#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module trie-tests mzscheme
(require
 (lib "assert.ss" "offby1")
 (planet "test.ss"     ("schematics" "schemeunit.plt" 2))
 (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 2))
 (planet "util.ss"     ("schematics" "schemeunit.plt" 2))
 "byte-vector-counter.ss"
 ;;"vtrie.ss"
 "alist-trie.ss"
                )

(define-check (check-fullness vtrie expected-true expected-examined)
  (let-values (((actual-true actual-examined)
                (how-full  vtrie)))
    (with-check-info
     (('actual (list actual-true actual-examined))
      ('expected (list expected-true expected-examined)))
     (or
      #t
      (and (equal? actual-true expected-true)
           (equal? actual-examined expected-examined))
      (fail-check)))))

(define *chars-of-interest* (string->list "boxiest"))
(let ((c1 (make-count *chars-of-interest*))
      (c2 (make-count *chars-of-interest*))
      (min 1)
      (max 14))
  (exit-if-failed
   (test/text-ui
    (test-suite
     "The one and only suite"

     #:before
     (lambda ()
       (inc-count! #\b c1 2)
       (inc-count! #\x c2 3))

     (test-case
      "empty"
      (let ((vt (make-trie max  *chars-of-interest*)))
        (check-false (is-present? vt c1))
        (check-fullness vt 0 max)))

     (test-case
      "adding"
      (let ((vt (make-trie max *chars-of-interest*)))
        (note! vt c1)
        (check-not-false
         (is-present? vt c1))
        (check-fullness vt 1 (* max (length *chars-of-interest*)))
        (note! vt c2)
        (check-not-false
         (and
          (is-present? vt c1)
          (is-present? vt c2)))))

     (test-case
      "common prefix"
      (let ((vt (make-trie max *chars-of-interest*))
            (c1 (make-count *chars-of-interest*))
            (c2 (make-count *chars-of-interest*)))
        ;; c1: 1 2 3 0
        ;; c2: 1 2 3 4
        (inc-count! #\b c1 1)
        (inc-count! #\b c2 1)

        (inc-count! #\o c1 2)
        (inc-count! #\o c2 2)

        (inc-count! #\x c1 3)
        (inc-count! #\x c2 3)

        (inc-count! #\i c2 4)
        (note! vt c1)
        (check-not-false (is-present? vt c1))
        (check-false     (is-present? vt c2))
        (note! vt c2)
        (check-not-false (is-present? vt c1))
        (check-not-false (is-present? vt c2))
        ))
     )))))