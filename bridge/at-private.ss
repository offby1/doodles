#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require
 (planet "test.ss"    ("schematics" "schemeunit.plt" 1))
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
 (planet "util.ss"    ("schematics" "schemeunit.plt" 1)))

(require/expose "auction.ss" (a-risk))

(exit (if (test/text-ui
           (let ((a (make-auction)))
             (make-test-suite
              "Tests for private auction stuff."
              (make-test-case
               "right risk"
               (assert eq? 'undefined (a-risk a))
               (auction-add! a 'pass)
               (assert eq? 'undefined (a-risk a))
               (auction-add! a '(1 heart))
               (assert = 1 (a-risk a))
               (auction-add! a 'double)
               (assert = 2 (a-risk a))
               (auction-add! a 'redouble)
               (assert = 4 (a-risk a))
               (auction-add! a 'pass)
               (assert = 4 (a-risk a))
               (auction-add! a '(1 spade))
               (assert = 1 (a-risk a))
               (auction-add! a 'double)
               (auction-add! a 'pass)
               (auction-add! a 'pass)
               (assert = 2 (a-risk a))
               (auction-add! a 'pass)
               (assert = 2 (a-risk a)))
              )))
          0 1)
      )
