#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require
 (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
 (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(require/expose "auction.ss" (a-risk))
(require (only "auction.ss"  make-auction auction-add!))

(exit (if (test/text-ui
           (let ((a (make-auction 'north)))
             (test-suite
              "Tests for private auction stuff."
              (test-case
               "right risk"
               (check = 0 (a-risk a))
               (auction-add! a 'pass)
               (check eq? 0 (a-risk a))
               (auction-add! a '(1 heart))
               (check = 1 (a-risk a))
               (auction-add! a 'double)
               (check = 2 (a-risk a))
               (auction-add! a 'redouble)
               (check = 4 (a-risk a))
               (auction-add! a 'pass)
               (check = 4 (a-risk a))
               (auction-add! a '(1 spade))
               (check = 1 (a-risk a))
               (auction-add! a 'double)
               (auction-add! a 'pass)
               (auction-add! a 'pass)
               (check = 2 (a-risk a))
               (auction-add! a 'pass)
               (check = 2 (a-risk a)))
              )))
          0 1)
      )
