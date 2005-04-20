#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require
 (planet "test.ss" ("schematics" "schemeunit.plt" 1))
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
 ;(planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 1))
 (planet "util.ss" ("schematics" "schemeunit.plt" 1))
 (lib "pretty.ss")
 "call.ss")

(when (test/text-ui
       (make-test-suite
        "Tests for call.ss"

        (make-test-case
         "error when given garbage"
         (assert-exn exn:fail:contract? (lambda () (make-call "you ugly"))))
  
        (make-test-case
         "error when given too-large number"
         (assert-exn exn:fail:contract? (lambda () (make-call (make-bid 9 'clubs)))))


        (make-test-case
         "error when given something what ain't a real suit"
         (assert-exn exn:fail:contract? (lambda () (make-call (make-bid 3 'pitchforks)))))

        (make-test-case
         "error when asked for level of something that's not a bid"
         (assert-exn exn:fail:contract? (lambda () (level (make-call 'pass)))))
  
        (make-test-case
         "error when asked for denomination of something that's not a bid"
         (assert-exn exn:fail:contract? (lambda () (denomination (make-call 'pass)))))

        (make-test-case
         "passes are passes, etc."
         (assert-true (pass?     (make-call 'pass)))
         (assert-true (double?   (make-call 'double)))
         (assert-true (redouble? (make-call 'redouble)))
         )
  
        (make-test-case
         "passes are not doubles, etc"
         (assert-false (double? (make-call 'pass)))
         (assert-false (pass? (make-call 'redouble)))
         (assert-false (redouble? (make-call 'double)))
         )
  
        (make-test-case
         "allows singular form"
         (assert eq? 'clubs    (denomination (make-bid 3 'club)))
         (assert eq? 'diamonds (denomination (make-bid 3 'diamond)))
         (assert eq? 'hearts   (denomination (make-bid 3 'heart)))
         (assert eq? 'spades   (denomination (make-bid 3 'spade)))
         )
        
        (make-test-case
         "Random shit aren't passes"
         (assert-false (pass? 1776)))
  
        (let ((b (make-bid 3 'clubs)))
          (make-test-suite
           "Real bids"
           (make-test-case
            "right level"
            (assert = 3 (level b)))

           (make-test-case
            "right denomination"
            (assert eq? 'clubs (denomination b)))

           (make-test-case
            "Bids are calls"
            (assert-true (call? b) "If you want to send a message, call Western Union."))

           (make-test-case
            "ordering"
            (assert-true (bid>? b (make-bid 2 'clubs)))
            (assert-false (bid>? (make-bid 2 'clubs) b))
            (assert-true (bid>? (make-bid 3 'diamonds) b))
            (assert-true (bid>? (make-bid 3 'notrump) b))
            )))

        ))
  (exit 0))
(exit 1)