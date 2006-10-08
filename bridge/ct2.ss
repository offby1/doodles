#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require
 (planet "test.ss" ("schematics" "schemeunit.plt" 2))
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
 ;(planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2))
 (planet "util.ss" ("schematics" "schemeunit.plt" 2))
 (lib "pretty.ss")
 "call.ss"
 "exceptions.ss")

(when (test/text-ui
       (test-suite
        "Tests for call.ss"

        (test-case
         "error when given garbage"
         (check-exn exn:fail:bridge? (lambda () (make-call "you ugly"))))
  
        (test-case
         "error when given too-large number"
         (check-exn exn:fail:bridge? (lambda () (make-call (make-bid 9 'clubs)))))


        (test-case
         "error when given something what ain't a real suit"
         (check-exn exn:fail:bridge? (lambda () (make-call (make-bid 3 'pitchforks)))))

        (test-case
         "error when asked for level of something that's not a bid"
         (check-exn exn:fail:contract? (lambda () (level (make-call 'pass)))))
  
        (test-case
         "error when asked for denomination of something that's not a bid"
         (check-exn exn:fail:contract? (lambda () (denomination (make-call 'pass)))))

        (test-case
         "passes are passes, etc."
         (check-true (pass?     (make-call 'pass)))
         (check-true (double?   (make-call 'double)))
         (check-true (redouble? (make-call 'redouble)))
         )
  
        (test-case
         "passes are not doubles, etc"
         (check-false (double? (make-call 'pass)))
         (check-false (pass? (make-call 'redouble)))
         (check-false (redouble? (make-call 'double)))
         )
  
        (test-case
         "allows singular form"
         (check eq? 'clubs    (denomination (make-bid 3 'club)))
         (check eq? 'diamonds (denomination (make-bid 3 'diamond)))
         (check eq? 'hearts   (denomination (make-bid 3 'heart)))
         (check eq? 'spades   (denomination (make-bid 3 'spade)))
         )
        
        (test-case
         "Random shit aren't passes"
         (check-false (pass? 1776)))
  
        (let ((b (make-bid 3 'clubs)))
          (test-suite
           "Real bids"
           (test-case
            "right level"
            (check = 3 (level b)))

           (test-case
            "right denomination"
            (check eq? 'clubs (denomination b)))

           (test-case
            "Bids are calls"
            (check-true (call? b) "If you want to send a message, call Western Union."))

           (test-case
            "ordering"
            (check-true (bid>? b (make-bid 2 'clubs)))
            (check-false (bid>? (make-bid 2 'clubs) b))
            (check-true (bid>? (make-bid 3 'diamonds) b))
            (check-true (bid>? (make-bid 3 'notrump) b))
            )))

        (test-case
         "stringification"
         (check string=? (call->string (make-call 3 'clubs)) "3C")
         (check string=? (call->string (make-call 'pass)) "p-")
         (check string=? (call->string (make-call 'double)) "X ")
         (check string=? (call->string (make-call 'redouble)) "XX")
         )

        ))
  (exit 0))
(exit 1)
