#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

;;; make-call is a confusing name, since this directory contains a
;;; module named 'call' that defines a function by that name.
(module make-call mzscheme
;;;the entry point for bidding
  (require
           "auction.ss"
           "constants.ss"
           "deck.ss"
           "tree.ss"
           (only "call.ss" call->string) ;gotta avoid "make-call" :-|
           (lib "trace.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))

  (define *seconds-per-call* 1/10)

  (define (make-call vulnerability auction-so-far deck my-seat)
    (auction-ref
     (best-auction-from-prefix auction-so-far (holding deck my-seat) *seconds-per-call*)
     (auction-length auction-so-far)))

  ;; a little exercise
  (when
      (positive?
       (test/text-ui
        (test-suite
         "Exercise make-call"
         (test-case
          "poo"
          (let ((dealer 'south)
                (deck (shuffled-deck)))
            (let loop ((a (make-auction dealer))
                       (current-caller dealer))
              (if (auction-complete? a)
                  (printf "OK, we're done:~n~a~n" (auction-contract a))
                (let ((new-call (make-call 'dunno a deck dealer)))
                  (auction-add! a new-call)
                  (printf "~a: ~a~n" current-caller (call->string new-call))
                  (loop a
                        (nth-successor current-caller 1))))))))))
    (exit 1)))
