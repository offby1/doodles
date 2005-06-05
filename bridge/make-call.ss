#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; make-call is a confusing name, since this directory contains a
;; module named 'call' that defines (but thankfully doesn't provide) a
;; function by that name.
(module make-call mzscheme
;;;the entry point for bidding
  (provide make-call)
  (require "tree.ss"
           "auction.ss"
           "constants.ss"
           (only "call.ss" call->string)
           (lib "trace.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 1))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1)))

  (define *seconds-per-call* 1/10)
  
  (define (make-call vulnerability auction-so-far deck)
    (auction-ref
     (best-auction-from-prefix auction-so-far *seconds-per-call*)
     (auction-length auction-so-far)))

  ;; a little exercise
  (when
      (not (test/text-ui
       (make-test-suite
        "Exercise make-call"
        (make-test-case
         "poo"
         (let ((dealer 'south))
           (let loop ((a (make-auction dealer))
                      (current-caller dealer))
             (if (auction-complete? a)
                 (printf "OK, we're done:~n~a~n" (auction-contract a))
               (let ((new-call (make-call 'dunno a dealer)))
                 (auction-add! a new-call)
                 (printf "~a: ~a~n" current-caller (call->string new-call))
                 (loop a
                       (nth-successor current-caller 1))))))))))
    (exit 1)))
