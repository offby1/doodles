#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui thread-with-id-tests 'verbose))"
|#
(module thread mzscheme

;; this is just so that vprintf can display a meaningful "id" for each
;; thread.  Thanks to Eli Barzilay for the suggestion.

(define threads-created 0)
(require (only "globals.ss" register-version-string))
(register-version-string "$Id$")
(define *current-thread-id* (make-parameter 0))
(define (thread-with-id thunk)
  (set! threads-created (add1 threads-created))
  (parameterize ((*current-thread-id* threads-created))
    (thread thunk)))



(provide (all-defined-except threads-created))
)
