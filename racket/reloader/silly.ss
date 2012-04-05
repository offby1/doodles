#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module silly mzscheme

(define (silly)
  (printf "This sure is silly.~%"))
(provide (all-defined))
)
