#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module reloader mzscheme
(require "silly.ss")

(let loop ()
  (silly)
  (sleep 10)
  (loop))

(provide (all-defined))
)
