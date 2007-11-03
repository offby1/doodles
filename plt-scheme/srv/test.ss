#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#

(module test mzscheme
(require (lib "thread.ss")
         "server.ss")
(if #f
    (server-loop (current-input-port)
                 (current-output-port))
    (run-server 1234 server-loop #f))
)