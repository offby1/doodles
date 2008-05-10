#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
PATH=$PATH:/usr/local/src/plt/bin
mzscheme --eval "(compile-enforce-module-constants #f)" --load $0
|#

(require scheme/enter)
(require "victim.ss")
(printf "Before doing anything: ~s~%" TheThing)
(enter! "victim.ss")
(set! TheThing "Some other thing")
(enter! #f)
(printf "After: ~s~%" TheThing)
