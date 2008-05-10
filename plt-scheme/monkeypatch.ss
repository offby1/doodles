#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
PATH=$PATH:/usr/local/src/plt/bin
mzscheme --eval "(compile-enforce-module-constants #f)" --load $0
|#

;;(compile-enforce-module-constants #f)

(require scheme/enter)
(require "mut.ss")
(printf "Before doing anything: ~s~%" TheThing)
(enter! "mut.ss")
(set! TheThing "Some other thing")
(enter! #f)
(printf "After: ~s~%" TheThing)
