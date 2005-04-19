#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module auction mzscheme
  
  (provide
   make-auction)

  (define (make-auction)
    'get-bent)
  )

