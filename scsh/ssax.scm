#!/usr/bin/scsh \
-lm /usr/local/src/SSAX/lib/packages.scm -m ssax-vanilla -o extended-ports -s
!#

(display (SSAX:XML->SXML (make-string-input-port "<foo/>") '()))
(newline)
