#!/usr/local/bin/scsh \
-lm /usr/local/src/SSAX/lib/packages.scm -m ssax-vanilla -o extended-ports -s
!#

;;(call-with-input-string "<a href=\"http://www.bar.org/\">a link to bar</a>" (lambda (p) (write (SSAX:XML->SXML p '()))))
(call-with-input-file "/home/erich/public_html/lisps.xml"
                      (lambda (p) (write (SSAX:XML->SXML p '()))))
(newline)
