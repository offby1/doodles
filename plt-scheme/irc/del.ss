#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module del mzscheme
(require (lib "cmdline.ss")
         (planet "delicious.ss" ("untyped" "delicious.plt" 1 1))
         (only (lib "19.ss" "srfi")
               date->string)
         (only (lib "1.ss" "srfi")
               take)
         (lib "pretty.ss")
         (lib "trace.ss"))

;; return all items with the tag "moviestowatchfor"

;;(dump-sxml-responses? #t)

(command-line
 "del.icio.us"
 (current-command-line-arguments)
 (once-each
  (("-p" "--password") p "Your password on del.icio.us"
   (current-password p))))

(current-username "tucumcari")

(let ((gotten (recent-posts "moviestowatchfor")))
  (display "GOt these:")
  (pretty-print (map (lambda (p)
                       (list (post-description p)
                             (post-url p)))
                     gotten)))

)