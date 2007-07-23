#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qr "$0" ${1+"$@"}
|#

(require "planet-emacsen.ss"
         (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (only (lib "1.ss" "srfi")
               append-map)
         (lib "pretty.ss"))
(define (de-html str)
  (apply string-append ((sxpath '(// *text*)) (html->shtml str))))
(define (str seq)
  (de-html (apply string-append seq)))
(let* ((sources
        ((sxpath '(// entry source))
         (planet-emacsen-news))))
  (pretty-print
   (map (lambda (source)
          (map str
               (list
                ((sxpath '(title *text*))
                 source)
                ((sxpath '(subtitle *text*))
                 source)
                ((sxpath '(updated *text*))
                 source)
                )))
        sources))
  )
(newline)
