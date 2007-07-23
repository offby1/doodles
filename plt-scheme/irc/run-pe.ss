#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qr "$0" ${1+"$@"}
|#

(require "planet-emacsen.ss"
         (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (only (planet "zdate.ss" ("offby1" "offby1.plt"))
               PLT-date->srfi-19-date
               zdate)
         (only (lib "1.ss" "srfi")
               append-map
               filter

               third)
         (lib "pretty.ss"))

(define *some-time-ago* (PLT-date->srfi-19-date (seconds->date (- (current-seconds) (* 24 3600)))))

(define (de-html str)
  (apply string-append ((sxpath '(// *text*)) (html->shtml str))))

(pretty-print
 (entries-newer-than
  ((sxpath '(feed)) (planet-emacsen-news))
  *some-time-ago*))
(newline)
