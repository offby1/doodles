#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qr "$0" ${1+"$@"}
|#

;; I am just a driver for the planet-emacsen module; once it's
;; properly working, and integrated into the bot, I will no longer be
;; needed.

(require "planet-emacsen.ss"
         (rename (lib "19.ss" "srfi") 19:make-date make-date)
         (only (planet "rfc3339.ss" ("neil" "rfc3339.plt"))
               rfc3339-string->srfi19-date/constructor)
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

(define *some-time-ago* (rfc3339-string->srfi19-date/constructor
                         "2007-07-16T19:59:00+00:00"
                         19:make-date))

(pretty-print
 (map entry->string
      (entries-newer-than
       ((sxpath '(feed)) (static-news-for-testing))
       *some-time-ago*)))
(newline)
