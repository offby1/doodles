#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qr "$0" ${1+"$@"}
|#

(require "planet-emacsen.ss"
         (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (only (planet "rfc3339.ss" ("neil" "rfc3339.plt")) rfc3339-string->srfi19-date/constructor)
         (only (planet "zdate.ss" ("offby1" "offby1.plt"))
               PLT-date->srfi-19-date
               zdate)
         (only (lib "1.ss" "srfi")
               append-map
               filter
               first
               third)
         (only (lib "19.ss" "srfi" )
               date->time-utc
               make-date
               time>=?)
         (lib "pretty.ss"))

(define *some-time-ago* (PLT-date->srfi-19-date (seconds->date (- (current-seconds) (* 24 3600)))))

(define (de-html str)
  (apply string-append ((sxpath '(// *text*)) (html->shtml str))))

(define (str seq)
  (de-html (apply string-append seq)))

(let* ((news ((sxpath '(feed)) (planet-emacsen-news)))
       (entries
        ((sxpath '(entry))
         news)))

  (pretty-print
   (filter
    (lambda (triplet)
      (time>=?
       (date->time-utc (first triplet))
       (date->time-utc *some-time-ago*))
      )

    (map
     (lambda (entry)
       (let* ((updated
               (rfc3339-string->srfi19-date/constructor
                (car
                 ((sxpath '(updated *text*))
                  entry))
                make-date))
              (source
               ((sxpath '(source))
                entry))
              (title
               (car
                ((sxpath '(title *text*))
                 source)))
              (subtitle
               ((sxpath '(subtitle *text*))
                source))
              )
         (cons updated (cons title subtitle))))

     entries))))

(newline)
