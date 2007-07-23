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
               third)
         (only (lib "19.ss" "srfi" )
               date->time-utc
               make-date
               time>=?)
         (lib "pretty.ss"))

(define *some-time-ago* (PLT-date->srfi-19-date (seconds->date (- (current-seconds) (* 6 3600)))))

(define (de-html str)
  (apply string-append ((sxpath '(// *text*)) (html->shtml str))))

(define (str seq)
  (de-html (apply string-append seq)))

(let* ((sources
        ((sxpath '(// entry source))
         (planet-emacsen-news))))
  (pretty-print
   (filter
    (lambda (triplet)
      (printf "~s ~s~%"
              (third triplet)
              *some-time-ago*)
      (time>=?
       (date->time-utc (third triplet))
       (date->time-utc *some-time-ago*)))

    (map (lambda (source)
           (append
            (map str
                 (list
                  ((sxpath '(title *text*))
                   source)
                  ((sxpath '(subtitle *text*))
                   source)))
            (map
             (lambda (str)
               (rfc3339-string->srfi19-date/constructor
                str
                make-date))
             ((sxpath '(updated *text*))
              source))))
         sources))
                         )
  )
(newline)
