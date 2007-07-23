#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module planet-emacsen mzscheme
(require (lib "trace.ss")
         (only (lib "1.ss" "srfi")
               filter
               first)
         (only (lib "19.ss" "srfi" )
               date->time-utc

               time>=?)
         (rename (lib "19.ss" "srfi" )
                 19:make-date make-date)
         (only (lib "uri-codec.ss" "net")
               current-alist-separator-mode)

         (only (planet "rfc3339.ss" ("neil" "rfc3339.plt")) rfc3339-string->srfi19-date/constructor)
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (only (planet "port.ss"      ("schematics"  "port.plt" ))
               port->string)

         (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))

         (only (lib "url.ss" "net")
               get-pure-port
               string->url))

(provide
 planet-emacsen-news
 entries-newer-than)
(define (trim str)
  (regexp-replace*
   (pregexp "(\r|\n)+")
   str
   ""))
;;(trace trim)
(define *the-url* (string->url "http://planet.emacsen.org/atom.xml"))
(define (planet-emacsen-news)
  (parameterize ((current-alist-separator-mode 'amp))
                (html->shtml
                 (port->string (get-pure-port
                                *the-url*
                                (list))))))

(define (entries-newer-than news srfi-19-date)

  (let ((entries
         ((sxpath '(entry))
          news)))

    (filter
     (lambda (triplet)
       (time>=?
        (date->time-utc (first triplet))
        (date->time-utc srfi-19-date)))

     (map
      (lambda (entry)
        (let* ((updated
                ;; if only the Atom spec ensured that all the time
                ;; zones are the same, all I would have needed to do is
                ;; ensure that srfi-19-date uses the same zone, and
                ;; then just compare the strings.  But alas.
                (rfc3339-string->srfi19-date/constructor
                 (car
                  ((sxpath '(updated *text*))
                   entry))
                 19:make-date))
               (source
                ((sxpath '(source))
                 entry))
               (title
                (car
                 ((sxpath '(title *text*))
                  source)))
               (subtitle
                ((sxpath '(subtitle *text*))
                 source)))
          (cons updated (cons title subtitle))))

      entries))))
)
