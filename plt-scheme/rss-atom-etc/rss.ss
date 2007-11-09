#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme  --no-init-file --mute-banner --version --require "$0"
|#
(module rss mzscheme
(require (only (lib "1.ss" "srfi")
               second)
         (lib "etc.ss")
         (lib "pretty.ss")
         (lib "url.ss" "net")
         (only (planet "rfc3339.ss" ("neil" "rfc3339.plt"))
               rfc3339-string->srfi19-date/constructor)
         (only (lib "19.ss" "srfi" )
               date->time-utc
               make-time
               time-nanosecond
               time-second
               time-type
               time-utc->date
               time=?
               time<?
               time>?)
         (rename (lib "19.ss" "srfi" )
                 19:make-date make-date)
         (only (planet "ssax.ss" ("lizorkin" "ssax.plt" 1))
               ssax:xml->sxml)
         (planet "sxml.ss" ("lizorkin" "sxml.plt")))

(define-struct entry (title link id updated content author source) #f)

(define *timestamp-file-name*
  (build-path
   (this-expression-source-directory)
   "timestamp"))

(define
  *last-update-time-seconds*
  (with-handlers
      ([exn:fail:filesystem? (lambda (e) 0)])
      (with-input-from-file *timestamp-file-name* read)))

(define (write-timestamp-seconds s)
  (call-with-output-file
      *timestamp-file-name*
    (lambda (op)
      (write s op)
      (newline op))))

(define (grab-rss-stuff url)
  (call/input-url
   (string->url url)
   get-pure-port
   (lambda (ip)
     (dynamic-wind
         void
         (lambda ()
           ;; nuts -- not all RSS feeds have all their elements
           ;; prefixed with this URI; their elements have simple names
           ;; like "entry" instead of
           ;; "http://www.w3.org/2005/Atom/entry".

           (ssax:xml->sxml ip '((atom . "http://www.w3.org/2005/Atom"))))
         (lambda () (close-input-port ip))))))



(pretty-print
 (map
  (lambda (e-sxml)
    (make-entry
     (car
      ((sxpath '(atom:title *text*)) e-sxml))
     (car
      ((sxpath '(atom:link ((@ href *text*)))) e-sxml))
     (car
      ((sxpath '(atom:id *text*)) e-sxml))
     (date->time-utc
      (rfc3339-string->srfi19-date/constructor
       (car
        ((sxpath '(atom:updated *text*))
         e-sxml))
       19:make-date))
     (car
      ((sxpath '(atom:content *text*)) e-sxml))
     (car
      ((sxpath '(atom:author atom:name *text*)) e-sxml))
     (car
      ((sxpath '(atom:source atom:link ((@ href *text*)))) e-sxml))))

  ((sxpath '(// atom:entry))
   (grab-rss-stuff
    "http://planet.emacsen.org/atom.xml"
    ;; "http://graphics8.nytimes.com/services/xml/rss/nyt/HomePage.xml"
    ))))


(provide (all-defined)))

