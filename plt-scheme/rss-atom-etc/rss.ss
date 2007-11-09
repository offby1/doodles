#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme  --no-init-file --mute-banner --version --require "$0"
|#
(module rss mzscheme
(require (lib "trace.ss")
         (only (lib "1.ss" "srfi")
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

           (ssax:xml->sxml
            ip
            '(

              ;; works for http://planet.emacsen.org/atom.xml
              (atom . "http://www.w3.org/2005/Atom")

              ;; works for http://news.google.com/news?q=lemurs&output=atom
              (atom . "http://purl.org/atom/ns#"))
            ))
         (lambda () (close-input-port ip))))))
;;(trace grab-rss-stuff)

(define (fc thing)
  (and (pair? thing)
       (car thing)))

(define (atom->first-entry whole-feed description)
  (let* ((crappy? (equal? '("0.3") ((sxpath '(atom:feed @ version *text*)) whole-feed))))
    (fprintf (current-error-port)
             "~a: crappy? ~s~%"
             description
             crappy?)
    (let ((e-sxml ((sxpath '(atom:feed atom:entry)) whole-feed)))
      (make-entry
       (fc
        ((sxpath '(atom:title *text*)) e-sxml))
       (fc
        ((sxpath '(atom:link ((@ href *text*)))) e-sxml))
       (fc
        ((sxpath '(atom:id *text*)) e-sxml))
       (date->time-utc
        (rfc3339-string->srfi19-date/constructor
         (fc
          ((sxpath (if crappy?
                       '(atom:modified *text*)
                       '(atom:updated *text*)))
           e-sxml))
         19:make-date))
       (fc
        ((sxpath '(atom:content *text*)) e-sxml))
       (fc
        ((sxpath '(atom:author atom:name *text*)) e-sxml))
       (fc
        ((sxpath '(atom:source atom:link ((@ href *text*)))) e-sxml))))
    ))

(define (first-entry url)
  (sxpath '(atom:entry))
  (let ((parsed (grab-rss-stuff url)))
    (atom->first-entry parsed url)))

(pretty-print
  (map
   first-entry
   (list
    "http://planet.emacsen.org/atom.xml"
    "http://news.google.com/news?q=lemurs&output=atom"
    )))


(provide (all-defined)))

