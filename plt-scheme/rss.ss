#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme  --no-init-file --mute-banner --version --require "$0"
|#
(module rss mzscheme
(require (lib "etc.ss")
         (lib "pretty.ss")
         (lib "url.ss" "net")
         (only (planet "ssax.ss" ("lizorkin" "ssax.plt" 1))
               ssax:xml->sxml)
         (planet "sxml.ss" ("lizorkin" "sxml.plt")))

(define-struct entry (title link id updated content author source))

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
         (lambda () (ssax:xml->sxml ip '((atom . "http://www.w3.org/2005/Atom"))))
         (lambda () (close-input-port ip))))))



(pretty-print
 ((sxpath '(// atom:entry)) (grab-rss-stuff  "http://planet.emacsen.org/atom.xml")))

(provide (all-defined))
)
