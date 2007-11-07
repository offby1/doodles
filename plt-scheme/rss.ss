#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme  --no-init-file --mute-banner --version --require "$0"
|#
(module rss mzscheme
(require (lib "pretty.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (lib "url.ss" "net")
         (planet "sxml.ss" ("lizorkin" "sxml.plt"))
         (only (planet "ssax.ss" ("lizorkin" "ssax.plt" 1))
               ssax:xml->sxml)
         (only (planet "port.ss" ("schematics" "port.plt" ))
               port->string))

(define (port->string/close ip)
  (begin0
      (port->string ip)
    (close-input-port ip)))

(define (grab-rss-stuff url)
  (let ((url (cond
              ((string? url)
               (string->url url))
              (else
               url)
              )))
  (call/input-url
   url
   get-pure-port
   (lambda (ip)
     (dynamic-wind
         void
         (lambda ()(ssax:xml->sxml ip '()))
         (lambda ()
           (fprintf (current-error-port)
                    "Closing that input port, boss~%")
           (close-input-port ip)))))))



(pretty-print
 (grab-rss-stuff  "http://planet.emacsen.org/atom.xml"))

(provide (all-defined))
)
