#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme  --no-init-file --mute-banner --version --require "$0"
|#
(module rss mzscheme
(require (lib "pretty.ss")
         (lib "url.ss" "net")
         (only (planet "ssax.ss" ("lizorkin" "ssax.plt" 1))
               ssax:xml->sxml))

(define (grab-rss-stuff url)
  (let ((url (cond
              ((string? url)
               (string->url url))
              (else
               url))))

  (call/input-url
   url
   get-pure-port
   (lambda (ip)
     (dynamic-wind
         void
         (lambda () (ssax:xml->sxml ip '((atom . "http://www.w3.org/2005/Atom"))))
         (lambda () (close-input-port ip)))))))




(pretty-print
 (grab-rss-stuff  "http://planet.emacsen.org/atom.xml"))

(provide (all-defined))
)
