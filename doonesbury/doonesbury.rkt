#lang racket

;; The problem: I love to read Doonesbury, and I love reading stuff
;; via RSS.  However, the Doonesbury RSS feed points me to a page that
;; is so ugly, and so cluttered, that my heart sinks every time I look
;; at it (even though my beloved Doonesbury strip is indeed embedded
;; within).

;; This program therefore grabs just the image, without all the crap.

(require (planet neil/html-parsing:2:0)
          net/url
          racket/pretty ;; "You pretty now" -- D'Angelo Barkesdale
          (planet clements/sxml2:1:=3)
          browser/external
          )

(define (strip-url-string->xexp url-string)
  (call/input-url
   (string->url url-string)
   get-pure-port
   html->xexp))

(define (extract-image-url-string xexp)
  (let ([gold ((sxpath "//img[@class='strip']/@src") xexp)])
    (and (not (null? gold))
         (second (first gold)))))

;; http://www.gyford.com/misc/doonesburyrss.php
(module+ main
  (send-url
   (extract-image-url-string
    (strip-url-string->xexp  "http://doonesbury.slate.com/strip/archive/2013/02/17"))))
