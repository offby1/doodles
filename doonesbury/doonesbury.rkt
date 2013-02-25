#lang racket

(require (planet neil/html-parsing:2:0)
          net/url
          racket/pretty ;; "You pretty now" -- D'Angelo Barkesdale
          (planet clements/sxml2:1:=3)
          browser/external
          )

;; The problem: I love to read Doonesbury, and I love reading stuff
;; via RSS.  However, the Doonesbury RSS feed points me to a page that
;; is so ugly, and so cluttered, that my heart sinks every time I look
;; at it (even though my beloved Doonesbury strip is indeed embedded
;; within).

;; This program therefore grabs just the image, without all the crap.
;; It starts with the URL to the RSS feed itself.

;; I got this by subscribing at doonesbury.com with Google Reader,
;; then exporting my Google Reader stuff through "Google Takeout"
(define *rss-feed-url* "http://www.gyford.com/misc/doonesburyrss.php")

(define (get-following-redirections url)
  (let-values ([(ip headers)
                (get-pure-port/headers url #:redirections 1)])
    ip))

(define (HTML-url-string->xexp url-string)
  (call/input-url
   (string->url url-string)
   get-following-redirections
   html->xexp))

(define (extract-image-url-string xexp)
  (let ([gold ((sxpath "//img[@class='strip']/@src") xexp)])
    (and (not (null? gold))
         (second (first gold)))))

(define (rss-URL-string->HTML-url-strings rss-url)
  (let ([sxml (call/input-url
               (string->url rss-url)
               get-following-redirections
               (curryr ssax:xml->sxml  '((x . "http://purl.org/rss/1.0/"))))])

    ;; See
    ;; http://planet.racket-lang.org/package-source/clements/sxml2.plt/1/3/planet-docs/sxml/sxpath.html

    ;; ... particularly the bit that says ``Handling of namespaces in
    ;; sxpath is a bit surprising''.  It sure is :-|
    (map second ((sxpath "//x:item/x:link" '((x . "x"))) sxml)))
  )

(module+ main
  (for-each displayln
            (map (compose extract-image-url-string HTML-url-string->xexp)
                 (rss-URL-string->HTML-url-strings *rss-feed-url*))))
