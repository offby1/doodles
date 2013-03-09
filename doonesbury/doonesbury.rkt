#lang racket

(require (planet neil/html-parsing:2:0)
          net/url
          racket/pretty ;; "You pretty now" -- D'Angelo Barksdale
          (planet clements/sxml2:1:=3)
          browser/external
          "modify-rdf.rkt"
          )

(module+ test
  (require rackunit))

;; The problem: I love to read Doonesbury, and I love reading stuff
;; via RSS.  However, the Doonesbury RSS feed points me to a page that
;; is so ugly, so cluttered, so slow to load, that my heart sinks
;; every time I look at it (even though my beloved Doonesbury strip is
;; indeed embedded within).  (Take a look at
;; http://doonesbury.slate.com/strip/archive/2013/02/17, for example).

;; So this takes the existing RSS feed, and transforms it a little, by
;; replacing the links to the awful page, with links to just the bare
;; images.  Voila!

;; Berry starts it off with the URL to the RSS feed itself.

;; I got this by subscribing at doonesbury.com with Google Reader,
;; then exporting my Google Reader stuff through "Google Takeout"
(define *rss-feed-url* "http://www.gyford.com/misc/doonesburyrss.php")

;; Just like "get-pure-port", except it transparently follows HTTP
;; redirects.
(define (get-following-redirections url)
  (let-values ([(ip headers)
                (get-pure-port/headers url #:redirections 1)])
    ip))

(define (progress thunk op fmt . vals)
  (apply fprintf op fmt vals)
  (let ([val (thunk)])
    (newline op)
    val))

;; Analagous to call-with-input-file
(define (call-with-input-URL url-string input-port-reader)
  (progress (thunk
             (call/input-url
              (string->url url-string)
              get-following-redirections
              input-port-reader))
            (current-error-port)
            "Downloading from ~a..."
            url-string))

(define *rss-namespace-abbrev* '((x . "http://purl.org/rss/1.0/")
                                 (rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")))

;; Given parsed XML that came from a slow ugly page at Slate.com, use
;; some xpath magic to find the URL of a "bare" Doonesbury image.
;; This is the heart of it all, and may well need to be tweaked as
;; time passes, and as Slate redesigns their page.
(define (extract-image-url-string xexp)
  (let ([gold ((sxpath "//img[@class='strip']/@src") xexp)])
    (and (not (null? gold))
         (second (first gold)))))

;; Given SXML that represents a parsed RSS feed, extract the text of
;; "x:link" elements whose parent is an "x:item" element.
(module+ test
  (let ([sxml
         '(*TOP*
           bogon1
           (x:item (x:link "whoa"))
           (bogon number two)
           (x:item (x:link "Nellie"))
           )])
    (check-equal? (extract-link-URLs sxml 'x) '("whoa" "Nellie"))))

(define (extract-link-URLs sxml namespace-prefix-symbol)

  ;; See
  ;; http://planet.racket-lang.org/package-source/clements/sxml2.plt/1/3/planet-docs/sxml/sxpath.html

  ;; ... particularly the bit that says ``Handling of namespaces in
  ;; sxpath is a bit surprising''.  It sure is :-|
  (map second ((sxpath (format "//~a:item/~a:link"
                               namespace-prefix-symbol
                               namespace-prefix-symbol)
                       `((,namespace-prefix-symbol . ,(format "~a" namespace-prefix-symbol))))
               sxml)))

;; When #t, reads from some files in this directory, rather than downloading.
(define *demo-mode* #t)

(define ssax-parse-from-input-port
  (curryr ssax:xml->sxml *rss-namespace-abbrev*))

;; Given the URL of an ugly Slate page holding a particular Doonesbury
;; strip, download the page, extract the URL of just the image, and
;; return that.
(define (improve-doonesbury-url url-string)
  (extract-image-url-string
   (if *demo-mode*
       (call-with-input-file "strip-archive-2013-02-17.html" html->xexp)
       (call-with-input-URL url-string html->xexp))))

(module+ main
  (define (parsed-feed)
    (if *demo-mode*
        (call-with-input-file "sample-rss-feed.xml" ssax-parse-from-input-port)
        (call-with-input-URL *rss-feed-url* ssax-parse-from-input-port)))

  (srl:sxml->xml
   (modify-RDF-item-links (parsed-feed) improve-doonesbury-url)
   (current-output-port)))
