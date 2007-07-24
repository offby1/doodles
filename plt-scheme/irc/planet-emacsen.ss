#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module planet-emacsen mzscheme
(require (lib "trace.ss")
         (only (lib "etc.ss")
               this-expression-source-directory)
         (only (lib "1.ss" "srfi")
               filter
               first
               second
               third)
         (only (lib "19.ss" "srfi" )
               date->time-utc

               time>?)
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
 entries-newer-than
 entry->string
 planet-emacsen-news
 static-news-for-testing
 )

;; TODO -- define an "entry" structure instead of using "first",
;; "second", and "third".

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

(define (static-news-for-testing)
  (html->shtml
   (call-with-input-file
       (build-path
        (this-expression-source-directory)
        "example-planet-emacsen.xml") port->string)))

;; make sure this returns the entries with the oldest first, or at
;; least, document which order they come back in.
(define (entries-newer-than news srfi-19-date)

  (let ((entries
         ((sxpath '(entry))
          news)))

    (filter
     (lambda (triplet)
       (time>?
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
               (title
                (car
                 ((sxpath '(title *text*))
                  entry)))
               (link
                ((sxpath '(link @ href *text*))
                 entry)))
          (cons updated (cons title link))))

      entries))))
(define (entry->string triplet)
  (define (de-html str)
    (apply string-append ((sxpath '(// *text*)) (html->shtml str))))
  (format "~a: ~a"
          (de-html (second triplet))
          (third triplet)))
)
