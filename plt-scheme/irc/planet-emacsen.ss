#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module planet-emacsen mzscheme
(require (only (lib "list.ss")
               last-pair
               sort)
         (only (lib "async-channel.ss")
               async-channel-put
               make-async-channel)
         (lib "trace.ss")
         (only (lib "etc.ss")
               this-expression-source-directory)
         (only (lib "1.ss" "srfi")
               filter)
         (only (lib "19.ss" "srfi" )
               current-date
               date->time-utc
               time<?
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
               string->url)
         "vprintf.ss")

(provide
 channel-of-entries-since
 entry->string
 )

(define-struct entry (timestamp title link) (make-inspector))

(define (trim str)
  (regexp-replace*
   (pregexp "(\r|\n)+")
   str
   ""))
;;(trace trim)

(define (planet-emacsen-news)
  (vtprintf "SNARFING REAL DATA FROM WEB!!!!!!!~%")
  (html->shtml
   (port->string (get-pure-port
                  (string->url "http://planet.emacsen.org/atom.xml")
                  (list)))))

(define (static-news-for-testing)
  (vtprintf "SNARFing test data from file.  Chill.~%")
  (html->shtml
   (call-with-input-file
       (build-path
        (this-expression-source-directory)
        "example-planet-emacsen.xml")
     port->string)))

;;(trace static-news-for-testing)
(define (entries-newer-than srfi-19-date)
  (internal-entries-newer-than (planet-emacsen-news) srfi-19-date))

(define (test-entries-newer-than srfi-19-date)
  (internal-entries-newer-than (static-news-for-testing) srfi-19-date))

;; returned entries are sorted oldest first.
(define (internal-entries-newer-than news srfi-19-date)

  (let ((entries
         ((sxpath '(feed entry))
          news)))

    (sort
     (filter
      (lambda (triplet)
        (time>?
         (date->time-utc (entry-timestamp triplet))
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
           (make-entry updated title link)))

       entries))

     (lambda (e1 e2)
       (time<?
        (date->time-utc (entry-timestamp e1))
        (date->time-utc (entry-timestamp e2)))))))
;(trace internal-entries-newer-than)
(define (entry->string triplet)
  (define (de-html str)
    (apply string-append ((sxpath '(// *text*)) (html->shtml str))))
  (format "~a: ~a"
          (de-html (entry-title triplet))
          (entry-link triplet)))

;; TODO -- consider exposing the thread, so that we can kill it.

;; Gaah -- beware multiple meanings of the word "channel".
(define (channel-of-entries-since srfi-19-date)

  ;; It's not clear that there's any point to limiting the size of the
  ;; channel ... I suppose it ensures that, in case people write blog
  ;; posts at a furious clip, and people are contantly yammering in
  ;; #emacs, we won't fill memory with un-announced blog posts :-)
  (let ((the-channel (make-async-channel 4)))
    (thread
     (lambda ()
       (let loop ((last-entry-time srfi-19-date))
         (let* ((entries (entries-newer-than last-entry-time))
                (time-of-latest-entry
                 (if (null? entries)
                     (current-date)
                   (car (last-pair entries)))))
           (for-each
            (lambda (e)
              (async-channel-put the-channel e))
            entries)
           (sleep 3600)
           (loop time-of-latest-entry))
         )))
    the-channel)  )
)
