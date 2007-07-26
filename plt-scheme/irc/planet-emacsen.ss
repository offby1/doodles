#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module planet-emacsen mzscheme
(require (lib "kw.ss")
         (only (lib "list.ss")
               last-pair
               sort)
         (only (lib "async-channel.ss")
               async-channel-put
               make-async-channel)
         (lib "trace.ss")
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

         (only (planet "rfc3339.ss" ("neil" "rfc3339.plt"))
               rfc3339-string->srfi19-date/constructor)
         (only (planet "htmlprag.ss" ("neil" "htmlprag.plt" ))
               html->shtml)
         (only (planet "port.ss" ("schematics" "port.plt" ))
               port->string)

         (planet "sxml.ss" ("lizorkin" "sxml.plt"))

         (only (lib "url.ss" "net")
               get-pure-port
               string->url)
         "vprintf.ss")

(provide
 queue-of-entries
 entry->string
 entry-datestamp
 planet-emacsen-input-port
 )

(define-struct entry (datestamp title link) (make-inspector))

(define planet-emacsen-input-port (make-parameter #f))

;; returned entries are sorted oldest first.

;; void -> (listof entry?)
(define/kw (snarf-em-all)

  (sort
   (map
    (lambda (entry)
      (let* ((updated
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

    ((sxpath '(feed entry))

     ;; Hitmill to Shitmill / Port to String / I can debug / Anything
     ;; / ... Burma Shave
     (html->shtml
      (port->string
       (or (planet-emacsen-input-port)
           (begin
             (vtprintf "SNARFING REAL DATA FROM WEB!!!!!!!~%")
             (get-pure-port
              (string->url "http://planet.emacsen.org/atom.xml")
              (list))))))))

   (lambda (e1 e2)
     (time<?
      (date->time-utc (entry-datestamp e1))
      (date->time-utc (entry-datestamp e2))))))

(define (entry->string triplet)
  (define (de-html str)
    (apply string-append ((sxpath '(// *text*)) (html->shtml str))))
  (format "~a: ~a"
          (de-html (entry-title triplet))
          (entry-link triplet)))

;; TODO -- consider exposing the thread, so that we can kill it.

(define/kw (queue-of-entries)

  ;; It's not clear that there's any point to limiting the size of the
  ;; channel ... I suppose it ensures that, in case people write blog
  ;; posts at a furious clip, and people are contantly yammering in
  ;; #emacs, we won't fill memory with un-announced blog posts :-)
  (let ((the-channel (make-async-channel 4)))
    (thread
     (lambda ()
       (let loop ()
         (let* ((entries (snarf-em-all)))
           (for-each
            (lambda (e)
              (async-channel-put the-channel e))
            entries)
           (sleep 3600))
         )))
    the-channel)  )
)
