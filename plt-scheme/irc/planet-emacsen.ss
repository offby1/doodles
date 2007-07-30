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
         (only (lib "1.ss" "srfi")
               filter)
         (only (lib "19.ss" "srfi" )
               date->time-utc
               time-utc->date
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

         (only (planet "zdate.ss" ("offby1" "offby1.plt")) date->string)
         (planet "sxml.ss" ("lizorkin" "sxml.plt"))

         (only (lib "url.ss" "net")
               get-pure-port
               string->url)
         "vprintf.ss")

(provide
 entry->string
 entry-timestamp
 planet-emacsen-input-port
 queue-of-entries
 )

(define-struct entry (timestamp title link) (make-inspector))

(define planet-emacsen-input-port (make-parameter #f))

;; returned entries are sorted oldest first.

;; void -> (listof entry?)
(define (snarf-em-all)

  (sort
   (map
    (lambda (entry)
      (let* ((updated
              (date->time-utc
               (rfc3339-string->srfi19-date/constructor
                (car
                 ((sxpath '(updated *text*))
                  entry))
                19:make-date)))
             (title
              (car
               ((sxpath '(title *text*))
                entry)))
             (link
              (car
               ((sxpath '(link @ href *text*))
                entry))))
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
      (entry-timestamp e1)
      (entry-timestamp e2)))))

(define (entry->string entry)
  (define (de-html str)
    (apply string-append ((sxpath '(// *text*)) (html->shtml str))))
  (format "(~a) ~a: ~a"
          (date->string
           (time-utc->date (entry-timestamp entry) 0)
           "~A, ~B ~d ~Y ~k:~M ~z")
          (de-html (entry-title entry))
          (entry-link entry)))

(define (queue-of-entries . options)

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
           (if (and (not (null? options))
                    (eq? (car options)
                         'once))
               (async-channel-put the-channel 'no-more)
             (begin
               (sleep 3600)
               (loop))))
         )))
    the-channel)  )
)
