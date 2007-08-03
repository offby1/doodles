#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module planet-emacsen mzscheme
(require (only (lib "etc.ss") this-expression-source-directory)
         (lib "kw.ss")
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

         "vprintf.ss")

(provide
 entry->string
 entry-timestamp
 queue-of-entries
 *planet-poll-interval*
 )

;; how often (in seconds) do we re-create and read the input port
(define *planet-poll-interval* (make-parameter 10))

(define-struct entry (timestamp title link) (make-inspector))

;; returned entries are sorted oldest first.

;; void -> (listof entry?)
(define (snarf-em-all ip)

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
      (begin0
        (port->string ip)
        (close-input-port ip)))))

   (lambda (e1 e2)
     (time<?
      (entry-timestamp e1)
      (entry-timestamp e2)))))
;;(trace snarf-em-all)
(define (entry->string entry)
  (define (de-html str)
    (apply string-append ((sxpath '(// *text*)) (html->shtml str))))
  (format "(~a) ~a: ~a"
          (date->string
           (time-utc->date (entry-timestamp entry) 0)
           "~A, ~B ~d ~Y ~k:~M ~z")
          (de-html (entry-title entry))
          (entry-link entry)))

(define/kw (queue-of-entries
            #:key
            [whence]
            [how-many 'continuous])

  (when (not whence)
    (set! whence
          (lambda ()
            (let ((fn (build-path
                       (this-expression-source-directory)
                       "example-planet-emacsen.xml")))
              (vtprintf "snarfing test data from ~s~%"
                        fn)

              (open-input-file fn)))))
  ;; It's not clear that there's any point to limiting the size of the
  ;; channel ... I suppose it ensures that, in case people write blog
  ;; posts at a furious clip, and people are contantly yammering in
  ;; #emacs, we won't fill memory with un-announced blog posts :-)
  (let ((the-channel (make-async-channel 4))
        (entries-put (make-hash-table 'equal)))
    (thread
     (lambda ()
       (let loop ()
         (vtprintf "queue-of-entries top of a loop~%")
         (for-each
          (lambda (e)
            ;; this is annoying -- apparently structures with equal
            ;; elements aren't themselves equal -- so I have to use
            ;; (entry->string e) as the hash table key, instead of
            ;; simply e.
            (when (not (hash-table-get entries-put (entry->string e) #f))
              (vtprintf "Planet producer thread about to put ~s onto the async ... "
                        (entry->string e))
              (async-channel-put the-channel e)
              (vtprintf "done~%")
              (hash-table-put! entries-put (entry->string e) #t)))
           (snarf-em-all (whence)))
         (if (eq? how-many 'once)
             (async-channel-put the-channel 'no-more)
           (begin
             (vtprintf "Planet producer thread sleeping ~a seconds before snarfing again~%"
                       (*planet-poll-interval*))
             (sleep (*planet-poll-interval*))
             (loop)))
         )))
    the-channel)  )
;;(trace queue-of-entries)
)
