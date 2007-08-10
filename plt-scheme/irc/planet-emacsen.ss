#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui planet-tests 'verbose))"
|#

(module planet-emacsen mzscheme
(require (lib "trace.ss")
         (only  (lib "file.ss")
                get-preference
                put-preferences)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))

         (only (lib "etc.ss") this-expression-source-directory)
         (lib "kw.ss")
         (only (lib "list.ss")
               last-pair
               sort)
         (only (lib "async-channel.ss")
               async-channel-put
               make-async-channel)
         (lib "trace.ss")
         (only (lib "1.ss" "srfi")
               first second third fourth
               filter)
         (only (lib "19.ss" "srfi" )
               date->time-utc
               make-time
               time-nanosecond
               time-second
               time-type
               time-utc->date
               time=?
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

         (only (planet "zdate.ss" ("offby1" "offby1.plt"))
               date->string
               zdate)
         (planet "sxml.ss" ("lizorkin" "sxml.plt"))

         "globals.ss"
         "vprintf.ss")

;; how often (in seconds) do we re-create and read the input port
(define *planet-poll-interval* (make-parameter 3600))

(define-struct entry (timestamp title link) (make-inspector))
(define (entry-hash e)
  (equal-hash-code (cons (entry-timestamp e)
                         (cons (entry-title e)
                               (entry-link e)))))
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

(define (reliably-put-pref value)
  (let retry ()
    (put-preferences
     (list (*atom-timestamp-preference-name*))
     (list value)
     (lambda (lockpath)
       (vtprintf "preference file is locked (~s); retrying~%" lockpath)
       (sleep (/ (add1 (random 10)) 10))
       (retry)))))

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
  (let ((the-channel (make-async-channel #f))
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
              (vtprintf "Planet producer thread about to put ~s onto the async ... ~%"
                        (entry->string e))
              (reliably-put-pref (list (time-type (entry-timestamp e))
                                       (time-nanosecond (entry-timestamp e))
                                       (time-second (entry-timestamp e))
                                       (entry-hash e)))
              (async-channel-put the-channel e)
              (vtprintf "ppt: done~%")
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



(define planet-tests

  (test-suite
   "planet"
   #:before
   (lambda ()
     (*use-real-atom-feed?* #f)
     (put-preferences (list (*atom-timestamp-preference-name*))
                      (list #f)))

   (test-case
    "delivers an entry raht quick-like"
    (verbose!)
    (let ((q (queue-of-entries #:whence #f)))
      (let ((first-entry (sync q))
            (leftover-headline-stamp (get-preference (*atom-timestamp-preference-name*))))

        (check-pred entry? first-entry "It's not an entry!!")
        (check-not-false leftover-headline-stamp "queue didn't save a preference")
        (printf "First entry: ~s; hash: ~s~%"
                first-entry
                (entry-hash first-entry))

        (let ((t (make-time (first leftover-headline-stamp)
                            (second leftover-headline-stamp)
                            (third leftover-headline-stamp)))
              (hash (fourth leftover-headline-stamp)))

          (printf "time from disk: ~s; hash from disk: ~s~%"
                  t hash)

          (if (time=? t (entry-timestamp first-entry))
              (check-false (equal? hash (entry-hash first-entry))
                           "we just got the same headline that was saved on disk!!")
            (check-true (time<? (entry-timestamp first-entry) t)
                        "we just got a headline older than the one on disk!!")))))

    )))

(provide
 entry->string
 entry-timestamp
 planet-tests
 queue-of-entries
 *planet-poll-interval*
 )
)
