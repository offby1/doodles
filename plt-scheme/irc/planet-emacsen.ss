#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui planet-tests 'verbose))"
|#

(module planet-emacsen mzscheme
(require (lib "async-channel.ss")
         (lib "trace.ss")
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
         "headline.ss"
         "globals.ss"
         "thread.ss"
         "vprintf.ss")

;; how often (in seconds) do we re-create and read the input port
(define *planet-poll-interval* (make-parameter 3600))

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
;; this is for display purposes, not for serializing.

(define (entry->string entry)
  (define (de-html str)
    (apply string-append ((sxpath '(// *text*)) (html->shtml str))))
  (format "(~a) ~a: ~a"
          (date->string
           (time-utc->date (entry-timestamp entry) 0)
           "~A, ~B ~d ~Y ~k:~M ~z")
          (de-html (entry-title entry))
          (entry-link entry)))

;; so we want to ensure that we never put the same article onto the
;; async-channel twice.  I can't think of a simple way to absolutely
;; guarantee that, but I can think of a simple way to come pretty
;; darned close:

;; - every time we put an article onto the channel, we save its
;; timestamp in a "prefernce" -- that's on-disk storage, so it'll be
;; there after we exit.

;; - also when we put an article onto the channel, we save the whole
;; article into a hash table.

;; before we put an article onto the channel, we check its timestamp
;; against the saved one -- if it's older, we skip it.  And if it's
;; not older, we check the hash table: if it's in there, we skip it
;; too.

;; Now the only way we'll put the same article in twice is if we shut
;; down after putting article A in the async-channel, then start up
;; again before that article vanishes from the output of our atom
;; feed.  This will actually happen pretty often, but oh well.

(define (reliably-put-pref value)
  (let retry ()
    (put-preferences
     (list (*atom-timestamp-preference-name*))
     (list value)
     (lambda (lockpath)
       (vtprintf "preference file is locked (~s); retrying~%" lockpath)
       (sleep (/ (add1 (random 10)) 10))
       (retry)))))

;; idea -- don't just return this queue; instad, return a structure
;; that contains the queue _and_ a "cache" (just one entry) of the
;; most-recently-gotten entry.  Thus we'd have to write a little
;; replacement for async-channel-get that would fill the cache when
;; needed, and return values from it when asked.
(define/kw (queue-of-entries
            #:key
            [whence])

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

        ;; keep track of each entry we put on the async-channel, so
        ;; that we never put the same entry on twice.
        (entries-put (make-hash-table 'equal)))
    (my-thread
     (lambda ()
       (let loop ()

         ;; keep track of the time of each entry, too.  This plus the
         ;; hash table seem like overkill, but note that the
         ;; preference persists on disk even after our process exits,
         ;; whereas the hash table doesn't (because I'm too lazy to
         ;; bother saving it).
         (let ((time-of-last-entry-put
                (or (get-preference (*atom-timestamp-preference-name*))
                    0)))
           (for-each
            (lambda (e)
              ;; put this headline in the channel if it's newer than
              ;; any we've previously seen, AND we haven't already
              ;; seen it.
              (if (and (> (time-second (entry-timestamp e))
                         time-of-last-entry-put)
                       (not (hash-table-get entries-put e #f)))

                  (begin
                    (reliably-put-pref (time-second (entry-timestamp e)))
                    (hash-table-put! entries-put e #t)
                    (async-channel-put the-channel e))))

            (snarf-em-all (whence))))

         (sleep (*planet-poll-interval*))
         (loop))))

    the-channel)  )

;;(trace queue-of-entries)



(define (async->list as)
  (let loop ((what-we-found '()))
    (let ((datum (sync/timeout 1/10 as)))

      (if datum
          (loop (cons datum what-we-found))
        what-we-found))))
;;(trace async->list)

;; the key thing about this data is that there's more than one entry,
;; they have the same timestamp, but otherwise their content is
;; different.
(define (preloaded-input-port)
  (let-values (((ip op) (make-pipe)))
    (display  #<<THASSALL
<feed>
<entry>
<title >Yo vinnie</title>
<link href="http://vincent.it"/>
<updated>2000-00-00T00:00:00+00:00</updated>
</entry>
<entry>
<title>Whassup</title>
<link href="http://dawg.za"/>
<updated>2000-00-00T00:00:00+00:00</updated>
</entry>
</feed>
THASSALL
op)
    (newline op)
    (close-output-port op)
    ip)
  )

(define planet-tests

  (test-suite
   "planet"

   (test-case
    "doesn't duplicate headlines"
    (before
     (begin
       (*use-real-atom-feed?* #f)
     (put-preferences (list (*atom-timestamp-preference-name*))
                      (list #f)))
     ;; create two entries with identical times but different content.

    ;; delete the preference
    (put-preferences (list (*atom-timestamp-preference-name*))
                     (list #f))

    ;; create a fake atom feed for queue-of-entries to read

    ;; stuff both entries into the fake feed

    (let ((q (queue-of-entries #:whence preloaded-input-port)))
      (check-equal? (length (async->list q))
                    2
                    "didn't get both entries"))
    (let ((q (queue-of-entries #:whence preloaded-input-port)))
      ;; now stuff both entries back into the fake feed, and pull
      ;; again; you should see nothing.

      (check-equal? (length (async->list q)) 0
                    "nuts! not exactly zero entries")))
    )
   (test-case
    "delivers an entry raht quick-like"
    (before
     (begin
       (*use-real-atom-feed?* #f)
       (put-preferences (list (*atom-timestamp-preference-name*))
                        (list #f)))
     (let ((q (queue-of-entries #:whence #f)))
       (let ((first-entry (sync q))
             (time-of-last-entry-put (get-preference (*atom-timestamp-preference-name*))))

         (check-pred entry? first-entry "It's not an entry!!")
         (check-not-false time-of-last-entry-put "queue didn't save a preference")))))))


(provide
 entry->string
 entry-timestamp
 planet-tests
 queue-of-entries
 *planet-poll-interval*
 )
)

