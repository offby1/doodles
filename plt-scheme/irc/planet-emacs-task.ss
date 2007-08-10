#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module planet-emacs-task mzscheme
(require (lib "async-channel.ss")
         (only  (lib "file.ss")
                get-preference
                put-preferences)
         (only (planet "rfc3339.ss" ("neil" "rfc3339.plt"))
               rfc3339-string->srfi19-date/constructor)
         (rename (lib "19.ss" "srfi") 19:make-date make-date)
         (only (lib "19.ss" "srfi")
               add-duration
               current-date
               date->time-utc
               make-time
               time-duration
               time-utc->date
               time>?
               )
         (only (lib "url.ss" "net")
               get-pure-port
               string->url)
         "planet-emacsen.ss"
         (only (planet "zdate.ss" ("offby1" "offby1.plt")) zdate)
         (only "globals.ss"
               *atom-timestamp-preference-name*
               *use-real-atom-feed?*)
         "vprintf.ss")

;; TODO -- probably parameterize this by URL, name of channel, and
;; procedure to reduce the atom feed to list of entries
(define (make-pe-consumer-proc)
  (let ((atom-feed (queue-of-entries
                    #:whence
                    (and (*use-real-atom-feed?*)
                         (lambda ()
                           (vtprintf "SNARFING REAL DATA FROM WEB!!!!!!!~%")
                           (get-pure-port
                            (string->url "http://planet.emacsen.org/atom.xml")
                            (list)))
                         )))
        (number-spewed 0)
        (time-of-latest-spewed-entry
         (date->time-utc
          (rfc3339-string->srfi19-date/constructor
           (or (get-preference (*atom-timestamp-preference-name*))
               "2000-00-00T00:00:00+00:00")
           19:make-date))))

    (printf "make-pe-consumer-proc: time-of-latest-spewed-entry is ~s~%"
            (zdate (time-utc->date time-of-latest-spewed-entry)))
    (lambda (output-proc)
      (let loop ()
        (let ((datum (async-channel-get atom-feed)))
          (vtprintf "Consumer thread: Just got ~s from our atom feed~%" (and datum (entry->string datum)))
          (vtprintf "Consumer thread: time-of-latest-spewed-entry is ~s~%"
                    (zdate (time-utc->date time-of-latest-spewed-entry)))
          ;; spew any _new_ entries that we haven't already spewed
          (if (time>?
               (entry-timestamp datum)
               time-of-latest-spewed-entry)
              (begin
                (vtprintf "consumer thread writing ~s to output proc~%"
                          (entry->string datum))

                (output-proc
                 (entry->string datum))

                (set! number-spewed (add1 number-spewed))
                (when (time>?
                       (entry-timestamp datum)
                       time-of-latest-spewed-entry)
                  (set! time-of-latest-spewed-entry
                        (entry-timestamp datum))
                  (let retry ()
                    (put-preferences
                     (list (*atom-timestamp-preference-name*))
                     (list
                      (zdate
                       (time-utc->date time-of-latest-spewed-entry)))
                     (lambda (lockpath)
                       (vtprintf "preference file is locked (~s); retrying~%" lockpath)
                       (sleep (/ (add1 (random 10)) 10))
                       (retry))))))
            (begin
              (vtprintf "Consumer thread: Nothing new on planet emacs (we already spewed an entry dated ~s) ~%"
                        (zdate (time-utc->date time-of-latest-spewed-entry)))
              (loop)))))
      )))
(provide (all-defined))
)