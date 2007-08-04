#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module planet-emacs-task mzscheme
(require  (lib "async-channel.ss")
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
               *use-real-atom-feed?*)
         "vprintf.ss")
(define *atom-timestamp-file-name* (make-parameter "timestamp"))

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
           (or (and (file-exists? (*atom-timestamp-file-name*))
                    (call-with-input-file
                        (*atom-timestamp-file-name*)
                      (lambda (ip)
                        (printf "consumer task: reading timestamp file ~s: "
                                  (object-name ip))
                        (let ((rv (read ip)))
                          (printf "~s~%" rv)
                          rv))))
               "2000-00-00T00:00:00+00:00")
           19:make-date))))

    (printf "make-pe-consumer-proc: time-of-latest-spewed-entry is ~s~%"
            (zdate (time-utc->date time-of-latest-spewed-entry)))
    (lambda (op)
      (let loop ()
        (let ((datum (async-channel-try-get atom-feed)))
          (vtprintf "Consumer thread: Just got ~s from our atom feed~%" (and datum (entry->string datum)))
          (vtprintf "Consumer thread: time-of-latest-spewed-entry is ~s~%"
                    (zdate (time-utc->date time-of-latest-spewed-entry)))
          (when datum
            ;; spew any _new_ entries that we
            ;; haven't already spewed ... but
            ;; also spew the single newest entry
            ;; even if it's kind of old.
            (if (time>?
                 (entry-timestamp datum)
                 time-of-latest-spewed-entry)
                (begin
                  (vtprintf "consumer thread writing ~s to ~s~%"
                            (entry->string datum)
                            (object-name op))
                  (fprintf
                   op
                   "PRIVMSG #emacs :~a~%"
                   (entry->string datum))
                  (flush-output op)
                  (set! number-spewed (add1 number-spewed))
                  (when (time>?
                         (entry-timestamp datum)
                         time-of-latest-spewed-entry)
                    (set! time-of-latest-spewed-entry
                          (entry-timestamp datum))
                    (call-with-output-file
                        (*atom-timestamp-file-name*)
                      (lambda (op)
                        (write
                         (zdate
                          (time-utc->date time-of-latest-spewed-entry))
                         op))
                      'truncate/replace)))
              (begin
                (vtprintf "Consumer thread: Nothing new on planet emacs (we already spewed an entry dated ~s) ~%"
                          (zdate (time-utc->date time-of-latest-spewed-entry)))
                (loop))))))
      )))
(provide (all-defined))
)