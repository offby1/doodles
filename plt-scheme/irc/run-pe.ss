#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qr "$0" ${1+"$@"}
|#

;; I am just a driver for the planet-emacsen module; once it's
;; properly working, and integrated into the bot, I will no longer be
;; needed.

(require "planet-emacsen.ss"
         (only (lib "list.ss") last-pair)
         (lib "async-channel.ss")
         (rename (lib "19.ss" "srfi") 19:make-date make-date)
         (only (lib "19.ss" "srfi") current-date)
         (only (planet "rfc3339.ss" ("neil" "rfc3339.plt"))
               rfc3339-string->srfi19-date/constructor)
         (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (only (planet "zdate.ss" ("offby1" "offby1.plt"))
               PLT-date->srfi-19-date
               zdate)
         (only (lib "1.ss" "srfi")
               append-map
               filter

               third)
         (lib "pretty.ss"))

(define *some-time-ago*
  (rfc3339-string->srfi19-date/constructor
   "2007-07-16T19:59:00+00:00"
   19:make-date))

(let ((the-channel (make-async-channel 10)))
  (thread
   (lambda ()
     (let loop ((last-entry-time *some-time-ago*))
       (let* ((entries (test-entries-newer-than last-entry-time))
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

  (let loop ()
    (let ((datum (async-channel-try-get the-channel)))
      (if datum
          (pretty-print datum)
        (begin
          (printf "No data; sleeping~%")
          (sleep 2))))
    (loop)
    )  )


(newline)
