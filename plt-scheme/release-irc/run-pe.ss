#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; I am just a driver for the planet-emacsen module; once it's
;; properly working, and integrated into the bot, I will no longer be
;; needed.

(module run-pe mzscheme
(require "planet-emacsen.ss"
         (lib "cmdline.ss")
         (only (lib "etc.ss")
               this-expression-source-directory)
         (lib "async-channel.ss")
         (rename (lib "19.ss" "srfi") 19:make-date make-date)
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
         (lib "pretty.ss")
         (only "globals.ss" *verbose*))

(parameterize ((*verbose* #t)
               (planet-emacsen-input-port
                (open-input-file
                 (build-path
                  (this-expression-source-directory)
                  "example-planet-emacsen.xml"))))
              (let ((the-channel
                     (queue-of-entries 'once)))
                (let loop ()
                  (let ((datum (async-channel-try-get the-channel)))
                    (cond
                     ((equal? datum 'no-more)
                      (printf "I guess that's all, then ~%"))
                     (datum
                      (printf "~a~%" (entry->string datum))
                      (loop))
                     (else
                      (printf "No data; sleeping~%")
                      (sleep 2)
                      (loop))))
                  )))


(newline))
