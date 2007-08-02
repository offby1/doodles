#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module pe-tests mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "zdate.ss" ("offby1" "offby1.plt"))
         (prefix 19: (lib "19.ss" "srfi" ))
         (lib "async-channel.ss")
         (only "globals.ss"
               *planet-task-spew-interval*
               *verbose*)
         (only "bot.ss"
               *atom-timestamp-file-name*
               kill-all-tasks
               respond
               )
         (only (lib "pregexp.ss") pregexp-quote)
         (only "tests.ss"
               collect-output
               expect/timeout
               get-retort
               say-to-bot
               string->lines)
         (only "planet-emacsen.ss" queue-of-entries)
         "vprintf.ss")
(require/expose
 "planet-emacsen.ss"
 (
  entry?
  make-entry
  ))
(define make-fake-atom-xml
  (let ((time (current-seconds)))
    (lambda ()
      (begin0
        (string-append
         #<<YOW!!!
         <?xml version="1.0" encoding="utf-8" standalone="yes" ?>
         <feed xmlns="http://www.w3.org/2005/Atom">
         <entry>
         <title type="html"> Joseph Miklojcik: gnupg </title>
         <link href="http://jfm3-repl.blogspot.com/2007/07/gnupg.html"/>
YOW!!!
         "<updated>"
         (zdate (seconds->date time))
         "</updated>"
         #<<YOW!!!
         </entry>
         </feed>
YOW!!!
         )

      (set! time (add1 time)))))  )
;;(trace make-fake-atom-xml)
(define (stub-atom-feed)
  (let-values (((ip op) (make-pipe)))
    (display (make-fake-atom-xml) op)
    (display (make-fake-atom-xml) op)
    (close-output-port op)
    ip))
;;(trace stub-atom-feed)
(define (all-distinct? seq)
  (let ((ht (make-hash-table 'equal)))
    (let/ec return
      (for-each (lambda (item)
                  (when (hash-table-get ht item #f)
                    (return #f))
                  (hash-table-put! ht item #t))
                seq)
      #t)))

(define (test-prep)
  (*planet-task-spew-interval* 0)
  (printf "Killing all tasks~%")
  (kill-all-tasks)
  (when (file-exists? (*atom-timestamp-file-name*))
    (printf "Deleting ~s~%" (*atom-timestamp-file-name*))
    (delete-file (*atom-timestamp-file-name*))))

;;(trace all-distinct?)
(define pe-tests
  (test-suite
   "planet.emacsen.org"

   (test-case
    "feed contains no dups"
    (before
     (test-prep)
     (let ((feed (queue-of-entries #:whence stub-atom-feed #:how-many 'once)))
       (let loop ((items '()))
         (when (and (list? items)
                    (< (length items ) 10))
           (vtprintf "Getting from stub-atom-feed ... ")
           (let ((datum (async-channel-get feed)))
             (vtprintf "Snarfed ~s~%" datum)
             (when (entry? datum)
               (loop (cons datum items)))))
         (check-pred all-distinct?  items))
       )))

   (test-case
    "Occasionally spews planet.emacsen.org news to #emacs"
    (before
     (test-prep)

     (let-values (((ip op) (make-pipe)))
       (respond "353 foo bar #bots" op)
       (check-not-false
        (expect/timeout
         ip
         "."
         ;; (pregexp-quote
;;           "Michael Olson: [tech] Managing several radio feeds with MusicPD and Icecast"
;;           )
         10)
        "No text from our news feed :-("
        ))))

   ;; this is JUST AWFUL.  This test requires the previous test to
   ;; have run, since the previous test causes a crucial side-effect
   ;; -- namely, the accumulation of atom entries.
   (test-case
    "Returns planet.emacsen.org news on demand"
    (before
     (test-prep)
     (parameterize
         (
          ;;        (*verbose* #t)
          )

       ;; (check-regexp-match

;;         #rx"no news yet"
;;         (say-to-bot "news"))

       ;; cause some news to spew to the channel
       (respond "353 foo bar #bots" (open-output-string))
       (sleep 1/2)
       (check-regexp-match

        ;; this is the newest item in our example xml
        (pregexp-quote "http://yrk.livejournal.com/186492.html")

        (say-to-bot "news")
        "we didn't see the URL we wuz looking for"
        ))))
   ))

(provide pe-tests)
;;(*verbose* #t)
(exit (if (positive? (parameterize ((*atom-timestamp-file-name* "test-timestamp"))
                     (test/text-ui pe-tests 'verbose)))
          1 0))
)