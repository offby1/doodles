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
         <id> tag:blogger.com,1999:blog-29447904.post-8169203024370114899 </id>
         <updated>
YOW!!!
         (zdate (seconds->date time))
         #<<YOW!!!
         </updated>
         <content type="html"> You stink. </content>
         <author> <name> jfm3 </name> <uri> http://jfm3-repl.blogspot.com/ </uri> </author>
         <source>
         <title type="html"> jfm3's journal </title>
         <link rel="self" href="http://jfm3-repl.blogspot.com/atom.xml"/>
         <id> tag:blogger.com,1999:blog-29447904 </id>
         <updated> 2007-07-22T17:46:09+00:00 </updated> </source>
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

;;(trace all-distinct?)
(define pe-tests
  (test-suite
   "planet.emacsen.org"
   (test-case
       "feed contains no dups"
       (before
        (lambda () (kill-all-tasks))
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
     (lambda () (kill-all-tasks))
     (if (file-exists? *atom-timestamp-file-name*)
         (begin
           (fprintf (current-error-port)
                    "file ~s exists; skipping a test~%"
                    *atom-timestamp-file-name*)
           (check-true #t))

       (parameterize ((*planet-task-spew-interval* 0))
         (let-values (((ip op) (make-pipe)))
           (respond "353 foo bar #bots" op)
           (check-not-false
            (expect/timeout
             ip
              (pregexp-quote "Michael Olson: [tech] Managing several radio feeds with MusicPD and Icecast")
             10)
            "No text from our news feed :-("
            ))
         ))))
   (test-case
    "Returns planet.emacsen.org news on demand"
    (before
     (lambda ()(kill-all-tasks))
     (parameterize
      ((*planet-task-spew-interval* 0)
;;        (*verbose* #t)
       )

      (check-regexp-match
       (pregexp-quote "http://yrk.livejournal.com/186492.html")
       (say-to-bot "news")
       "we didn't see the URL we wuz looking for"
       ))))
   ))

(provide pe-tests)
;;(*verbose* #t)
(exit (if (positive? (test/text-ui pe-tests 'verbose))
          1 0))
)