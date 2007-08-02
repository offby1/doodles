#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; TODO -- find a clean way to parameterize *atom-timestamp-file-name*
;; to "test-timestamp" while the tests run.

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
  (kill-all-tasks)
  (when (file-exists? (*atom-timestamp-file-name*))
    (delete-file (*atom-timestamp-file-name*))))

;;(trace all-distinct?)
(define original-timestamp-file-name #f)
(define pe-tests
  (test-suite
   "planet.emacsen.org"
   #:before (lambda () (set! original-timestamp-file-name (*atom-timestamp-file-name*))
                       (*atom-timestamp-file-name* "test-timestamp"))
   #:after  (lambda () (*atom-timestamp-file-name* original-timestamp-file-name))
   (test-case
    "Returns planet.emacsen.org news on demand"
    (before
     (test-prep)
     (parameterize (
                    ;; roughly, "never"
                    (*planet-task-spew-interval* 3600))

       (check-regexp-match
        #rx"no news yet"
        (say-to-bot "news"))

       ;; cause some news to get put into the async
       (respond "353 foo bar #bots" (open-output-string))

       (for-each (lambda (url)
                   (check-regexp-match

                    (pregexp-quote url)

                    (say-to-bot "news")
                    "we didn't see the URL we wuz looking for"
                    ))
                 (list
                  ;; this is the oldest item in our example xml
                  "http://yrk.livejournal.com/186492.html"

                  ;; of course this is the second-oldest
                  "http://ty.phoo"
                  )
       ))))

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

     (parameterize ((*planet-task-spew-interval* 0))
     (let-values (((ip op) (make-pipe)))
       (respond "353 foo bar #bots" op)
       (check-not-false
        (expect/timeout
         ip
         (pregexp-quote
          "Michael Olson: [tech] Managing several radio feeds with MusicPD and Icecast")
         10)
        "didn't find the headline we expected :-("
        )))))
   ))

(provide pe-tests)
)

;; to run just these from the command line:

;; mzscheme --no-init-file --mute-banner --version --require ./pe-tests.ss -p "text-ui.ss" "schematics" "schemeunit.plt" -e '(test/text-ui pe-tests)'
