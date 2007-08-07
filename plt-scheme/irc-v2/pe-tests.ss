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
               *my-nick*
               *planet-task-spew-interval*
               verbose!)
         (only "bot.ss"
               *tasks-by-channel*
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
         (only "planet-emacs-task.ss"
               *atom-timestamp-file-name*
               make-pe-consumer-proc)
         (only "task.ss"
               kill
               make-task
               task-unsuspend
               )
         "vprintf.ss")
(require/expose
 "planet-emacsen.ss"
 (
  entry?
  make-entry
  ))

(define (kill-all-tasks)
  (hash-table-for-each
   *tasks-by-channel*
   (lambda (channel-name task)
     (kill task))))
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
  (let-values (((ip op) (make-pipe #f "stub ip" "stub op")))
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

(define *headlines-source* (make-parameter #f))

(define (test-prep)
  (*planet-task-spew-interval* 0)
  (kill-all-tasks)
  (if (file-exists? (*atom-timestamp-file-name*))
    (begin
      (vtprintf "Deleted ~s~%" (*atom-timestamp-file-name*))
      (delete-file (*atom-timestamp-file-name*)))
    (vtprintf "No file named ~s~%"
              (*atom-timestamp-file-name*)))

  (let-values (((ip op) (make-pipe
                         #f
                         "read headlines from here"
                         "headlines get writ here")))
    (let* ((consumer (make-pe-consumer-proc))
           (t (make-task 'headline-spewer-task
                        (* 60 20)
                        (lambda ()
                          (consumer op)))))
      (hash-table-put!
       *tasks-by-channel*
       "#emacs"
       t)

      ;; ick.  We need to wait awhile to ensure the "producer"
      ;; (the task that puts entries onto the async channel) has
      ;; started; otherwise our saying "news" to the bot will have
      ;; no effect, and we'll have to wait an hour.
      (sleep 1/3)

      (task-unsuspend t))
    (*headlines-source* ip)))

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

       ;; cause some news to get put into the async
       (check-not-false
        (begin
          (respond "353 foo bar #bots" (open-output-string "/dev/null"))

          (say-to-bot "news")

          (expect/timeout
           (*headlines-source*)
           (pregexp-quote
            "Michael Olson: [tech] Managing several radio feeds with MusicPD and Icecast")
           3))

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
           (*headlines-source*)
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
