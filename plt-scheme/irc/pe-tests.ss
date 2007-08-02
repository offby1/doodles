#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module pe-tests mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
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
               get-retort
               say-to-bot
               string->lines)
         (only "planet-emacsen.ss" queue-of-entries))
(require/expose
 "planet-emacsen.ss"
 (
  entry?
  make-entry
  ))
(define (make-fake-entry)
  #<<YOW!!!
    <entry>
    <title type="html"> Joseph Miklojcik: gnupg </title>
    <link href="http://jfm3-repl.blogspot.com/2007/07/gnupg.html"/>
    <id> tag:blogger.com,1999:blog-29447904.post-8169203024370114899 </id>
    <updated> 2007-07-16T23:57:35+00:00 </updated>
    <content type="html"> You stink. </content>
    <author> <name> jfm3 </name> <uri> http://jfm3-repl.blogspot.com/ </uri> </author>
    <source>
      <title type="html"> jfm3's journal </title>
      <link rel="self" href="http://jfm3-repl.blogspot.com/atom.xml"/>
      <id> tag:blogger.com,1999:blog-29447904 </id>
      <updated> 2007-07-22T17:46:09+00:00 </updated> </source>
  </entry>
YOW!!!
  )

(define (stub-atom-feed)
  (let-values (((ip op) (make-pipe)))
    (display (make-fake-entry) op)
    (display (make-fake-entry) op)
    ip))

(define (all-distinct? seq)
  (let ((ht (make-hash-table 'equal)))
    (let/ec return
      (for-each (lambda (item)
                  (when (hash-table-get ht item #f)
                    (return #f))
                  (hash-table-put! ht item #t))
                seq)
      #t)))

(define pe-tests
  (test-suite
   "planet.emacsen.org"
   (test-case
    "feed contains no dups"
    (let ((feed (queue-of-entries #:whence stub-atom-feed)))
      (let loop ((items '()))
        (if (< (length items ) 10)
            (let ((datum (async-channel-get feed)))
              (printf "Snarfed ~s~%" datum)
              (loop datum))
          (check-true all-distinct? items)))
      ))
   (test-case
    "Occasionally spews planet.emacsen.org news to #emacs"
    (if (file-exists? *atom-timestamp-file-name*)
        (begin
          (fprintf (current-error-port)
                   "file ~s exists; skipping a test~%"
                   *atom-timestamp-file-name*)
          (check-true #t))
      (let ((op (open-output-string)))
        (parameterize ((*planet-task-spew-interval* 0))
                      (kill-all-tasks)
                      (respond "353 foo bar #bots" op)
                      (sleep 1/10))
        (let ((newstext (get-output-string op)))
          (check-false (null? newstext)
                       "No text from our news feed :-(")
          (check-regexp-match
           #rx"^PRIVMSG #emacs :"
           (car (string->lines newstext))
           "didn't spew to #emacs")
          (check-regexp-match

           ;; this matches the oldest item in our sample Atom data
           (pregexp (pregexp-quote "Michael Olson: [tech] Managing several radio feeds with MusicPD and Icecast"))

           newstext)))))
   (test-case
    "Returns planet.emacsen.org news on demand"
    (parameterize
     ((*planet-task-spew-interval* 0))
     (kill-all-tasks)
     (get-retort "353 foo bar #emacs")
     (sleep 1/10)

     (let ((recent-news (say-to-bot "news")))
       ;; these match the newest items in the sample Atom data
       (check-regexp-match (pregexp (pregexp-quote "http://yrk.livejournal.com/186492.html")) recent-news)
       (check-regexp-match (pregexp (pregexp-quote "http://feeds.feedburner.com/~r/sachac/~3/136355742/2007.07.22.php")) recent-news)
       (check-regexp-match (pregexp (pregexp-quote "http://blog.mwolson.org/tech/trying_to_get_emacs22_into_gutsy__part_3.html")) recent-news)
       ))))
  )
(provide pe-tests)
(*verbose* #t)
(exit  (if (positive? (test/text-ui
                       pe-tests))
           1
         0))
)