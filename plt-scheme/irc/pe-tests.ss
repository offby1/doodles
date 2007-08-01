#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module pe-tests mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (prefix 19: (lib "19.ss" "srfi" ))
         (only "globals.ss"
               *planet-task-spew-interval*)
         (only "bot.ss"
               *atom-timestamp-file-name*
               kill-all-tasks
               respond
               )
         (only (lib "pregexp.ss") pregexp-quote)
         (only "tests.ss"
               get-retort
               say-to-bot
               string->lines))
(require/expose
 "planet-emacsen.ss"
 (
  entry?
  make-entry
  ))
(define (make-fake-entry)
  (make-entry (19:current-date)
              "Some title"
              "http://some.url.com"))
(define pe-tests
  (test-suite
   "planet.emacsen.org"
   (test-pred
    "it's an entry"
    entry?
    (make-fake-entry))
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
)