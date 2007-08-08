#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui tinyurl-task-tests))"
|#
(module tinyurl-task mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "bot.ss"
         "bot-tests.ss"
         (only "globals.ss"
               *log-output-port*
               verbose!)
         "tinyurl.ss")

(define (got-response? input regexp)
  (let-values (((ip op) (make-pipe)))
    (respond input op)
    (expect/timeout ip regexp 1)))

(define long-url "http://foo.bar/baz/i/hope/this/is/long/enough/its/really/quite/long")

(define tinyurl-task-tests

  (test-suite
   "tinyurl-task"
   (test-case
    "no response for a short URL"
    (check-false
     (got-response?
      ":x!y@z PRIVMSG #duh :http://foo.bar"
      #rx""))
   )

   (test-case
    "uses NOTICE, not PRIVMSG"
    (check-not-false
     (got-response?
      (format ":x!y@z PRIVMSG #duh :~a" long-url)
      #rx"NOTICE #duh :http://tinyurl.com/")
     ))

   (test-case
    "no response to a bot"
    (check-false
     (got-response?
      (format ":botbot!botbot@z PRIVMSG #duh :~a" long-url)
      #rx"")
     ))))

(provide (all-defined))
)
