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
         "tinyurl.ss")

(define tinyurl-task-tests

  (test-suite
   "tinyurl-task"
   (test-case
    "no response for a short URL"
    (let-values (((ip op) (make-pipe)))
      (respond
        ":x!y@z PRIVMSG #duh :http://foo.bar"
       op)

      (check-false (expect/timeout ip
                                   #rx"doesn't matter"
                                   2))
      )
   )

   (test-case "no response to a bot"     (check-true #f "I haven't yet written the test!!"))
   (test-case "uses NOTICE, not PRIVMSG" (check-true #f "I haven't yet written the test!!"))))

(provide (all-defined))
)
