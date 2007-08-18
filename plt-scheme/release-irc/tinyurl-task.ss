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
         "globals.ss"
         "tinyurl.ss")
(register-version-string "$Id$")

(define long-url
  (let loop ((kinda-long "http://foo.bar/baz/i/hope/this/is/long/enough"))
    (if (< (string-length kinda-long) (*tinyurl-url-length-threshold*))
        (loop (string-append kinda-long (format "/geez-louise~a" (string-length kinda-long))))
      kinda-long)))

(define tinyurl-task-tests

  (let-values (((ip op) (make-pipe #f "bot-tests")))
    (let ((sess (make-irc-session op)))
      (test-suite
       "tinyurl-task"
       (test-case
        "no response for a short URL"
        (check-false
         (got-response? sess ip
          ":x!y@z PRIVMSG #duh :http://foo.bar"
          #rx""))
        )

       (test-case
        "uses PRIVMSG, not NOTICE"
        (check-not-false
         (got-response? sess ip
          (format ":x!y@z PRIVMSG #duh :~a" long-url)
          #rx"PRIVMSG #duh :http://tinyurl.com/")
         ))

       (test-case
        "no response to a bot"
        (check-false
         (got-response? sess ip
          (format ":botbot!botbot@z PRIVMSG #duh :~a" long-url)
          #rx"")
         ))))))
(provide (all-defined))
)
