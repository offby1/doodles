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

(define long-url
  (let loop ((kinda-long "http://foo.bar/baz/i/hope/this/is/long/enough"))
    (if (< (string-length kinda-long) (*tinyurl-url-length-threshold*))
        (loop (string-append kinda-long (format "/geez-louise~a" (string-length kinda-long))))
      kinda-long)))

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
