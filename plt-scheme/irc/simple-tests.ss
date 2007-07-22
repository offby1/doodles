#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module simple-tests mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
         (planet "util.ss" ("schematics" "schemeunit.plt" 2))
         (only (lib "1.ss" "srfi") third)
         (only (lib "13.ss" "srfi")
               string-tokenize
               )
         (only (lib "14.ss" "srfi")
               char-set
               char-set-complement
               )
         "bot.ss")

(define get-retort
  (case-lambda
   ((input)
    (get-retort input 0))
   ((input which)
    (let ((reaction (open-output-string)))
      (callback
       input
       (open-input-string "")
       reaction)
      (let ((lines (string-tokenize
                    (get-output-string reaction)
                    (char-set-complement (char-set #\newline)))))
        (cond
         ((not (null? lines))
          (list-ref lines which))
        (else #f))
        )))))

(test/text-ui
 (test-suite
  "Feed it lines, see what it says"
  (test-equal?
   "echoes back stuff addressed to it"
   (get-retort ":me!~me@1.2.3.4 PRIVMSG rudybot :hey you" 2)
   "PRIVMSG me :Well, me; I think :hey you too.")

  (test-equal?
   "more of the same."
   (get-retort ":me!~me@1.2.3.4 PRIVMSG rudybot :hey you")
   "PRIVMSG me :Well, me; I think :hey you too.")

  (test-case
   "Responds to VERSION CTCP request"
   (check-regexp-match
    #rx"NOTICE me :\u0001VERSION .*:.*:.*\u0001"
    (get-retort ":me!~me@1.2.3.4 PRIVMSG rudybot :\u0001VERSION\u0001")))
  ))
)