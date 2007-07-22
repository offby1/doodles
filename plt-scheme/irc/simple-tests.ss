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
         (only (lib "1.ss" "srfi")
               first
               second
               third)
         (only (lib "13.ss" "srfi")
               string-tokenize
               )
         (only (lib "14.ss" "srfi")
               char-set
               char-set-complement
               )
         "bot.ss"
         (only "globals.ss"
               *my-nick*
               *random?*))

(define get-retort
  (case-lambda
   ((input)
    (get-retort input 0))
   ((input which)
    (let ((reaction (open-output-string)))
      (printf "<= ~s~%" input)
      (callback
       input
       (open-input-string "")
       reaction)
      (let ((lines (string-tokenize
                    (get-output-string reaction)
                    (char-set-complement (char-set #\newline)))))
        (cond
         ((not (null? lines))
          (cond
           ((number? which)
            (list-ref lines which))
           ((eq? 'all which) lines)
           (else
            (error 'get-retort "wanted integer or 'all; got ~s" which))))
        (else #f))
        )))))

(test/text-ui
 (test-suite
  "big ol' all-encompassing"

  (test-suite
  "logs in at startup"
  (test-case
   "proper JOIN and NICK and whatnot"
   (let ((lines (get-retort "" 'all)))
     (check-equal?
      (first  lines)
      (format "NICK ~a" (*my-nick*)))
     (check-regexp-match
      #rx"USER .* .* .* :.*"
      (second lines)))))
 (test-suite
  "Feed it lines, see what it says"
  (test-equal?
   "echoes back stuff addressed to it"
   (get-retort (format ":me!~~me@1.2.3.4 PRIVMSG ~a :hey you" (*my-nick*)))
   "PRIVMSG me :Well, me; I think :hey you too.")

  (test-case
   "Responds to VERSION CTCP request"
   (check-regexp-match
    #rx"NOTICE me :\u0001VERSION .*:.*:.*\u0001"
    (get-retort (format ":me!~~me@1.2.3.4 PRIVMSG ~a :\u0001VERSION\u0001"
                        (*my-nick*)))))

  (test-equal?
   "simple mimicry"
   (parameterize ((*random?* #f))
                 (get-retort
                  ":me!~me@1.2.3.4 PRIVMSG #some-channel :\u0001ACTION glances around nervously.\u0001"))
   "PRIVMSG #some-channel :\u0001ACTION copies me and glances around nervously.\u0001")

  (test-equal?
   "doesn't mimic bots"
   (parameterize ((*random?* #f))
                 (get-retort
                  ":mebot!~me@1.2.3.4 PRIVMSG #some-channel :\u0001ACTION glances around nervously.\u0001"))
   "PRIVMSG #some-channel :Imagine I copied mebot by saying \"/me glances around nervously.\"")
  )))
)