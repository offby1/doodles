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
               *my-nick*))

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
          (cond
           ((number? which)
            (list-ref lines which))
           ((eq? 'all which) lines)
           (else
            (error 'get-retort "wanted integer or 'all; got ~s" which))))
        (else ""))
        )))))

(define simple-tests
  (test-suite
   "big ol' all-encompassing"

   (test-suite
    "logs in at startup"
    (test-case
     "proper NICK and whatnot"
     (let ((lines (get-retort "" 'all)))
       (check-equal?
        (first  lines)
        (format "NICK ~a" (*my-nick*)))
       (check-regexp-match
        #rx"USER .* .* .* :.*"
        (second lines))))

    ;; TODO -- send it a 001 message and see that it JOINs the list o'
    ;; channels
    )

   ;; TODO -- send it a 353 and then see that it has filled in its
   ;; hash table with reasonable values

   ;; TODO -- send it a PING and see if it PONGs
   (test-suite
    "Feed it lines, see what it says"
    (test-equal?
     "silent unless spoken to, private message edition"
     (get-retort (format ":unit-test!~~unit-test@1.2.3.4 PRIVMSG somenick :hey you"))
     "")
    (test-equal?
     "silent unless spoken to, channel message edition"
     (get-retort (format ":unit-test!~~unit-test@1.2.3.4 PRIVMSG #some-channel :hey you"))
     "")
    (test-equal?
     "echoes back stuff addressed to it, private message edition"
     (get-retort (format ":unit-test!~~unit-test@1.2.3.4 PRIVMSG ~a :hey you" (*my-nick*)))
     "PRIVMSG unit-test :Well, unit-test, I think hey you too.")
    (test-equal?
     "echoes, channel message edition"
     (get-retort (format
                  ":unit-test!~~unit-test@1.2.3.4 PRIVMSG #some-channel :~a: hey you"
                  (*my-nick*)))
     "PRIVMSG #some-channel :Well, unit-test, I think hey you too.")
    (test-equal?
     "recognizes a comma after its nick"
     (get-retort (format
                  ":unit-test!~~unit-test@1.2.3.4 PRIVMSG #some-channel :~a, hey you"
                  (*my-nick*)))
     "PRIVMSG #some-channel :Well, unit-test, I think hey you too.")
    (test-case
     "Responds to VERSION CTCP request"
     (check-regexp-match
      #rx"NOTICE unit-test :\u0001VERSION .*:.*:.*\u0001"
      (get-retort (format ":unit-test!~~unit-test@1.2.3.4 PRIVMSG ~a :\u0001VERSION\u0001"
                          (*my-nick*)))))

    ;; mwolson has forbidden it to mimic.
    (test-equal?
     "doesn't mimic under any circumstances"
     (get-retort
      ":unit-test!~unit-test@1.2.3.4 PRIVMSG #some-channel :\u0001ACTION glances around nervously.\u0001")
     "")

    (test-equal?
     "doesn't mimic bots either"
     (get-retort
      ":mebot!~unit-test@1.2.3.4 PRIVMSG #some-channel :\u0001ACTION glances around nervously.\u0001")
     "")

    (test-case
     "witty quotes in response to a private message"
     (check-regexp-match
      #rx"PRIVMSG unit-test :.*heirs.*emacs.*johnw$"
      (get-retort (format ":unit-test!~~unit-test@1.2.3.4 PRIVMSG ~a :quote"
                          (*my-nick*)))))
    (test-case
     "witty quotes in response to a channel message"
     (check-regexp-match
      #rx"PRIVMSG #some-channel :.*heirs.*emacs.*johnw$"
      (get-retort (format ":unit-test!~~unit-test@1.2.3.4 PRIVMSG #some-channel :~a: quote"
                          (*my-nick*)))))
    )))
(provide simple-tests)
)