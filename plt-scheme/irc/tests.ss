#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module tests mzscheme
(require (lib "trace.ss")
         (lib "kw.ss")
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
               *verbose*
               ))

;; str [integer | 'all] -> str | (list of str)
(define get-retort
  (let ()
    (define (internal input which)
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
          )))
    (case-lambda
     ((input)
      (internal input 0))
     ((input which)
      (internal input which)))))

(*verbose* #f)
(define (p str)
  (write str)
  (newline)
  str)

;; recipient                                    what I call this kind of message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; channel, prefixed by *my-nick*
;; channel, not prefixed by *my-nick*
;; *my-nick*                                    <-- "private", aka QUERY
;; other nick                                   <-- we should never see one of these

(define/kw (send text
                 #:key
                 [source "unit-test"]
                 [recipient "#some-channel"]
                 #:allow-duplicate-keys
                 )
  (get-retort
   (p(format ":~a!n=~a@1.2.3.4 PRIVMSG ~a :~a"
           source source
           recipient
           text))))
(trace send)
(define (psend text . args)
  (apply send (cons text (cons '#:recipient (cons (*my-nick*) args)))))

(define (chsend source text)
  (send text #:source source))

(define utsend send)

(define *delimiter-char* (make-parameter #\:))
(define (say-to-bot text)
  (send (format "~a: ~a"
                (*my-nick*)
                text)
        ))
(define psay-to-bot psend)

(define tests
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

   ;; TODO -- join #emacs or #bots and see if it does its amusing
   ;; stuff (jordanb quotes, etc.)

   ;; TODO -- send it a NOTICE and make sure it does nothing

   ;; TODO -- send it a PING and see if it PONGs
   (test-suite
    "Feed it lines, see what it says"
    (test-equal?
     "silent unless spoken to, private message edition"
     (get-retort (format ":unit-test!~~unit-test@1.2.3.4 PRIVMSG somenick :hey you"))
     "")
    (test-equal?
     "silent unless spoken to, channel message edition"
     (utsend "hey you")
     "")
    (test-equal?
     "echoes back stuff addressed to it, private message edition"
     (psay-to-bot "hey you")
     "PRIVMSG unit-test :Well, unit-test, I think hey you too.")
    (test-equal?
     "echoes, channel message edition"
     (say-to-bot "hey you")
     "PRIVMSG #some-channel :Well, unit-test, I think hey you too.")
    (test-equal?
     "recognizes a comma after its nick"
     (parameterize ((*delimiter-char* #\,))
     (say-to-bot "hey you"))
     "PRIVMSG #some-channel :Well, unit-test, I think hey you too.")
    (test-case
     "Responds to VERSION CTCP request"
     (check-regexp-match
      #rx"NOTICE unit-test :\u0001VERSION .*:.*:.*\u0001"
      (psay-to-bot "\u0001VERSION\u0001")))

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
      (psay-to-bot "quote")))
    (test-case
     "witty quotes in response to a channel message"
     (check-regexp-match
      #rx"PRIVMSG #some-channel :.*heirs.*emacs.*johnw$"
      (say-to-bot "quote")))

    (test-case
     "the 'seen' command"

     (before
      (let ()

        (chsend "bob" "what up, cuz")
        (chsend "sam" "I got your back")
        (chsend "chris" "\u0001ACTION Gets bent\u0001")
        (chsend "bob" "I like Doritos")
        (callback (format
                   ":tim!n=tim@1.2.3.4 PRIVMSG ~a :\u0001ACTION confesses to the bot in private\u0001"
                   (*my-nick*))
                  (open-input-string "")
                  (open-output-string)))

      (check-regexp-match
       #rx"bob last spoke at .*, saying \"I like Doritos\"$"
       (say-to-bot "seen bob"))
      (check-regexp-match
       #rx"I haven't seen ted$"
       (say-to-bot "seen ted"))
      (check-regexp-match
       #rx"chris last acted at .*: chris Gets bent$"
       (say-to-bot "seen chris")
       "failed to notice ACTION")

      (check-regexp-match
       #rx"I haven't seen tim$"
       (say-to-bot "seen tim")
       "blabbed a secret")

      (check-regexp-match
       #rx"tim last acted at .*: tim confesses"
       (get-retort (format ":unit-test!~~unit-test@1.2.3.4 PRIVMSG ~a: seen tim"
                           (*my-nick*)))
       "failed to remind tim of his own (private) action")

      ;; ignores 'seen' unless there's an actual argument
      (check-equal?
       "PRIVMSG #some-channel :Well, unit-test, I think seen too."
       (say-to-bot "seen "))))
    (test-equal?
     "mostly ignores bots"
     (chsend  "slimebot"
              (format
               "~a~a ~a"
               (*my-nick*)
               (*delimiter-char*)
               "hey you"))
     "PRIVMSG #some-channel :\u0001ACTION holds his tongue.\u0001")
    )

   (test-suite
    "try to break it!!"
    (test-case
     "very long input"

     (before
      (callback
       (format ":bob!n=bob@1.2.3.4 PRIVMSG #some-channel :~a"
               (make-string 10000 #\!))
       (open-input-string "")
       (open-output-string))

      (check-equal?
       ""
       (get-retort ":unit-test!~~unit-test@1.2.3.4 PRIVMSG #some-channel :wozzup folks."))
      (check-not-false
       (< (string-length
           (say-to-bot "seen bob"))
          511)
       "failed to truncate a stored message")
      )))
   ))

(provide tests)
)