#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module tests mzscheme
(require (lib "trace.ss")
         (lib "kw.ss")
         (only (lib "etc.ss")
               build-string
               this-expression-source-directory)
         (only (lib "pregexp.ss") pregexp-quote)
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
               char-set:whitespace
               )
         "bot.ss"
         (only "globals.ss"
               *initial-channel-names*
               *my-nick*
               *planet-task-spew-interval*
               *verbose*
               )
         "task.ss"
         "vprintf.ss")



;(trace expect/timeout)

;; str [integer | 'all] -> str | (list of str)
(define/kw (get-retort input #:key [which 0])
  (let ((lines (collect-output (lambda (op) (respond input op)))))
    (cond
     ((not (null? lines))
      (cond
       ((number? which)
        (list-ref lines which))
       ((eq? 'all which) lines)
       (else
        (error 'get-retort "wanted integer or 'all; got ~s" which))))
     (else ""))
    ))

(define/kw (send
            text
            #:key
            [source "unit-test"]
            [recipient "#some-channel"]
            #:allow-duplicate-keys)

  (get-retort
   (format ":~a!n=~a@1.2.3.4 PRIVMSG ~a :~a"
           source source
           recipient
           text)))

(define (psend text . args)
  (apply send (cons text (cons '#:recipient (cons (*my-nick*) args)))))

(define *whitespace-char* (make-parameter #\space))
(define/kw (say-to-bot
            text
            #:key
            [delimiter-char #\:]
            [source "unit-test"])
  (send (format "~a~a~a~a"
                (*my-nick*)
                delimiter-char
                (*whitespace-char*)
                text)
        #:source source))

(define (default-dumb-response recipient)
  (format "PRIVMSG ~a :\u0001ACTION is at a loss for words.\u0001"
          (cond
           ((string? recipient)
            recipient)
           (recipient "#some-channel")
           (else "some-nick"))))

(define (string->lines str)
  (string-tokenize
     str
     (char-set-complement (char-set #\newline))))

;; output-port? -> void -> (listof string?)
(define (collect-output func)
  (let ((string-port (open-output-string "op for testing")))
    (func string-port)
    (string->lines
     (get-output-string string-port))))

(define tests
  (test-suite
   "big ol' all-encompassing"

   (test-suite
    "logs in at startup"
    (test-case
     "sends NICK and USER at startup"

     (let ((lines (collect-output do-startup-stuff)))
       (check-true (pair? lines) "retort isn't a pair")
       (check-equal?
        (first  lines)
        (format "NICK ~a" (*my-nick*)))
       (check-regexp-match
        #rx"USER .* .* .* :.*"
        (second lines)))))
   (test-suite
    "excercise the \"respond\" function"

    (test-case
     "Responds to VERSION CTCP request"
     (check-regexp-match
      #rx"NOTICE unit-test :\u0001VERSION .*:.*:.*\u0001"
      (psend "\u0001VERSION\u0001")))

    ;; mwolson has forbidden it to mimic.
    (test-equal?
     "doesn't mimic under any circumstances"
     (send "\u0001ACTION glances around nervously.\u0001")
     "")

    (test-equal?
     "doesn't mimic bots either"
     (send "\u0001ACTION glances around nervously.\u0001" #:source "mebot")
     "")

    (test-case
     "witty quotes in response to a private message"
     (check-regexp-match
      #rx"PRIVMSG unit-test :.*heirs.*emacs.*johnw$"
      (psend "quote")))
    (test-case
     "witty quotes in response to a channel message"
     (check-regexp-match
      #rx"PRIVMSG #some-channel :.*heirs.*emacs.*johnw$"
      (say-to-bot "quote")))

    (test-case
     "witty quotes after being told to shaddap"
     (say-to-bot "shaddap")
     (check-regexp-match
      #rx"PRIVMSG #some-channel :.*heirs.*emacs.*johnw$"
      (say-to-bot "quote")))

    (test-case
     "tiny-ifies URLs"
     (check-regexp-match
      #rx"http://tinyurl.com/....., http://tinyurl.com/.....$"
      (say-to-bot "I'm telling ya, http://photo.net/foo?bar=baz, is, like rilly cool; http://microsoft.com is not")))
    (test-suite
     "the 'seen' command"
     #:before (lambda ()  (send "what up, cuz" #:source "bob") (send "I got your back" #:source "sam") (send "\u0001ACTION Gets bent\u0001" #:source "chris") (send "I like Doritos" #:source "bob") (psend "\u0001ACTION confesses to the bot in private\u0001"))
     (test-case
      "lotsa various ... uh ..."
      (check-regexp-match
       #rx"bob last spoke in #some-channel at .*, saying \"I like Doritos\"$"
       (say-to-bot "seen bob"))
      (check-regexp-match
       #rx"I haven't seen ted in #some-channel$"
       (say-to-bot "seen ted"))
      (check-regexp-match
       #rx"chris's last action in #some-channel was at .*: chris Gets bent$"
       (say-to-bot "seen chris")
       "failed to notice ACTION")

      (check-regexp-match
       #rx"I haven't seen tim in #some-channel$"
       (say-to-bot "seen tim")
       "blabbed a secret")

      (when #f
        (check-regexp-match
         #rx"tim last acted at .*: tim confesses"
         (psend "seen tim")
         "failed to remind tim of his own (private) action"))

      ;; ignores 'seen' unless there's an actual argument
      (check-equal?
       (default-dumb-response #t)
       (say-to-bot "seen "))))

    (test-equal?
     "mostly ignores bots"
     (say-to-bot "hey you" #:source "slimebot")
     "PRIVMSG #some-channel :\u0001ACTION holds his tongue.\u0001")
    )

   (test-suite
    "try to break it!!"
    (test-case
     "Survives a short CTCP request"
     (check-not-exn
      (lambda () (say-to-bot "\u0001GOATSEX\u0001"))))

    (test-case
     "very long input"

     (before
      (send (make-string 10000 #\!) #:source "bob")
      (check-equal?
       ""
       (send "wozzup folks."))
      (check-not-false
       (< (string-length
           (say-to-bot "seen bob"))
          511)
       "failed to truncate a stored message")
      ))

    (test-case
     "binary input"
     (let ((spam (build-string 256 integer->char)))
       (send spam #:source "spammer")
       (check-regexp-match
        (regexp
         (string-append
          "spammer last spoke in #some-channel at .*, saying \""
          ;; this seems fishy.  I don't know why it's splitting on
          ;; whitespace but it is.
          (pregexp-quote
           (first
            (string-tokenize
             spam
             (char-set-complement
              char-set:whitespace))))))

        (say-to-bot "seen spammer"))))

    (test-suite
     "short strings from server"
     (test-equal? "empty string" (get-retort "") "")
     (test-equal? "mostly empty string" (get-retort " ") "")
     (test-equal? "string with just one field" (get-retort "x") "")
     (test-equal? "numeric string with just one field" (get-retort "353") "")
     (test-equal? "string with just two fields" (get-retort "x y") "")
     (test-equal? "string with just three fields" (get-retort "x y z") ""))
    )

   ))

(provide
 collect-output
 expect/timeout
 get-retort
 say-to-bot
 string->lines
 tests)
)