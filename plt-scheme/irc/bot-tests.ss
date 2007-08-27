#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui bot-tests 'verbose))"
|#
(module bot-tests mzscheme
(require (only (lib "pregexp.ss") pregexp-quote)
         (lib "trace.ss")
         (only (lib "19.ss" "srfi")
               current-time)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "bot.ss"
         "cached-channel.ss"
         "globals.ss"
         "headline.ss"
         "session.ss"
         "test-utils.ss"
         (only "planet-emacsen.ss"
               make-cached-channel
               *planet-poll-interval*)
         (only "parse.ss"
               parse-irc-message)
         "vprintf.ss")
(register-version-string "$Id$")

;; TODO -- write proper "check-response", so that it gives meaningful
;; spew when it fails
(define (got-response? sess ip input regexp)
  (respond (parse-irc-message input) sess)
  (expect/timeout ip regexp 3/2))

(define (fresh-session op)
  (let ((s (make-irc-session op)))
    (register-usual-services! s)
    s))

(define bot-tests

  (let-values (((ip op) (make-pipe #f "bot-tests")))

    (let ((sess #f))

      (test-suite
       "crap"
       #:before
       (lambda ()
         (*initial-channel-names* (list "#bots"))
         )
       #:after
       (lambda ()
         (custodian-shutdown-all (irc-session-custodian sess)))
       (test-case
        "join"
        (before
         (set! sess (fresh-session op))
         (respond
          (parse-irc-message ":server 001 :welcome")
          sess))

        (check-not-false
         (expect/timeout ip #rx"JOIN #bots" 1)
         "didn't join"))

       (test-case
        "short semi-private message"
        (before
         (set! sess (fresh-session op))

         (check-not-false
          (got-response?
           sess
           ip
           (format ":a!b@c PRIVMSG #d :~a: " (*my-nick*))
           (pregexp-quote "PRIVMSG #d :Eh? Speak up"))
          )))
       (test-case
        "replies go to the right place"
        (check-not-false
         (got-response?
          sess
          ip
          (format ":a!b@c PRIVMSG ~a :what are your plans this weekend?" (*my-nick*))
          #rx"PRIVMSG a :.*at a loss for words")))

       (test-case
        "backed-up idle events"

        (before
         (begin
           (*minimum-delay-for-periodic-spew* 1/10)
           (*planet-poll-interval* 2)
           ;; start the news thread
           (set! sess (fresh-session op))
           (respond (parse-irc-message ":x 366 rudybot #emacs :backed-up idle events'") sess))

         ;; ensure there is no news available.
         (set-irc-session-async-for-news! sess (make-cached-channel #f))

         ;; let the channel go idle.
         (vtprintf "Test is sleeping~%")

         ;; have someone say something on the channel.
         (respond (parse-irc-message ":a!b@c PRIVMSG #emacs :yo") sess)

         ;; now QUICKLY provide some news.
         (cached-channel-put
          (irc-session-async-for-news sess)
          (make-entry (current-time)
                      "JAPS BOMB PERL HARBOR"
                      "http://ruby-lang.org/mua/ha/ha"))

         ;; we should not see the bot spew the news, because the channel is
         ;; no longer idle.

         (check-false
          (expect/timeout ip #rx"JAPS" (* 3/4 (*minimum-delay-for-periodic-spew*))))))

       (test-suite
        "news in general"
        #:before
        (lambda ()
          ;; start the news thread

          ;; it's important to modify these parameters _before_ we
          ;; call fresh-session, since that call effectively makes
          ;; copies (in the new threads) of the parameters.
          (*planet-poll-interval* 2)
          (printf "Test is setting quote-and-headline-interval to 1/2 second~%") (flush-output)
          (*minimum-delay-for-periodic-spew* 1/10)

          (set! sess (fresh-session op))
          (respond (parse-irc-message ":x 366 rudybot #emacs :keeps saying 'no news'") sess))
        (test-case
         "keeps saying 'no news'"

         ;; ensure there is no news available.
         (set-irc-session-async-for-news! sess (make-cached-channel #f))

         ;; wait a smidge for the input-examiner proc to get
         ;; subscribed.
         (sleep 1/2)

         (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 1" (*my-nick*)) #rx"no news"))
         (sleep (*minimum-delay-for-periodic-spew*))
         (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 2" (*my-nick*)) #rx"no news"))
         (sleep (*minimum-delay-for-periodic-spew*))
         (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 3" (*my-nick*)) #rx"no news"))
         (sleep (*minimum-delay-for-periodic-spew*))
         (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 4" (*my-nick*)) #rx"no news"))
         (sleep (*minimum-delay-for-periodic-spew*))
         (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 5" (*my-nick*)) #rx"no news")))

        (test-case
         "items only appear once"

         (before
          (reliably-put-pref #f)

          ;; now put one item out there; expect to see it exactly once.
          (cached-channel-put
           (irc-session-async-for-news sess)
           (make-entry (current-time)
                       "SPACE ALIENS CAUSE GLOBAL WARMING"
                       "http://synasthesia.org")))

         (check-not-false
          (expect/timeout ip #rx"SPACE ALIENS" (* 2 (*minimum-delay-for-periodic-spew*)))
          "damn, it didn't appear the first time")
         (check-false
          (expect/timeout ip #rx"SPACE ALIENS" (* 4 (*minimum-delay-for-periodic-spew*)))
          "damn, it appeared a second time")
         )
        )

       ))))

(provide (all-defined))
)
