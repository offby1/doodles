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
         (only "planet-emacsen.ss"
               make-cached-channel
               *planet-poll-interval*)
         (only "parse.ss"
               parse-irc-message)
         "vprintf.ss")
(register-version-string "$Id$")

;; returns #f if we didn't find what we're looking for.

(define (expect/timeout ip regex seconds)
  (let* ((ch (make-channel))
         (reader
          (thread
           (lambda ()
             (let loop ()
               (vtprintf "expect/timeout about to look for ~s from ~s ...~%"
                        regex
                        (object-name ip))
               (let ((line (read-line ip)))
                 (vtprintf "expect/timeout got ~s~%" line)
                 (cond
                  ((eof-object? line)
                   (channel-put ch #f))
                  ((regexp-match regex line)
                   (vtprintf "expect/timeout: Got match!~%")
                   (channel-put ch #t))
                  (else
                   (vtprintf "expect/timeout: nope; retrying~%")
                   (loop)))

                 ))))))
    (and (sync/timeout seconds ch)
         ch)))

;; TODO -- write proper "check-response", so that it gives meaningful
;; spew when it fails
(define (got-response? sess ip input regexp)
  (respond (parse-irc-message input) sess)
  (expect/timeout ip regexp 3/2))

(define bot-tests

  (let-values (((ip op) (make-pipe #f "bot-tests")))

    (let ((sess (make-irc-session op)))

      (verbose!)
      (test-suite
       "crap"
       #:before
       (lambda ()
         (*initial-channel-names* (list "#bots")))
       (test-case
        "join"
        (respond
         (parse-irc-message ":server 001 :welcome")
         sess)
        (check-not-false
         (expect/timeout ip #rx"JOIN #bots" 1)
         "didn't join"))
       (test-case
        "short semi-private message"
        (check-not-false
         (got-response?
          sess
          ip
          (format ":a!b@c PRIVMSG #d :~a: " (*my-nick*))
          (pregexp-quote "PRIVMSG #d :Eh?  Speak up."))
         ))

       (test-case
        "backed-up idle events"

        (before
         (begin
           (custodian-shutdown-all (irc-session-custodian sess))
           (set-irc-session-custodian! sess (make-custodian))

           ;; start the news thread
           (*planet-poll-interval* 2)
           (*quote-and-headline-interval* 1/10)
           (respond (parse-irc-message ":x 366 rudybot #emacs :backed-up idle events'") sess)
           )


         ;; ensure there is no news available.
         (set-irc-session-async-for-news! sess (make-cached-channel #f))

         ;; let the channel go idle.
         (vtprintf "Test is sleeping~%")
         (sleep (* 2 (*quote-and-headline-interval*)))

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
          (expect/timeout ip #rx"JAPS" (* 3/4 (*quote-and-headline-interval*))))))
       (test-case
        "keeps saying 'no news'"
        (before
         (begin
           (custodian-shutdown-all (irc-session-custodian sess))
           (set-irc-session-custodian! sess (make-custodian))

           ;; start the news thread
           (*planet-poll-interval* 2)
           (*quote-and-headline-interval* 1/2)
           (respond (parse-irc-message ":x 366 rudybot #emacs :keeps saying 'no news'") sess)
           )

         ;; ensure there is no news available.
         (set-irc-session-async-for-news! sess (make-cached-channel #f))

         ;; wait a smidge for the input-examiner proc to get
         ;; subscribed.
         (sleep 1/2)

         (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 1" (*my-nick*)) #rx"no news"))
         (sleep (*quote-and-headline-interval*))
         (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 2" (*my-nick*)) #rx"no news"))
         (sleep (*quote-and-headline-interval*))
         (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 3" (*my-nick*)) #rx"no news"))
         (sleep (*quote-and-headline-interval*))
         (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 4" (*my-nick*)) #rx"no news"))
         (sleep (*quote-and-headline-interval*))
         (check-not-false (got-response? sess ip (format ":a!b@c PRIVMSG #emacs :~a: news 5" (*my-nick*)) #rx"no news"))

         )
        )
       ))))

(provide (all-defined))
)
