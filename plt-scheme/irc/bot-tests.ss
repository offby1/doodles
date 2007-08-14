#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui bot-tests 'verbose))"
|#
(module bot-tests mzscheme
(require (only (lib "pregexp.ss") pregexp-quote)
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "bot.ss"
         "globals.ss"
         (only "parse.ss"
               parse-irc-message)
         "vprintf.ss")

;; returns #f if we didn't find what we're looking for.

(define (expect/timeout ip regex seconds)
  (let* ((ch (make-channel))
         (reader
          (thread
           (lambda ()
             (let loop ()
               (vprintf "expect/timeout about to look for ~s from ~s ...~%"
                        regex
                        (object-name ip))
               (let ((line (read-line ip)))
                 (cond
                  ((eof-object? line)
                   (vprintf "expect/timeout: eof~%")
                   (channel-put ch #f))
                  ((regexp-match regex line)
                   (vprintf "expect/timeout: Got match!~%")
                   (channel-put ch #t))
                  (else
                   (vprintf "expect/timeout: nope; retrying~%")
                   (loop)))

                 ))))))
    (and (sync/timeout seconds ch)
         ch)))

;; TODO -- write proper "check-response", so that it gives meaningful
;; spew when it fails
(define (got-response? sess ip input regexp)
  (respond (parse-irc-message input) sess)
  (expect/timeout ip regexp 1))

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

        ;; ensure there is no news available.

        ;; let the channel go idle.

        ;; have someone say something on the channel.

        ;; now QUICKLY provide some news.

        ;; we should not see the bot spew the news, because the channel is
        ;; no longer idle.

        (before
         (begin
           (printf "Doin' some prep work.~%")
           (custodian-shutdown-all (irc-session-custodian sess))
           (set-irc-session-custodian! sess (make-custodian)))
         (fail
          "I really need to write this test."))
        )
       ))))

(provide (all-defined))
)
