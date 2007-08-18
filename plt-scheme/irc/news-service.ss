#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui news-service-tests 'verbose))"
|#
(module news-service mzscheme
(require (lib "kw.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "test-utils.ss"
         "cached-channel.ss"
         "channel-events.ss"
         (only "globals.ss" *my-nick*)
         "parse.ss"
         "planet-emacsen.ss"
         "session.ss"
         "vprintf.ss")

(define (on-demand-news-service
         channel-name
         irc-session
         pm)

  (let ((news-request-event
         (make-channel-request-event
          (lambda (message)
            (and (PRIVMSG-is-for-channel? message)
                 (equal? (PRIVMSG-destination message) channel-name)
                 (gist-equal?  "news" message))))))

    (thread
     (lambda ()
       (let loop ()
         (let ((why (sync news-request-event))
               (headline (cached-channel-cache (irc-session-async-for-news irc-session))))

           (pm channel-name
               (if headline
                   (format "~a, news: ~a"
                           (PRIVMSG-speaker why)
                           (entry->string headline))
                 (format
                  "~a: Sorry, no news yet."
                  (PRIVMSG-speaker why))))
           (loop)))))
    news-request-event))



(define news-service-tests

  (test-suite
   "news-service"
   (test-case
    "yow"
    (let-values (((ip op) (make-pipe)))
      (let ((sess (make-irc-session op)))
        (let ((service
               (on-demand-news-service
                "#emacs"
                sess
                (lambda (str)
                  (display str op)
                  (newline op)))))


          (check-pred channel-request-event? service)

          (fprintf op ":a@b!x PRIVMSG #emacs ~a :news" (*my-nick*))

          (check-not-false (expect/timeout ip "no news yet" 1/10)))  )))))

(provide (all-defined))
)

