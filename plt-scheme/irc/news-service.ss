#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui news-service-tests 'verbose))"
|#
(module news-service mzscheme
(require (only (lib "19.ss" "srfi")
               current-time)
         (lib "kw.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "cached-channel.ss"
         "channel-events.ss"
         "globals.ss"
         "headline.ss"
         "parse.ss"
         "planet-emacsen.ss"
         "session.ss"
         "test-utils.ss"
         "vprintf.ss"
         )

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



(verbose!)

(define news-service-tests

  (test-suite
   "news-service"
   (test-case
    "yow"
    (let-values (((ip op) (make-pipe)))
      (let* ((feed (make-cached-channel #f))
             (sess (make-irc-session
                    op
                    #:feed feed))
             (service
              (on-demand-news-service
               "#emacs"
               sess
               (lambda (channel text)
                 (fprintf op "PRIVMSG ~a :~a~%"
                          channel text)
                 (newline op)))))

        (define (ask-for-news)
          ((channel-request-event-input-examiner service)
           (parse-irc-message (format ":a!b@x PRIVMSG #emacs :~a: news~%" (*my-nick*)))))

        (check-pred channel-request-event? service)

        (ask-for-news)

        (check-not-false (expect/timeout ip "no news yet" 1/10))

        (cached-channel-put feed (make-entry (current-time)
                                             "JAPS BOMB PERL HARBOR!!"
                                             "http://ruby-lang.org"))

        (sync/timeout 0 feed)
        (ask-for-news)
        (check-not-false (expect/timeout ip "JAPS BOMB PERL HARBOR!!" 1/10)))))))

(provide (all-defined))
)
