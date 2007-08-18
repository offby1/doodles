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
         "cached-channel.ss"
         "channel-events.ss"
         "parse.ss"
         "planet-emacsen.ss"
         "session.ss"
         "vprintf.ss")

(define (news-service
         channel-name
         subscriber
         unsubscriber
         gist-equal?
         irc-session
         pm)

  (lambda ()
    ;; men seldom make passes at girls who wear
    ;; glasses
    (let loop ((passes 0))

      (vtprintf "news service: top of loop, pass ~a~%" passes)
      (let* ((input-examiner
              (lambda (message)
                (define (p . args)
                  (let ((format-string (car args))
                        (args (cdr args)))

                    (apply
                     vtprintf
                     (string-append
                      "news service input-examiner for 'news' #~a: "
                      format-string)
                     (cons passes args))))
                (p "running!~%")
                (let ((chan? (PRIVMSG-is-for-channel? message)))
                  (p "message ~a for a channel~%"
                     (if chan? "is" "is not"))
                  (and chan?
                       (let ((tc? (equal? (PRIVMSG-destination message) channel-name)))
                         (p "message ~a for _this_ channel~%"
                            (if tc? "is" "is not"))
                         (let ((ge? (gist-equal?  "news" message)))
                           (p "gist ~a 'news'~%"
                              (if ge? "is" "is not"))))))))
             (news-request-event
              (make-channel-request-event input-examiner)))

        (vtprintf "news service: subscribing proc ~s~%" input-examiner)
        (subscriber
         (channel-request-event-input-examiner news-request-event))

        (let ((why (sync news-request-event))
              (headline (cached-channel-cache (irc-session-async-for-news irc-session))))
          (vtprintf "news service: about to pm~%")
          (pm channel-name
              (if headline
                  (format "~a, news: ~a"
                          (PRIVMSG-speaker why)
                          (entry->string headline))
                (format
                 "~a: Sorry, no news yet."
                 (PRIVMSG-speaker why))))

          ;; once we loop, our news-request-event will
          ;; go out of scope, and nobody will ever
          ;; sync on it again.  If we don't yank its
          ;; input-examiner from the subscription
          ;; list, it will continue to tell the main
          ;; loop that its corresponding thread will
          ;; handle the message, and thus that message
          ;; will never actually get processed.
          (unsubscriber
           (channel-request-event-input-examiner news-request-event))
          (vtprintf "news service: unsubscribed; about to loop~%")
          (loop (add1 passes))))

      )))


(define news-service-tests

  (test-suite
   "news-service"
   (test-case
    "yow"
    (check-not-false (news-service
                      "#emacs"
                      values
                      values
                      values
                      (make-irc-session (open-output-string))
                      values)))))

(provide (all-defined))
)
