#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui session-tests 'verbose))"
|#
(module session mzscheme
(require (lib "kw.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "assert.ss" ("offby1" "offby1.plt")) check-type)
         "cached-channel.ss"
         "vprintf.ss")
(define-struct irc-session
  (
   appearances-by-nick

   ;; Procedures who want to be called whenever a new message arrives.
   ;; They're likely channel-idle-events.  Conceptually it's a list,
   ;; but actually it's a weak hash table whose keys are the
   ;; procedures, and whose values are ignored.  This way, in theory,
   ;; if we drop references to the procedures, they'll get
   ;; garbage-collected.  Otherwise they'd accumulate here.  (As it
   ;; happens the current code -doesn't- drop references to those
   ;; procedures, but I might later make it do so.)
   message-subscriptions

   ;; where we get news headlines from.  #f means we get 'em from a
   ;; little stub, for testing.
   async-for-news

   ;; where we get "movies to watch for" headlines from.
   movies-queue

   ;; the IRC server is at the other end of this port.
   op

   ;; this is just for testing, so that we can easily ensure none of
   ;; the background threads are running.
   custodian

   ) #f)

(define/kw (public-make-irc-session
            op
            #:key
            [newsfeed #f])
  (when newsfeed
    (check-type 'make-irc-session cached-channel? newsfeed))
  (letrec ((sess
            (make-irc-session

             ;; find some PLT equivalent of Perl's tied hashes, so that this
             ;; table will persist to disk.  Name the disk file after the IRC
             ;; server.  Put it in /var/something on *nix, and %APPDATA%\rudybot
             ;; on Winders.
             (make-hash-table 'equal)

             (make-hash-table 'equal)
             newsfeed
             (make-cached-channel)
             op
             (make-custodian)
             )))
    sess))

(define (public-set-irc-session-async-for-news! sess thing)
  (when thing
    (check-type 'set-irc-session-async-for-news! cached-channel? thing))
  (set-irc-session-async-for-news! sess thing))


(define session-tests

  (test-suite
   "session"
   (test-case
    "yow"
    (check-regexp-match
     #rx"bar"
     "foo"))))

(provide (all-defined-except public-make-irc-session make-irc-session)
         (rename public-make-irc-session make-irc-session))
)
