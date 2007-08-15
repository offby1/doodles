#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require bot-tests.ss -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui bot-tests 'verbose))"
|#
(module bot mzscheme
(require (lib "async-channel.ss")
         (lib "kw.ss")
         (only (lib "1.ss" "srfi")
               first second third
               filter)
         (only (lib "13.ss" "srfi")
               string-join)
         (only (lib "19.ss" "srfi")
               current-date)
         (only (lib "url.ss" "net")
               get-pure-port
               string->url)
         (lib "trace.ss")
         (only (planet "port.ss" ("schematics" "port.plt" 1 0)) port->string)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "zdate.ss" ("offby1" "offby1.plt")) zdate)
         (only (planet "assert.ss" ("offby1" "offby1.plt")) check-type)
         "channel-idle-event.ss"
         "del.ss"
         "globals.ss"
         "parse.ss"
         "planet-emacsen.ss"
         "quotes.ss"
         "alarm-with-snooze.ss"
         "thread.ss"
         "tinyurl.ss"
         "vprintf.ss")
(register-version-string "$Id$")

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

   ;; the IRC server is at the other end of this port.
   op

   ;; this is just for testing, so that we can easily ensure none of
   ;; the background threads are running.
   custodian
   ) #f)

(define/kw (public-make-irc-session op #:key [feed #f] )
  (when feed
    (check-type 'make-irc-session cached-channel? feed))
  (make-irc-session
    (make-hash-table 'equal)
    (make-hash-table 'equal 'weak)
    feed
    op
    (make-custodian)
    ))

(define (public-set-irc-session-async-for-news! sess thing)
  (when thing
    (check-type 'set-irc-session-async-for-news! cached-channel? thing))
  (set-irc-session-async-for-news! sess thing))

(define (respond message s)

  ;; so that any threads we make will be easily killable
  (parameterize ((current-custodian (irc-session-custodian s)))

    (define (subscribe-proc-to-server-messages! proc)
      (hash-table-put!
       (irc-session-message-subscriptions s)
       proc
       #t))

    (define (out . args)
      (apply fprintf (irc-session-op s) args)
      (vtprintf " => ~s~%" (apply format args)))

    (define (pm target msg)
      (out "PRIVMSG ~a :~a~%" target msg))
    (define (notice target msg)
      (out "NOTICE ~a :~a~%" target msg))

    (define/kw (reply response #:key [proc pm])
      (proc ((if (PRIVMSG-is-for-channel? message) PRIVMSG-destination PRIVMSG-speaker)
             message)
            response))

    (define for-us?
      (and
       (PRIVMSG? message)
       (equal? (*my-nick*)
               ((if (PRIVMSG-is-for-channel? message)
                    PRIVMSG-approximate-recipient
                  PRIVMSG-destination) message))))

    (define gist-for-us
      (and for-us?
           (cond
            ((and (PRIVMSG-is-for-channel? message)
                  (< 1 (length (PRIVMSG-text-words message))))
             (second (PRIVMSG-text-words message)))
            ((and (not (PRIVMSG-is-for-channel? message))
                  (< 0 (length (PRIVMSG-text-words message))))
             (first (PRIVMSG-text-words message)))
            (else
             #f))
           ))

    (define (gist-equal? str)
      (equal? str gist-for-us))

    ;; trim trailing punctuation
    (set! gist-for-us
          (and gist-for-us
               (regexp-replace (pregexp "[^[:alpha:]]+$") gist-for-us "")))

    (vtprintf " <= ~s~%" message)

    ;; notify each subscriber that we got a message.
    (hash-table-for-each
     (irc-session-message-subscriptions s)
     (lambda (proc ignored)
       (proc message)))

    ;; note who did what, when, where, how, and wearing what kind of
    ;; skirt; so that later we can respond to "seen Ted?"
    (when (and (PRIVMSG? message)
               (PRIVMSG-is-for-channel? message))
      (let ((who         (PRIVMSG-speaker     message))
            (where       (PRIVMSG-destination message))
            (what        (PRIVMSG-text        message))

            ;; don't name this variable "when"; that would shadow some
            ;; rather useful syntax :-)
            (time        (current-seconds))

            (was-action? (ACTION?             message)))

        (hash-table-put!
         (irc-session-appearances-by-nick s)
         who
         (format "~a~a in ~a~a ~a~a"
                 who
                 (if was-action? "'s last action" " last spoke")
                 where
                 (if was-action? " was at"        ""           )
                 (zdate (seconds->date time))
                 (if was-action?
                     (format ": ~a ~a" who what)
                   (format ", saying \"~a\"" what))))))

    ;; if someone other than a bot uttered a long URL, run it through
    ;; tinyurl.com and spew the result.

    ;; might be worth doing this in a separate thread, since it can
    ;; take a while.
    (cond
     ((and
       (PRIVMSG? message)
       (not (regexp-match #rx"bot$" (PRIVMSG-speaker message)))
       (regexp-match url-regexp (PRIVMSG-text message)))
      =>
      (lambda (match-data)
        (let ((url (car match-data)))
          (when (< (*tinyurl-url-length-threshold*) (string-length url))
            (reply (make-tiny-url url #:user-agent (long-version-string))
                   #:proc notice))))))

    (cond
     ((and (ACTION? message)
           (regexp-match #rx"glances around nervously" (PRIVMSG-text message)))
      (reply "\u0001ACTION loosens his collar with his index finger\u0001"))

     ;; "later do foo".  We kludge up a new message that is similar to
     ;; the one that we just received, but with the words "later do"
     ;; hacked out, and then call ourselves recursively with that new
     ;; message.
     ((and for-us?
           (let ((w  (PRIVMSG-text-words message)))
             (and
              (< 2 (length w))
              (string-ci=? "later" (second w))
              (string-ci=? "do"    (third w)))))
      (let* ((parsed-command (cons
                              (format "~a:" (*my-nick*))
                              (cdddr (PRIVMSG-text-words message))))
             (command (string-join parsed-command))
             (params  (list (PRIVMSG-destination message)
                            command)))
        (my-thread
         (lambda ()
           (sleep 10)
           (respond
            (make-PRIVMSG
             (message-prefix message)
             (symbol->string (message-command message))
             params
             (PRIVMSG-speaker message)
             (PRIVMSG-destination message)
             (PRIVMSG-approximate-recipient message)
             command
             parsed-command)
            s)))))

     ((and (gist-equal? "seen")
           (< 2 (length (PRIVMSG-text-words message))))
      (let* ((who (regexp-replace #rx"\\?+$" (third (PRIVMSG-text-words message)) ""))
             (sighting (hash-table-get (irc-session-appearances-by-nick s) who #f)))
        (reply (or sighting (format "I haven't seen ~a" who)))))

     ((gist-equal? "quote")
      (reply (one-quote)))

     ((and (gist-equal? "news"))
      (reply (or (let ((entry (cached-channel-cache
                               (irc-session-async-for-news s))))
                   (and entry (entry->string entry)))
                 "Sorry, no news yet.")))

     ((or (VERSION? message)
          (gist-equal? "version"))
      (if (VERSION? message)
          (out "NOTICE ~a :\u0001VERSION ~a\0001~%"
               (PRIVMSG-speaker message)
               (long-version-string))
        (reply (long-version-string))))

     ((or (SOURCE? message)
          (gist-equal? "source"))
      (let ((source-host "offby1.ath.cx")
            (source-directory "/~erich/bot/")
            (source-file-names "rudybot.tar.gz"))
        (if (SOURCE? message)
            (out "NOTICE ~a :\u0001SOURCE ~a:~a:~a\0001~%"
                 (PRIVMSG-speaker message)
                 source-host
                 source-directory
                 source-file-names)
          (reply (format "http://~a~a~a" source-host source-directory source-file-names)))))

     ((and for-us? (not gist-for-us))
      (reply "Eh?  Speak up."))

     (for-us?
      (reply "\u0001ACTION is at a loss for words, as usual\u0001"))

     (else
      (case (message-command message)
        ((001)
         (for-each (lambda (cn)
                     (vtprintf "Joining ~a~%" cn)
                     (out "JOIN ~a~%" cn))
                   (*initial-channel-names*)))

        ((366)
         (when (< 1 (length  (message-params message)))
           (let ((this-channel (second (message-params message))))

             (when (member this-channel '("#emacs" "#bots" ))
               (let ((idle-evt
                      (make-channel-idle-event
                       this-channel
                       (*quote-and-headline-interval*))))

                 (subscribe-proc-to-server-messages!
                  (channel-idle-event-input-examiner idle-evt))

                 (my-thread
                  (lambda ()
                    (let loop ()
                      (let ((q (one-quote)))
                        (sync idle-evt)
                        (pm this-channel q)
                        (loop)))))))

             (when (equal? this-channel "#emacs")
               (my-thread
                (lambda ()
                  (let loop ()
                    (vtprintf "Waiting for a headline.~%")
                    (let ((headline (cached-channel-apply
                                     (irc-session-async-for-news s)
                                     sync/timeout (*planet-poll-interval*)
                                     )))
                      (vtprintf "got ~s~%" headline)
                      (when headline
                        (let ((idle-evt
                               (make-channel-idle-event
                                this-channel
                                (*quote-and-headline-interval*))))
                          (subscribe-proc-to-server-messages!
                           (channel-idle-event-input-examiner idle-evt))
                          (vtprintf "Waiting for the channel to idle.~%")
                          (sync idle-evt)
                          (pm this-channel
                              (entry->string headline))))
                      (loop))))))

             (when (equal? this-channel "##cinema")
               (let ((posts #f))
                 (my-thread
                  (lambda ()
                    (let loop ()
                      (with-handlers
                          ([exn:delicious:auth?
                            (lambda (e)
                              (vtprintf
                               "wrong delicious password; won't snarf moviestowatchfor posts~%"))])
                        (set! posts (snarf-some-recent-posts))
                        (sleep  (* 7 24 3600))
                        (loop)))))

                 (let ((idle (make-channel-idle-event this-channel (* 3600 4))))

                   (subscribe-proc-to-server-messages! (channel-idle-event-input-examiner idle))

                   (my-thread
                    (lambda ()
                      (let loop ()
                        (sync idle)
                        (when posts
                          (notice this-channel
                                  (entry->string
                                   (list-ref posts (random (length posts))))))
                        (loop))
                      ))))))))

        ((433)
         (error 'respond "Nick already in use!"))

        ((NOTICE)
         #t ;; if it's a whine about identd, warn that it's gonna be slow.
         )

        ((PING)
         #t
         (out "PONG :~a~%" (car (message-params message))))
        )))))

;(trace respond)

(define (start)
  (let-values (((ip op)
                (if (*irc-server-name*)
                    (tcp-connect (*irc-server-name*) 6667)
                  (values (current-input-port)
                          (current-output-port)))))

    (define sess
      (public-make-irc-session
       op
       #:feed
       (queue-of-entries
        #:whence
        (and (*use-real-atom-feed?*)
             (lambda ()
               (get-pure-port
                (string->url "http://planet.emacsen.org/atom.xml")
                (list)))))))

    (when (*irc-server-name*)
      (*log-output-port*
       (open-output-file
        (format "~a-~a"
                (*irc-server-name*)
                (zdate)))))

    ;; so we don't have to call flush-output all the time
    (for-each (lambda (p)
                (file-stream-buffer-mode p 'line))
              (list op (*log-output-port*)))

    (for-each (lambda (s)
                (vprintf "~a~%" s))
              (version-strings))

    (fprintf op "NICK ~a~%" (*my-nick*))
    (fprintf op "USER ~a unknown-host ~a :~a, ~a~%"
             (or (getenv "USER") "unknown")
             (*irc-server-name*)
             *client-name*
             (*client-version*))

    (when (*nickserv-password*)
      (fprintf op "PRIVMSG NickServ :identify ~a~%" (*nickserv-password*)))

    (parameterize-break
     #t
     (with-handlers
         ([exn:break?
           (lambda (x)
             (fprintf op "QUIT :Ah been shot!~%")
             (flush-output op)
             (close-output-port op))]
          [exn:fail?
           (lambda (e)
             (fprintf op "QUIT :unexpected failure~%")
             (close-output-port op)
             (let ((whine (format  "Caught an exception: ~s~%" e)))
               (vtprintf whine)
               (fprintf (current-error-port)
                         whine)))])

       (let get-one-line ()
         (let ((line (read-line ip 'return-linefeed)))
           (if (eof-object? line)
               (begin
                 (vtprintf "eof on server; reconnecting~%")
                 (sleep 10)
                 (start))
             (begin
               (with-handlers
                   ([exn:fail:irc-parse?
                     (lambda (e)
                       (vtprintf "couldn't parse line from server: ~s~%" e))])
                 (respond (parse-irc-message line) sess))
               (get-one-line)))))))))

(provide (all-defined-except make-irc-session set-irc-session-async-for-news!)
         (rename public-make-irc-session make-irc-session)
         (rename public-set-irc-session-async-for-news! set-irc-session-async-for-news!))
)
