#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require bot-tests.ss -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui bot-tests 'verbose))"
|#
(module bot mzscheme
(require (lib "kw.ss")
         (only (lib "1.ss" "srfi")
               any
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
         "alarm-with-snooze.ss"
         "cached-channel.ss"
         "channel-events.ss"
         "del.ss"
         "globals.ss"
         "parse.ss"
         "planet-emacsen.ss"
         "quotes.ss"
         "session.ss"
         "thread.ss"
         "tinyurl.ss"
         "vprintf.ss"
                     )
(register-version-string "$Id$")

(define (respond message s)

  (let* ((threads (filter
                   thread?
                   (custodian-managed-list
                    (irc-session-custodian s)
                    (current-custodian))))
         (corpses (filter thread-dead? threads)))
    (when (not (null? corpses))
      (error 'respond "Gaah! Some threads died: ~s"
             corpses)))

  (vtprintf " <= ~s~%" message)

  ;; so that any threads we make will be easily killable
  (parameterize ((current-custodian (irc-session-custodian s)))

    (define (subscribe-proc-to-server-messages! proc)
      (hash-table-put!
       (irc-session-message-subscriptions s)
       proc
       #t))

    (define (unsubscribe-proc-to-server-messages! proc)
      ;; the hash-table-get has no effect, _except_ that it'll raise
      ;; an exception if PROC isn't already in the table, which is a
      ;; Good Thing to know.  Since that'd be, like, a bug.
      (hash-table-get
       (irc-session-message-subscriptions s)
       proc)

      (let ((before (hash-table-count (irc-session-message-subscriptions s))))
        (hash-table-remove!
         (irc-session-message-subscriptions s)
         proc)
        (let ((after (hash-table-count (irc-session-message-subscriptions s))))
          (when (not (equal? after (sub1 before)))
            (error 'unsubscribe-proc-to-server-messages!
                   "Expected ~a subscriptions, but there are instead ~a"
                   (sub1 before)
                   after)))))

    (define (out . args)
      (apply fprintf (irc-session-op s) args)
      (vtprintf " => ~s~%" (apply format args)))

    (define (pm target msg)
      (out "PRIVMSG ~a :~a~%" target msg))
    (define (notice target msg)
      (out "NOTICE ~a :~a~%" target msg))

    (define/kw (reply response #:key [proc pm] [message message])
      (for-each
       (lambda (r)
         (proc r response))

       (PRIVMSG-receivers message)))

    ;; (trace for-us?)
    ;;     (trace gist-for-us)
    ;;     (trace gist-equal?)
    (define claimed-by-background-task? #f)

    ;; notify each subscriber that we got a message.
    (hash-table-for-each
     (irc-session-message-subscriptions s)
     (lambda (proc ignored)
       (let ((claimed-by-this-proc? (proc message)))
         (when claimed-by-this-proc?
           (vtprintf "Proc ~s has claimed message ~s~%"
                     proc message))
         (set! claimed-by-background-task?
               (or claimed-by-background-task?
                   claimed-by-this-proc?)))))

    ;; note who did what, when, where, how, and wearing what kind of
    ;; skirt; so that later we can respond to "seen Ted?"
    (when (and (PRIVMSG? message)
               (PRIVMSG-is-for-channel? message))
      (let ((who         (PRIVMSG-speaker     message))
            (where       (car (PRIVMSG-receivers message)))
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

    (cond
     ;; if someone other than a bot uttered a long URL, run it through
     ;; tinyurl.com and spew the result.

     ;; might be worth doing this in a separate thread, since it can
     ;; take a while.
     ((and
       (PRIVMSG? message)
       (not (regexp-match #rx"bot$" (PRIVMSG-speaker message)))
       (regexp-match url-regexp (PRIVMSG-text message)))
      =>
      (lambda (match-data)
        (let ((url (car match-data)))
          (when (< (*tinyurl-url-length-threshold*) (string-length url))

            ;; I used to send these out as NOTICEs, since the RFC says
            ;; to do so, but people complained.
            (reply (make-tiny-url url #:user-agent (long-version-string)))))))

     ((RPL_ENDOFNAMES? message)
      (let ((ch (RPL_ENDOFNAMES-channel-name message)))
        ;; periodic jordanb quotes
        (when (member ch '("#emacs" "#bots" "#scheme-bots"))
          (let ((idle-evt
                 (make-channel-idle-event
                  ch
                  (*quote-and-headline-interval*))))

            (subscribe-proc-to-server-messages!
             (channel-idle-event-input-examiner idle-evt))

            (thread-with-id
             (lambda ()
               (let loop ()
                 (let ((q (one-quote)))
                   (sync idle-evt)
                   (pm ch q)
                   (loop)))))))

        ;; news spewage.
        (when (member ch '("#emacs" "#scheme-bots"))

          (vtprintf "Creating the news spewage on-demand service~%")
          (thread-with-id
           (lambda ()
             (let ((cre
                    (make-channel-request-event
                     (lambda (message)
                       (and (PRIVMSG? message)
                            (member ch
                                    (PRIVMSG-receivers message))
                            (gist-equal? "news" message))))))
               (subscribe-proc-to-server-messages!
                (channel-request-event-input-examiner cre))

               (let loop ()
                 (vtprintf "on-demand service waiting for request~%")
                 (let ((req (sync cre)))
                   (vtprintf "on-demand service got request: ~s~%" req)
                   (let ((headline (cached-channel-cache (irc-session-async-for-news s))))
                     (reply (if headline
                                (entry->string headline)
                              "no news yet.")
                            #:message req))
                   (loop))
                 ))))

          ;; periodic news spewage.
          (thread-with-id
           (lambda ()
             (let loop ()
               (vtprintf "Waiting for a headline.~%")
               (let ((headline (sync (irc-session-async-for-news s))))
                 (vtprintf "got ~s~%" headline)
                 (when headline
                   (let ((idle-evt
                          (make-channel-idle-event
                           ch
                           (*quote-and-headline-interval*))))

                     (subscribe-proc-to-server-messages!
                      (channel-idle-event-input-examiner idle-evt))

                     (vtprintf "Waiting for channel ~s to idle.~%"
                               ch)
                     (sync idle-evt)
                     (pm ch
                         (entry->string headline))

                     (unsubscribe-proc-to-server-messages!
                      (channel-idle-event-input-examiner idle-evt))))


                 (loop))))))

        ;; moviestowatchfor
        (when (member ch '("##cinema" "#scheme-bots"))
          (let ((posts #f))
            (thread-with-id
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
            (thread-with-id
             (lambda ()
               (let ((cre (make-channel-request-event
                           (lambda (m)
                             (and (PRIVMSG? m)
                                  (member ch (PRIVMSG-receivers m))
                                  (gist-equal? "movie" m))))))

                 (subscribe-proc-to-server-messages!
                  (channel-request-event-input-examiner cre))

                 (let loop ()
                   (let ((req (sync cre)))
                     (reply
                      (if posts
                          (entry->string
                           (list-ref posts (random (length posts))))
                        "hmm, no movie recommendations yet")
                      #:message req)

                     (loop)))(thread-with-id))))

            (let ((idle (make-channel-idle-event ch (*quote-and-headline-interval*))))

              (subscribe-proc-to-server-messages! (channel-idle-event-input-examiner idle))

              (thread-with-id
               (lambda ()
                 (let loop ()
                   (sync idle)
                   (when posts
                     (notice ch
                             (entry->string
                              (list-ref posts (random (length posts))))))
                   (loop))
                 )))))))

     ((and (ACTION? message)
           (regexp-match #rx"glances around nervously" (PRIVMSG-text message)))
      (reply "\u0001ACTION loosens his collar with his index finger\u0001"))

     ;; "later do foo".  We kludge up a new message that is similar to
     ;; the one that we just received, but with the words "later do"
     ;; hacked out, and then call ourselves recursively with that new
     ;; message.
     ((and (for-us? message)
           (let ((w  (PRIVMSG-text-words message)))
             (and
              (< 2 (length w))
              (string-ci=? "later" (second w))
              (string-ci=? "do"    (third w)))))
      (let* ((parsed-command (cons
                              (format "~a:" (*my-nick*))
                              (cdddr (PRIVMSG-text-words message))))
             (command (string-join parsed-command))
             (params  (append (PRIVMSG-receivers message)
                              (list command))))
        (thread-with-id
         (lambda ()
           (sleep 10)
           (respond
            (make-PRIVMSG
             (message-prefix message)
             (symbol->string (message-command message))
             params
             (PRIVMSG-speaker message)
             (PRIVMSG-receivers message)
             (PRIVMSG-approximate-recipient message)
             command
             parsed-command)
            s)))))

     ((and (gist-equal? "seen" message)
           (< 2 (length (PRIVMSG-text-words message))))
      (let* ((who (regexp-replace #rx"\\?+$" (third (PRIVMSG-text-words message)) ""))
             (sighting (hash-table-get (irc-session-appearances-by-nick s) who #f)))
        (reply (or sighting (format "I haven't seen ~a" who)))))

     ((gist-equal? "quote" message)
      (reply (one-quote)))

     ((or (VERSION? message)
          (gist-equal? "version" message))
      (if (VERSION? message)
          (out "NOTICE ~a :\u0001VERSION ~a\0001~%"
               (PRIVMSG-speaker message)
               (long-version-string))
        (reply (long-version-string))))

     ((or (SOURCE? message)
          (gist-equal? "source" message))
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

     ((and (for-us? message) (not (gist-for-us message)))
      (reply "Eh?  Speak up."))

     ((and
       (for-us? message)
       (not claimed-by-background-task?))

      (reply "\u0001ACTION is at a loss for words, as usual\u0001"))

     (else
      (case (message-command message)
        ((001)
         (for-each (lambda (cn)
                     (vtprintf "Joining ~a~%" cn)
                     (out "JOIN ~a~%" cn))
                   (*initial-channel-names*)))

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
  (with-handlers
      ([exn:fail:network?
        (lambda (e)
          (vtprintf "exception (~s); reconnecting~%"
                    e)

          (sleep 10)
          (start))])

    (let-values (((ip op)
                  (if (*irc-server-name*)
                      (tcp-connect (*irc-server-name*) 6667)
                    (values (current-input-port)
                            (current-output-port)))))

      (define sess
        (make-irc-session
         op
         #:feed
         (queue-of-entries
          #:whence
          (and (*use-real-atom-feed?*)
               (lambda ()
                 (get-pure-port
                  (string->url "http://planet.emacsen.org/atom.xml")
                  (list)))))))

      (when (*log-to-file*)
        (*log-output-port*
         (open-output-file
          ;; BUGBUGs: 1) this isn't portable; 2) we'll croak if this
          ;; directory doesn't exist
          (format "/var/log/irc-bot/~a-~a"
                  (*irc-server-name*)
                  (zdate)))))
      (fprintf (current-error-port)
               "Logging to ~s~%" (object-name (*log-output-port*)))

      ;; so we don't have to call flush-output all the time
      (for-each (lambda (p)
                  (file-stream-buffer-mode p 'line))
                (list op (*log-output-port*)))

      (for-each (lambda (s)
                  (vprintf "~a~%" s))
                (version-strings))
      (vprintf "~a~%" (long-version-string))

      (fprintf op "NICK ~a~%" (*my-nick*))
      (fprintf op "USER ~a unknown-host ~a :~a, ~a~%"
               (or (getenv "USER") "unknown")
               (*irc-server-name*)
               *client-name*
               *svnversion-string*)

      (when (*nickserv-password*)
        (fprintf op "PRIVMSG NickServ :identify ~a~%" (*nickserv-password*)))

      (parameterize-break
       #t
       (with-handlers
           ([exn:break?
             (lambda (x)
               ;; I often see               rudybot [~erich@127.0.0.1] has quit [Client Quit]
               ;; rather than the expected  rudybot [~erich@127.0.0.1] has quit [Ah been shot!]

               ;; http://poe.perl.org/?POE_Cookbook/IRC_Bots suggests
               ;; this may be because the server ignores custom QUIT
               ;; messages from clients that haven't been connected for
               ;; very long.
               (fprintf op "QUIT :Ah been shot!~%")
               (flush-output op)
               (close-output-port op))]
            [exn:fail?
             (lambda (e)
               (let ((whine (format  "Caught an exception: ~s~%" e)))
                 (display whine (*log-output-port*))
                 (display whine (current-error-port)))

               (with-handlers
                   ([exn:fail?
                     (lambda (e)
                       (vtprintf "oh hell, I can't send a quit message~%"))])
                 (fprintf op "QUIT :unexpected failure~%")
                 (flush-output op)
                 (close-output-port op))

               (raise e))])

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
                 (get-one-line))))))))))
(provide
 respond
 start)
)
