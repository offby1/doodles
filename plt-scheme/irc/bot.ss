#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require bot-tests.ss -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui bot-tests 'verbose))"
|#
(module bot mzscheme
(require (lib "kw.ss")
         (only (lib "etc.ss") this-expression-source-directory)
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
         (planet "assert.ss" ("offby1" "offby1.plt"))
         "alarm-with-snooze.ss"
         "cached-channel.ss"
         "channel-events.ss"
         "del.ss"
         "globals.ss"
         "headline.ss"
         "parse.ss"
         "planet-emacsen.ss"
         "quotes.ss"
         "session.ss"
         "shuffle.ss"
         "thread.ss"
         "tinyurl.ss"
         "vprintf.ss"
         )
(register-version-string "$Id$")

(define (on-channel? c m)
  (and (PRIVMSG? m)
       (member c (PRIVMSG-receivers m))))

(define out
  (lambda (s . args)
    (apply fprintf (irc-session-op s) args)
    (vtprintf " => ~s~%" (apply format args))))

(define pm
  (lambda (s target msg)
    (check-type 'pm string? msg)
    (check-type 'pm string? target)
    (out  s "PRIVMSG ~a :~a~%" target msg)))

(define  notice
  (lambda (s target msg)
    (check-type 'pm string? msg)
    (check-type 'pm string? target)
    (out  s "NOTICE ~a :~a~%" target msg)))

(define reply
  (lambda/kw (s message response #:key [proc pm] )
    (check-type 'reply irc-session? s)
    (check-type 'reply message? message)
    (check-type 'reply string? response)
    (check-type 'reply procedure? proc)
    (for-each
     (lambda (r)
       (proc s r response))

     (if (PRIVMSG-is-for-channel? message)
         (PRIVMSG-receivers message)
       (list (PRIVMSG-speaker message))))))

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

  ;; notify each subscriber that we got a message.
  (let ((handled? #f))
    (hash-table-for-each
     (irc-session-message-subscriptions s)
     (lambda (proc ignored)
       (when (proc message)
         (vtprintf "Proc ~s has claimed message ~s~%"
                   proc message)
         (set! handled? #t))))

    (when (and
           (for-us? message)
           (not handled?))

      (reply s message
             "\u0001ACTION is at a loss for words, as usual\u0001"))))


;;(trace respond)

(define *sess* #f)

(define (subscribe-proc-to-server-messages! proc s)
  (check-type 'subscribe-proc-to-server-messages! procedure? proc)
  (hash-table-put!
   (irc-session-message-subscriptions s)
   proc
   #t))

(define (unsubscribe-proc-to-server-messages! proc s)
  (when *sess*
    (hash-table-remove!
     (irc-session-message-subscriptions *sess*)
     proc)))

;(trace unsubscribe-proc-to-server-messages!)

(define (register-usual-services! session)

  ;; so that these threads will be easily killable
  (parameterize ((current-custodian (irc-session-custodian session)))

    (define/kw (add!
                discriminator
                action
                #:key
                [responds? #f]
                [timeout #f]
                [descr "unknown"])
      (subscribe-proc-to-server-messages!
       (make-channel-action
        discriminator
        action
        #:responds? responds?
        #:timeout timeout
        #:descr descr)
       session))

    (add!
     RPL_ENDOFNAMES?
     (lambda (366-message)
       (vtprintf "Got 366~%")
       (let ((ch (RPL_ENDOFNAMES-channel-name 366-message)))
         (define (chatter? m) (on-channel? ch m))
         (define (command=? str m) (and (chatter? m) (gist-equal? str m)))

         ;; (trace chatter?)
         ;; (trace command=?)

         (define (exponentially-backing-off-spewer proc descr)
           (thread-with-id
            (let* ((delay (*minimum-headline-delay*)))
              (lambda ()

                (add!
                 chatter?
                 (lambda (m)
                   (set! delay (*minimum-headline-delay*)))
                 #:responds? #f
                 #:descr descr)

                (let loop ()
                  (proc delay)
                  (set! delay (* 2 delay))
                  (loop))
                ))
            #:descr (format
                     "exponentially-backing-off-spewer for ~a for channel ~a"
                     descr ch)))

         (define/kw (consume-and-spew
                     news-source
                     headline-filter
                     headline-proc
                     #:key

                     ;; this is basically a chance to wrap both
                     ;; headline-filter and headline-proc in a
                     ;; call-with-semaphore, since in at least one
                     ;; case, those procedures need to read and write
                     ;; the PLT preferences file.
                     [wrapper (lambda ( t) ( t))]

                     [descr "unknown, damn it"])
           (exponentially-backing-off-spewer
            (lambda (delay)
              ;; wait for something to say.
              (let ((headline (sync news-source)))
                (wrapper
                 (lambda ()
                   (when (headline-filter headline)
                     ;; wait for a chance to say it.
                     (let ((cme (make-channel-message-event
                                 chatter?
                                 #:periodic? #f
                                 #:timeout delay)))
                       (subscribe-proc-to-server-messages!
                        (channel-idle-event-input-examiner cme)
                        session)

                       (sync cme)

                       (headline-proc headline)

                       (unsubscribe-proc-to-server-messages! cme session)))))))

            (format "delay resetter for ~a" descr)))

         ;; jordanb quotes

         ;; on-demand ...
         (add!
          (lambda (m) (command=? "quote" m))
          (lambda (m)
            (for-each
             (lambda (r)
               (notice session r (one-quote)))
             (PRIVMSG-receivers m)))
          #:responds? #t)

         (when (member ch '("#emacs" "#bots" "#scheme-bots"))
           (let ((quote-channel (make-channel)))
             ;; producer of quotes
             (thread-with-id
              (lambda ()
                (let loop ()
                  (channel-put quote-channel (one-quote))
                  (loop)))
              #:descr "quote producer")

             (consume-and-spew
              quote-channel
              (lambda (quote) #t)
              (lambda (quote)
                (pm session ch quote))
              #:descr "periodic funny quotes")))

         ;; on-demand news spewage.
         (add!
          (lambda (m) (command=? "news" m))
          (lambda (m)
            (let ((headline (cached-channel-cache (irc-session-async-for-news session))))
              (reply session m
                     (if headline
                         (entry->string headline)
                       "no news yet."))))
          #:responds? #t
          #:descr "on-demand news")

         (when (member ch '("#emacs" "#scheme-bots"))

           ;; periodic news spewage.
           (when (irc-session-async-for-news session)
             (consume-and-spew
              (irc-session-async-for-news session)
              (lambda (headline)
                 (assert (entry? headline))
                 (not (already-spewed? headline)))
              (lambda (headline)
                (assert (entry? headline))
                (pm session ch
                    (entry->string headline))
                (note-spewed! headline))
              #:wrapper (lambda (thunk)
                          (call-with-semaphore
                           *prefs-file-semaphore*
                           thunk))
              #:descr "periodic news spewage")))

         ;; moviestowatchfor
         (let ((posts-channel (make-cached-channel)))

           ;; producer thread -- updates posts-channel
           (thread-with-id
            (lambda ()
              (with-handlers
                  ([exn:delicious:auth?
                    (lambda (e)
                      (vtprintf
                       "wrong delicious password; won't snarf moviestowatchfor posts~%"))]
                   [exn:fail:network?
                    (lambda (e)
                      (vtprintf
                       "Can't seem to contact del.icio.us~%"))])

                (let loop ()
                  (for-each
                   (lambda (post)
                     (cached-channel-put
                      posts-channel
                      (maybe-make-URL-tiny post)))
                   (shuffle-list (snarf-some-recent-posts)))
                  (sleep (* 7 24 3600))
                  (loop))))

            #:descr "moviestowatchfor")

           (add!
            (lambda (m) (command=? "movie" m))
            (lambda (m)
              (reply session
                     m
                     (let ((post (cached-channel-cache posts-channel)))
                       (if post
                           (entry->string post)
                         "hmm, no movie recommendations yet"))))
            #:responds? #t)

         (when (member ch '("##cinema" "#scheme-bots"))

           (consume-and-spew
            posts-channel
            (lambda (post) #t)
            (lambda (post)
              (pm session
                  ch
                  (entry->string post)))
            #:descr "periodic moviestowatchfor"))))))

    (add!
     (lambda (m)
       (and (PRIVMSG? m)
            (PRIVMSG-is-for-channel? m)))
     (lambda (m)

       ;; note who did what, when, where, how, and wearing what kind
       ;; of skirt; so that later we can respond to "seen Ted?"
       (let ((who         (PRIVMSG-speaker     m))
             (where       (car (PRIVMSG-receivers m)))
             (what        (PRIVMSG-text        m))

             ;; don't name this variable "when"; that would shadow some
             ;; rather useful syntax :-)
             (time        (current-seconds))

             (was-action? (ACTION?             m)))

         (hash-table-put!
          (irc-session-appearances-by-nick session)
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
     #:descr "fingerprint file")

    (add!
     (lambda (m)
       (and
        (PRIVMSG? m)
        (not (regexp-match #rx"bot$" (PRIVMSG-speaker m)))
        (regexp-match url-regexp (PRIVMSG-text m))))
     (lambda (m)
       ;; if someone other than a bot uttered a long URL, run it
       ;; through tinyurl.com and spew the result.

       ;; might be worth doing this in a separate thread, since it
       ;; can take a while.
       (let ((url (car (regexp-match url-regexp (PRIVMSG-text m)))))
         (when (< (*tinyurl-url-length-threshold*) (string-length url))

           ;; I used to send these out as NOTICEs, since the RFC says
           ;; to do so, but people complained.
           (reply session
                  m
                  (make-tiny-url url #:user-agent (long-version-string))))))
     #:descr "tinyurl")

    (add!
     (lambda (m)
       (and
        (ACTION? m)
        (regexp-match #rx"glances around nervously" (PRIVMSG-text m))))
     (lambda (m)
       (reply session
              m
              "\u0001ACTION loosens his collar with his index finger\u0001"))
     #:descr "loosens collar")

    (add!
     (lambda (m)
       (and (gist-equal? "seen" m)
            (< 2 (length (PRIVMSG-text-words m)))))
     (lambda (m)
       (let* ((who (regexp-replace #rx"\\?+$" (third (PRIVMSG-text-words m)) ""))
              (sighting (hash-table-get (irc-session-appearances-by-nick session) who #f)))
         (reply session
                m
                (or sighting (format "I haven't seen ~a" who)))))
     #:responds? #t
     #:descr "'seen' command")

    (add!
     (lambda (m) (or (VERSION? m)
                     (gist-equal? "version" m)))
     (lambda (m)
       (if (VERSION? m)
           (out session "NOTICE ~a :\u0001VERSION ~a\0001~%"
                (PRIVMSG-speaker m)
                (long-version-string))
         (reply session m (long-version-string))))
     #:responds? #t)

    (add!
     (lambda (m)
       (or (SOURCE? m)
           (gist-equal? "source" m)))
     (lambda (m)
       (let ((source-host "offby1.ath.cx")
             (source-directory "/~erich/bot/")
             (source-file-names "rudybot.tar.gz"))
         (if (SOURCE? m)
             (out session "NOTICE ~a :\u0001SOURCE ~a:~a:~a\0001~%"
                  (PRIVMSG-speaker m)
                  source-host
                  source-directory
                  source-file-names)
           (reply session  m
                  (format "http://~a~a~a" source-host source-directory source-file-names)))))
     #:responds? #t)

    (add!
     (lambda (m)
       (equal? 001 (message-command m)))
     (lambda (m)
       (for-each (lambda (cn)
                   (vtprintf "Joining ~a~%" cn)
                   (out session "JOIN ~a~%" cn))
                 (*initial-channel-names*))))


    (add!
     (lambda (m)
       (equal? 'PING (message-command m)))
     (lambda (m)
       (out session "PONG :~a~%" (car (message-params m)))))

    (add!
     (lambda (m)
       (and (PRIVMSG? m)
            (for-us? m)
            (not (gist-for-us m))))
     (lambda (m)
       (reply session  m "Eh? Speak up, sonny.")))))

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

      (letrec ((s (make-irc-session
                   op
                   #:feed
                   (queue-of-entries
                    #:whence
                    (if (*use-real-atom-feed?*)
                        (lambda ()
                          (get-pure-port
                           (string->url "http://planet.emacsen.org/atom.xml")
                           (list)))
                      fake-atom-feed)
                    #:filter (lambda (e)
                               (not (already-spewed? e)))))))
        (set! *sess* s))

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

      (register-usual-services! *sess*)

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
                 (if #t
                     (vtprintf "eof on server; not reconnecting~%")
                   (begin
                     (vtprintf "eof on server; reconnecting~%")
                     (custodian-shutdown-all (irc-session-custodian *sess*))
                     (sleep 10)
                     (start)))
               (begin
                 (with-handlers
                     ([exn:fail:irc-parse?
                       (lambda (e)
                         (vtprintf
                          "~a: ~s~%"
                          (exn-message e)
                          (exn:fail:irc-parse-string e)))]
                      )
                   (respond (parse-irc-message line) *sess*))
                 (get-one-line))))))))))
(provide
 register-usual-services!
 respond
 start)
)
