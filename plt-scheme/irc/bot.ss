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
         "tinyurl.ss"
         "vprintf.ss")

(define-struct irc-session
  (
   appearances-by-nick
   message-subscriptions
   async-for-news
   op
   custodian
   ) #f)

(define/kw (public-make-irc-session op #:key [feed #f] )
  (make-irc-session
    (make-hash-table 'equal)
    '()
    feed
    op
    (make-custodian)))

(define (respond message s)
  (parameterize ((current-custodian (irc-session-custodian s)))

    (define (subscribe-proc-to-server-messages! proc)
      (set-irc-session-message-subscriptions!
       s
       (cons proc (irc-session-message-subscriptions s))))

    (define (out . args)
      (apply fprintf (irc-session-op s) args)
      (vtprintf " => ~s~%" (apply format args)))

    (define (pm target msg)
      (out "PRIVMSG ~a :~a~%" target msg))
    (define (notice target msg)
      (out "NOTICE ~a :~a~%" target msg))

    (define/kw (reply response #:key [proc pm])
      (proc (if (PRIVMSG-is-for-channel? message)
                (PRIVMSG-destination message)
              (PRIVMSG-speaker message))
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

    (check-type 'respond irc-session? s)
    (check-type 'respond message? message)

    (set! gist-for-us
          (and gist-for-us
               (regexp-replace (pregexp "[^[:alpha:]]+$") gist-for-us "")))

    (vtprintf " <= ~s~%" message)

    (for-each (lambda (proc)
                (proc message))
              (irc-session-message-subscriptions s))

    (when (and (PRIVMSG? message)
               (PRIVMSG-is-for-channel? message))
      (let ((who         (PRIVMSG-speaker     message))
            (where       (PRIVMSG-destination message))
            (what        (PRIVMSG-text        message))
            (time        (current-seconds))
            (was-action? (ACTION?             message)))

        ;; note who did what, when, where, how, and wearing what kind
        ;; of skirt; so that later we can respond to "seen Ted?"
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
          ;; tiny URLs are about 25 characters, so it seems reasonable
          ;; to ignore URLs that are shorter than twice that.
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
        (thread
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


            ;; *groan* how un-Lispy that I have to spell out all
            ;; these identifiers!
            ;; http://www.paulgraham.com/popular.html
            (irc-session-op s))))))

     ((equal? "seen" gist-for-us)
      (let* ((who (regexp-replace #rx"\\?+$" (third (PRIVMSG-text-words message)) ""))
             (poop (hash-table-get (irc-session-appearances-by-nick s) who #f)))
        (reply (or poop (format "I haven't seen ~a" who)))))

     ((and (equal? "quote" gist-for-us)
           (reply (one-quote))))

     ((and (equal? "news" gist-for-us))
      (reply (or (let ((entry (async-channel-try-get (irc-session-async-for-news s))))
                   (and entry (entry->string entry)))
                 "Sorry, no news yet.")))

     ((or (VERSION? message)
          (equal? "version" gist-for-us))
      (if (VERSION? message)
          (out "NOTICE ~a :\u0001VERSION ~a\0001~%"
               (PRIVMSG-speaker message)
               (long-version-string))
        (reply (long-version-string))))
     ((or (SOURCE? message)
          (equal? "source" gist-for-us))
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
         (let ((this-channel (second (message-params message))))
           (when (member this-channel '("#emacs" "#bots" ))
             (let ((idle-evt (make-channel-idle-event this-channel (*quote-and-headline-interval*))))

               (for-each
                subscribe-proc-to-server-messages!
                (list (channel-idle-event-input-examiner idle-evt)))

               (thread
                (lambda ()
                  (let loop ()
                    (let ((q (one-quote)))
                      (sync idle-evt)
                      (pm this-channel q)
                      (loop)))))))

           (when (equal? this-channel "#emacs")
             (let ((idle-evt (make-channel-idle-event this-channel (*quote-and-headline-interval*))))

               (for-each
                subscribe-proc-to-server-messages!
                (list (channel-idle-event-input-examiner idle-evt)))

               (thread
                (lambda ()
                  ;; re-spew the most recent headline if there's no
                  ;; new news.
                  (let loop ()
                    (let ((headline (sync/timeout (*planet-poll-interval*)
                                                  (irc-session-async-for-news s))))
                      (when headline
                        (sync idle-evt)
                        (pm this-channel
                            (entry->string headline)))
                      (loop)))))))
           (when (equal? this-channel "##cinema")
             (let ((posts #f))
               (thread
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

                 (thread
                  (lambda ()
                    (let loop ()
                      (sync idle)
                      (when posts
                        (notice this-channel
                                (entry->string
                                 (list-ref posts (random (length posts))))))
                      (loop))
                    )))))))

        ((433)
         (error 'respond "Nick already in use!")
         )
        ((NOTICE)
         #t ;; if it's a whine about identd, warn that it's gonna be slow.
         )
        ((PING)
         #t ;; send a PONG
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
        (format "~a~a"
                (*irc-server-name*)
                (zdate)))))

    ;; so we don't have to call flush-output all the time
    (for-each (lambda (p)
                (file-stream-buffer-mode p 'line))
              (list op (*log-output-port*)))

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
             (close-output-port op))])
       (let loop ()
         (let ((line (read-line ip 'return-linefeed)))
           (if (eof-object? line)
               ;; TODO: maybe reconnect
               (vtprintf "eof on server~%")
             (begin
               (with-handlers
                   ([exn:fail:irc-parse?
                     (lambda (e)
                       (vtprintf "couldn't parse line from server: ~s~%" e))])
                 (respond (parse-irc-message line) sess))
               (loop)))))))))

(provide (all-defined-except make-irc-session)
         (rename public-make-irc-session make-irc-session))
)
