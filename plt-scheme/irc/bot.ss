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
         "channel-idle-event.ss"
         "globals.ss"
         "parse.ss"
         "planet-emacsen.ss"
         "quotes.ss"
         "alarm-with-snooze.ss"
         "tinyurl.ss"
         "vprintf.ss")

(define *appearances-by-nick* (make-hash-table 'equal))

(define *message-subscriptions* '())
(define (subscribe-proc-to-server-messages! proc)
  (set! *message-subscriptions* (cons proc *message-subscriptions*)))

(define *planet-emacs-newsfeed* (queue-of-entries
           #:whence
           (and (*use-real-atom-feed?*)
                (lambda ()
                  (get-pure-port
                   (string->url "http://planet.emacsen.org/atom.xml")
                   (list))))))

(define/kw (respond message op)

  (define (out . args)
    (apply fprintf op args)
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

  (define ch-for-us?
    (and (PRIVMSG? message)
         (PRIVMSG-is-for-channel? message)
         (equal? (*my-nick*) (PRIVMSG-approximate-recipient message))))

  (vtprintf " <= ~s~%" message)

  (for-each (lambda (proc)
              (proc message))
            *message-subscriptions*)

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
       *appearances-by-nick*
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
   ((and ch-for-us?
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
          op)))))

   ((and ch-for-us?
         (= 1 (length (PRIVMSG-text-words message))))
    (reply "Eh?  Speak up.")
    )

   ((and ch-for-us?
         (string-ci=? "seen" (second (PRIVMSG-text-words message))))
    (let* ((who (regexp-replace #rx"\\?+$" (third (PRIVMSG-text-words message)) ""))
           (poop (hash-table-get *appearances-by-nick* who #f)))
      (reply (or poop (format "I haven't seen ~a" who)))))

   ((and ch-for-us?
         (string-ci=? "quote" (second (PRIVMSG-text-words message)))
         (reply (one-quote))))

   ((and ch-for-us?
         (string-ci=? "news" (second (PRIVMSG-text-words message))))
    (reply (or (let ((entry (async-channel-try-get *planet-emacs-newsfeed*)))
                 (and entry (entry->string entry)))
               "Sorry, no news yet.")))

   ((or (VERSION? message)
        (and ch-for-us?
             (string-ci=? "version" (second (PRIVMSG-text-words message)))))
    (if (VERSION? message)
        (out "NOTICE ~a :\u0001VERSION ~a\0001~%"
             (PRIVMSG-speaker message)
             (long-version-string))
      (reply (long-version-string))))
   ((or (SOURCE? message)
        (and ch-for-us?
             (string-ci=? "source" (second (PRIVMSG-text-words message)))))
    (let ((source-string
           "http://offby1.ath.cx/~erich/bot/"
           ))
      (if (SOURCE? message)
          (out "NOTICE ~a :\u0001SOURCE ~a\0001~%"
               (PRIVMSG-speaker message)
               source-string)
        (reply source-string))))

   (ch-for-us?
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
                  (let ((headline (sync/timeout 3600 *planet-emacs-newsfeed*)))
                    (when headline
                      (sync idle-evt)
                      (pm this-channel
                          (format "~a" (entry->string headline))))
                    (loop)))))))))

      ((433)
       (error 'respond "Nick already in use!")
       )
      ((NOTICE)
       #t ;; if it's a whine about identd, warn that it's gonna be slow.
       )
      ((PING)
       #t ;; send a PONG
       (out "PONG :~a~%" (car (message-params message))))


      ))))

;(trace respond)

(define (start)
  (let-values (((ip op)
                (if (*irc-server-name*)
                    (tcp-connect (*irc-server-name*) 6667)
                  (values (current-input-port)
                            (current-output-port)))))

    ;; so we don't have to call flush-output all the time
    (file-stream-buffer-mode (*log-output-port*) 'line)

    (fprintf op "NICK ~a~%" (*my-nick*))
    (fprintf op "USER ~a unknown-host ~a :~a, ~a~%"
             (or (getenv "USER") "unknown")
             (*irc-server-name*)
             *client-name*
             (*client-version*))

    (when (*nickserv-password*)
      (fprintf op "PRIVMSG NickServ :identify ~a~%" (*nickserv-password*)))

    ;; TODO -- wait for


    (vtprintf "Sent NICK and USER~%")

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
                       (vtprintf "malformed line from server: ~s => ~s~%"
                                 (exn:fail:irc-parse-string e)
                                 e))])
               (respond (parse-irc-message line) op))
               (loop)))))))))

(provide
 respond
 start)
)
