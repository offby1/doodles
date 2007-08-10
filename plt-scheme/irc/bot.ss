#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: bot.ss 4527 2007-08-07 15:03:50Z erich $
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(test/text-ui crap-tests 'verbose)"
|#
(module bot mzscheme
(require (lib "kw.ss")
         (only (lib "1.ss" "srfi")
               first second third
               filter)
         (only (lib "13.ss" "srfi")
               string-join)
         (only (lib "19.ss" "srfi")
               current-date)
         (lib "trace.ss")
         (only (planet "port.ss" ("schematics" "port.plt" 1 0)) port->string)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "zdate.ss" ("offby1" "offby1.plt")) zdate)
         "channel-idle-event.ss"
         "direct-bot-command-evt.ss"
         "globals.ss"
         "parse.ss"
         "planet-emacs-task.ss"
         "quotes.ss"
         "alarm-with-snooze.ss"
         "tinyurl.ss"
         "vprintf.ss")

;; two thunks.  Call "now", and the associated task will run once now.
;; Call "later", and the associated task _won't_ run for a while.  Do
;; neither, and eventually the task will run.
(define-struct control (now later) (make-inspector))

(define *controls-by-channel-and-task*  (make-hash-table 'equal))

(define *task-custodian* (make-custodian))

(define *appearances-by-nick* (make-hash-table 'equal))

(define *message-subscriptions* '())
(define (subscribe-proc-to-server-messages! proc)
  (set! *message-subscriptions* (cons proc *message-subscriptions*)))

(define (start-task! id interval action-thunk)
  (let ((trigger (make-semaphore)))

    ;; it seems odd not to put the thread object in any
    ;; variable, but I can't think of any reason to do so.

    (thread
     (lambda ()
       (let loop ()
         (let ((alarm (make-alarm-with-snooze interval #:id id)))

           (hash-table-put!
            *controls-by-channel-and-task*
            id
            (make-control trigger (alarm-with-snooze-snooze-button alarm)))

           (sync alarm trigger)
           (action-thunk))
         (loop))))))

(define/kw (respond line op #:key [preparsed-message #f])

  (let ((message (or preparsed-message
                     (parse-irc-message line))))

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

        (hash-table-for-each
         *controls-by-channel-and-task*
         (lambda (id control)
           (when (equal? (car id) where)
             ((control-later control)))))

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
          (when (< 50 (string-length url))
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
            ""                          ;ignored because we're passing
                                        ;in a preparsed message
            op
            #:preparsed-message

            ;; *groan* how un-Lispy that I have to spell out all
            ;; these identifiers!
            ;; http://www.paulgraham.com/popular.html
            (make-PRIVMSG
             (message-prefix message)
             (symbol->string (message-command message))
             params
             (PRIVMSG-speaker message)
             (PRIVMSG-destination message)
             (PRIVMSG-approximate-recipient message)
             command
             parsed-command))))))

     ((and ch-for-us?
           (= 1 (length (PRIVMSG-text-words message))))
      (reply "Eh?  Speak up.")
      )

     ((and ch-for-us?
           (string-ci=? "seen" (second (PRIVMSG-text-words message))))
      (let* ((who (regexp-replace #rx"\\?+$" (third (PRIVMSG-text-words message)) ""))
             (poop (hash-table-get *appearances-by-nick* who #f)))
        (reply (or poop (format "I haven't seen ~a" who)))))

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

     ((and ch-for-us?
           (string-ci=? "news" (second (PRIVMSG-text-words message))))
      (cond
       ((hash-table-get
         *controls-by-channel-and-task*
         (cons (PRIVMSG-destination message) 'news-spewer)
         #f)
        =>
        (lambda (c)
          (semaphore-post (control-now c))))
       ))

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
           (vtprintf "Got the 366.~%")
           (when (member this-channel '("#emacs" "#bots" ))
             (vtprintf "It's #emacs or #bots.~%")
             (let ((its-quiet-yeah-too-quiet
                    (make-channel-idle-event
                     this-channel
                     15))
                   (someone-wanted-a-quote
                    (make-direct-bot-command-evt
                     this-channel

                     "quote!")))
               (for-each
                subscribe-proc-to-server-messages!
                (list (direct-bot-command-evt-input-examiner someone-wanted-a-quote)
                      (channel-idle-event-input-examiner its-quiet-yeah-too-quiet)))

               (thread
                (lambda ()
                  (vtprintf "I am the quote-spewing thread.~%")
                  (let loop ()
                    (let ((q (one-quote)))
                      (vtprintf "quote-spewing thread at top of loop; waiting for chance to say ~s~%"
                                q)
                      (sync its-quiet-yeah-too-quiet someone-wanted-a-quote)
                      (vtprintf "quote-spewing thread: there, I said it.~%")
                      (pm this-channel q)
                      (loop)))))))

           (when (and #f
                 (member this-channel '("#emacs" "#scheme-bots")))
             (let ((planet-thing (make-pe-consumer-proc)))
               (start-task!
                (cons this-channel 'news-spewer)
                (*quote-and-headline-interval*)
                (lambda ()
                  (planet-thing
                   (lambda (headline)
                     (pm this-channel headline)))))))))

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
                (tcp-connect (*irc-server-name*) 6667)))

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
    (vtprintf "Sent NICK and USER~%")

    (let loop ()
      (let ((line (read-line ip 'return-linefeed)))
        (if (eof-object? line)
            ;; TODO: maybe reconnect
            (vtprintf "eof on server~%")
          (begin
            (respond line op)
            (loop)))))))

(provide
 respond
 start)
)
