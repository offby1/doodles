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
         (only (lib "pregexp.ss") pregexp-quote)
         (only (planet "port.ss" ("schematics" "port.plt" 1 0)) port->string)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "zdate.ss" ("offby1" "offby1.plt")) zdate)
         "globals.ss"
         "parse.ss"
         "planet-emacs-task.ss"
         "quotes.ss"
         "tinyurl.ss")

;; A periodical is a thread that spews into a specific channel, both
;; periodically (hence the name), and optionally in response to having
;; do-it-now! tickled.  Also, tickling back-to-sleep restarts the
;; clock.  (do-it-now! and back-to-sleep are semaphores.)
(define-struct periodical (channel-of-interest thread do-it-now! back-to-sleep id) (make-inspector))

;; if we ever connect to two servers at once, we'd want one instance
;; of this variable local to each server, instead of just one global
;; as it is now.
(define *periodicals-by-id* (make-hash-table 'equal))

(define (for-each-periodical proc)
  (hash-table-for-each *periodicals-by-id* proc))

(define *task-custodian* (make-custodian))

(define *appearances-by-nick* (make-hash-table 'equal))

(define/kw (respond line op #:key [preparsed-message #f])

  ;; cull the dead periodicals.
  (hash-table-for-each
   *periodicals-by-id*
   (lambda (k v)
     (when (thread-dead? (periodical-thread v))
       (hash-table-remove! *periodicals-by-id* k))))

  (let ((message (or preparsed-message
                     (parse-irc-message line))))

    (define (out . args)
      (apply fprintf op args)

      (display (zdate (current-date)) (*log-output-port*))
      (display " => " (*log-output-port*))
      (write (apply format args) (*log-output-port*))
      (newline (*log-output-port*)))

    (define (pm target msg)
      (out "PRIVMSG ~a :~a~%" target msg))
    (define (reply response)
      (pm (if (PRIVMSG-is-for-channel? message)
              (PRIVMSG-destination message)
            (PRIVMSG-speaker message))
          response))
    (define ch-for-us?
      (and (PRIVMSG? message)
           (PRIVMSG-is-for-channel? message)
           (equal? (*my-nick*) (PRIVMSG-approximate-recipient message))))

    (define/kw (add-periodical! what-to-do
                                interval
                                where-to-do-it
                                #:key
                                [id (hash-table-count *periodicals-by-id*)])

      (parameterize ((current-custodian *task-custodian*))

        (let* ((do-it-now!    (make-semaphore 1))
               (back-to-sleep (make-semaphore 0))
               (task (thread (lambda ()
                               (let loop ()
                                 (let ((datum (sync/timeout
                                               interval
                                               do-it-now!
                                               back-to-sleep)))
                                   (when (or (not datum)
                                             (equal? datum do-it-now!))
                                     (what-to-do where-to-do-it))

                                   (loop)
                                   )
                                 (loop))))))

          (hash-table-put!
           *periodicals-by-id*
           (cons id where-to-do-it)
           (make-periodical
            where-to-do-it
            task
            do-it-now!
            back-to-sleep
            id)))))

    (when (and (PRIVMSG? message)
               (PRIVMSG-is-for-channel? message))
      (let ((who         (PRIVMSG-speaker     message))
            (where       (PRIVMSG-destination message))
            (what        (PRIVMSG-text        message))
            (time        (current-seconds))
            (was-action? (ACTION?             message)))

        (for-each-periodical
         (lambda (id p)
           (when (equal? (periodical-channel-of-interest p)
                         where)
             (semaphore-post (periodical-back-to-sleep p)))))

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

    ;; disabled until I deal with TODO items
    (cond
     ((and
       #f
       (PRIVMSG? message)
       (regexp-match url-regexp (PRIVMSG-text message)))
      =>
      (lambda (match-data)
        (reply (make-tiny-url (car match-data))
               ))))

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
        (add-periodical!
         (lambda (my-channel)
           (sleep 10)
           (respond
            ""                          ;ignored
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
             parsed-command))
           (kill-thread (current-thread)))
         1                              ;irrelevant
         (PRIVMSG-destination message)
         #:id (format "delayed command ~s" command)
         )))

     ((and ch-for-us?
           (string-ci=? "die!" (second (PRIVMSG-text-words message))))
      (let ((times-to-run 10))
        (add-periodical!
         (lambda (my-channel)
           (when (zero? times-to-run)
             (pm my-channel "Goodbye, cruel world")
             (kill-thread (current-thread)))
           (pm my-channel (format "~a" times-to-run))
           (set! times-to-run (sub1 times-to-run)))
         3/2
         (first (message-params message))
         #:id "auto self-destruct sequence")))

     ((and ch-for-us?
           (string-ci=? "seen" (second (PRIVMSG-text-words message))))
      (let* ((who (regexp-replace #rx"\\?+$" (third (PRIVMSG-text-words message)) ""))
             (poop (hash-table-get *appearances-by-nick* who #f)))
        (reply (or poop (format "I haven't seen ~a" who)))))

     ((or (VERSION? message)
          (and ch-for-us?
               (string-ci=? "version" (second (PRIVMSG-text-words message)))))
      (let ((version-string (format
                             "~a (offby1@blarg.net):~a:~a"
                             *client-name*
                             *client-version-number*
                             *client-environment*)))
        (if (VERSION? message)
            (fprintf op "NOTICE ~a :\u0001VERSION ~a\0001~%"
                     (PRIVMSG-speaker message)
                     version-string)
          (reply version-string))))
     ((or (SOURCE? message)
          (and ch-for-us?
               (string-ci=? "source" (second (PRIVMSG-text-words message)))))
      (let ((source-string
             "svn+ssh://irc-bot@offby1.ath.cx/home/erich/svn-repos/trunk/doodles/plt-scheme/irc (but you'll need to contact me first, lest ye get an \"authorization denied\" error)"
             ))
        (if (SOURCE? message)
            (fprintf op "NOTICE ~a :\u0001SOURCE ~a\0001~%"
                     (PRIVMSG-speaker message)
                     source-string)
          (reply source-string))))
     ((and ch-for-us?
           (string-ci=? "quote" (second (PRIVMSG-text-words message))))
      (cond
       ((hash-table-get *periodicals-by-id* (cons 'quote-spewer (PRIVMSG-destination message)) #f)
        =>
        (lambda (p)
          (semaphore-post (periodical-do-it-now! p))))
       ))
     ((and ch-for-us?
           (string-ci=? "news" (second (PRIVMSG-text-words message))))
      (cond
       ((hash-table-get *periodicals-by-id* (cons 'news-spewer (PRIVMSG-destination message)) #f)
        =>
        (lambda (p)
          (semaphore-post (periodical-do-it-now! p))))
       ))
     (ch-for-us?
      (reply "\u0001ACTION is at a loss for words, as usual\u0001"))
     (else
      (case (message-command message)
        ((001)
         (for-each (lambda (cn)
                     (printf "Joining ~a~%" cn)
                     (fprintf op "JOIN ~a~%" cn))
                   (*initial-channel-names*)))

        ((366)
         (let ((this-channel (second (message-params message))))

           (when (member this-channel '("#emacs" "#bots" ))
             (add-periodical!
              (lambda (my-channel)
                (pm my-channel (one-quote)))
              (* 11/10 (*quote-and-headline-interval*))
              this-channel
              #:id 'quote-spewer))

           (when (member this-channel '("#emacs" "#scheme-bots"))
             (let ((planet-thing (make-pe-consumer-proc)))
               (add-periodical!
                (lambda (my-channel)
                  (planet-thing
                   (lambda (headline)
                     (pm my-channel headline))))

                (*quote-and-headline-interval*)
                this-channel
                #:id 'news-spewer)))))

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
    (printf "Sent NICK and USER~%")

    (let loop ()
      (let ((line (read-line ip 'return-linefeed)))
        (fprintf (*log-output-port*) "~a <= ~s~%"
                 (zdate (current-date))
                 line)
        (if (eof-object? line)
            ;; TODO: maybe reconnect
            (printf "eof on server~%")
          (begin
            (respond line op)
            (loop)))))))


;; The first thing we do, let's kill all the periodicals.
(define (kill-all-periodicals!)
  (printf "About to shut down custodian what manages all these dudes: ~s~%"
          (custodian-managed-list *task-custodian* (current-custodian)))
  (custodian-shutdown-all *task-custodian*)
  (set! *task-custodian* (make-custodian))
  (set! *periodicals-by-id* (make-hash-table 'equal)))
;; returns #f if we didn't find what we're looking for.

(define (expect/timeout ip regex seconds)
  (let* ((ch (make-channel))
         (reader
          (thread
           (lambda ()
             (let loop ()
               (printf "expect/timeout about to look for ~s from ~s ...~%"
                       regex
                       (object-name ip))
               (let ((line (read-line ip)))
                 (cond
                  ((eof-object? line)
                   (printf "expect/timeout: eof~%")
                   (channel-put ch #f))
                  ((regexp-match regex line)
                   (printf "expect/timeout: Got match!~%")
                   (channel-put ch #t))
                  (else
                   (printf "expect/timeout: nope; retrying~%")
                   (loop)))

                 ))))))
    (and (sync/timeout seconds ch)
         ch)))

(define crap-tests

  (test-suite
   "crap"
   #:before
   (lambda ()
     (kill-all-periodicals!))
   #:after
   (lambda ()
     (printf "~a periodicals:~%" (hash-table-count *periodicals-by-id*))
     (for-each-periodical
      (lambda (id p)
        (printf "periodical ~s: running: ~a; dead: ~a~%"
                (periodical-id p)
                (if (thread-running? (periodical-thread p))
                    "yes" " no")
                (if (thread-dead? (periodical-thread p))
                    "yes" " no"))))
     (printf "*task-custodian* manages all these dudes: ~s~%"
             (custodian-managed-list *task-custodian* (current-custodian)))
     )
   (test-case
    "join"
    (let-values (((ip op) (make-pipe)))
      (respond
       ":server 001 :welcome"
       op)
      (sleep 1/10)
      (check-not-false
       (expect/timeout ip #rx"JOIN #bots" 2)
       "didn't join"))
    )

   (test-case
    "starts threads"
    (let-values (((ip op) (make-pipe)))
      (respond
       ":server 366 yournick #channel :End of NAMES, dude."
       op)

      (check-not-false (expect/timeout  ip #rx"Apple sure sucks.$" 10))

      (respond
       ":server 366 mynick #gully :drop dead"
       op)

      (check-not-false (expect/timeout ip #rx"Apple sure sucks.$" 10))

      ))))

(provide (all-defined))
)
