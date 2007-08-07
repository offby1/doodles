#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(test/text-ui crap-tests 'verbose)"
|#
(module crap mzscheme
(require (lib "kw.ss")
         (lib "async-channel.ss")
         (only (lib "1.ss" "srfi")
               first second third
               filter)
         (lib "trace.ss")
         (only (planet "port.ss" ("schematics" "port.plt" 1 0)) port->string)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "zdate.ss" ("offby1" "offby1.plt")) zdate)
         "globals.ss"
         "parse.ss")

;; A periodical is a thread that spews into a specific channel, both
;; periodically (hence the name), and optionally in response to a
;; message.
(define-struct periodical (thread async-channel id) (make-inspector))

;; if we ever connect to two servers at once, we'd want one instance
;; of this variable local to each server, instead of just one global
;; as it is now.
(define *periodicals* '())

(define (for-each-periodical proc)
  (for-each proc *periodicals*))

(define *task-custodian* (make-custodian))

(define *appearances-by-nick* (make-hash-table 'equal))

(define (respond line op)

  ;; cull the dead periodicals.
  (set! *periodicals* (filter (lambda (d)
                            (not (thread-dead? (periodical-thread d))))
                          *periodicals*))

  (let ((message (parse-irc-message line)))

    (define (out . args)
      (apply fprintf op args)
      (display "=> " (*log-output-port*))
      (display (apply format args) (*log-output-port*)))

    (define (pm target msg)
      (out "PRIVMSG ~a :~a~%" target msg))
    (define (reply response)
      (pm (if (PRIVMSG-is-for-channel? message)
              (PRIVMSG-destination message)
            (PRIVMSG-speaker message))
          response))
    (define/kw (add-periodical! what-to-do
                                interval
                                when-else-to-do-it
                                where-to-do-it
                                #:key
                                [id (length *periodicals*)])

      (parameterize ((current-custodian *task-custodian*))

        (let* ((ch (make-async-channel))
               (task (thread (lambda ()
                               (let loop ()
                                 (let ((datum (sync/timeout interval ch)))
                                   (printf "periodic thread ~s got datum ~s~%"
                                           id
                                           datum)
                                   (when (or
                                          ;; timeout -- channel has been
                                          ;; quiet for a while
                                          (not datum)
                                          (and
                                           (PRIVMSG? datum)
                                           (equal? (PRIVMSG-destination datum) where-to-do-it)
                                           (when-else-to-do-it datum))
                                          )
                                     (what-to-do datum where-to-do-it)))
                                 (loop))))))

          (set! *periodicals* (cons (make-periodical
                                     task
                                     ch
                                     id)
                                    *periodicals*))


          ;; now that we've created a thread, have it run once,
          ;; since it won't otherwise get a chance to run until the
          ;; next time "respond" gets called.
          (async-channel-put ch message))))

    (printf "responding to ~s...~%" message)

    ;; pass the message to every periodical, to give them a chance to
    ;; ... deal with it
    (for-each-periodical
     (lambda (d)
       (async-channel-put (periodical-async-channel d) message)))

    (when (and (PRIVMSG? message)
               (PRIVMSG-is-for-channel? message))
      ;; note who did what, when, where, how, and wearing what kind of
      ;; skirt; so that later we can respond to "seen Ted?"
      (let ((who         (PRIVMSG-speaker     message))
            (where       (PRIVMSG-destination message))
            (what        (PRIVMSG-text        message))
            (when        (current-seconds))
            (was-action? (ACTION?             message)))
        (let ((the-skinny (format "~a~a in ~a~a ~a~a"
                                  who
                                  (if was-action? "'s last action" " last spoke")
                                  where
                                  (if was-action? " was at"        ""           )
                                  (zdate (seconds->date when))
                                  (if was-action?
                                      (format ": ~a ~a" who what)
                                    (format ", saying \"~a\"" what)))))
          (hash-table-put! *appearances-by-nick* who the-skinny)
          )))

    (cond
     ((and (ACTION? message)
           (regexp-match #rx"glances around nervously" (PRIVMSG-text message)))
      (reply "\u0001ACTION loosens his collar with his index finger\u0001"))

     ((and (PRIVMSG? message)
           (regexp-match #rx"^die!" (PRIVMSG-text message)))
      (let ((times-to-run 10))
        (add-periodical!
         (lambda (datum my-channel)
           (when (zero? times-to-run)
             (pm my-channel "Goodbye, cruel world")
             (kill-thread (current-thread)))
           (pm my-channel (format "~a" times-to-run))
           (set! times-to-run (sub1 times-to-run)))
         3/2
         (lambda (m) #f)
         (first (message-params message))
         #:id "auto self-destruct sequence")))

     ((and (PRIVMSG? message)
           (pair? (PRIVMSG-text-words message))
           (string-ci=? "seen" (first (PRIVMSG-text-words message))))
      (let* ((who (regexp-replace #rx"\\?+$" (second (PRIVMSG-text-words message)) ""))
             (poop (hash-table-get *appearances-by-nick* who #f)))
        (reply (or poop (format "I haven't seen ~a" who)))))

     ((or (VERSION? message)
          (and (PRIVMSG? message)
               (equal? (*my-nick*) (PRIVMSG-approximate-recipient message))
               (< 1 (length (PRIVMSG-text-words message)))
               (string-ci=? "version" (second (PRIVMSG-text-words message)))))
      (let ((version-string "none of your damned business"))
        (if (VERSION? message)
            (fprintf op "NOTICE ~a :\u0001VERSION ~a\0001~%"
                     (PRIVMSG-speaker message)
                     version-string)
          (reply version-string))))
     ((or (SOURCE? message)
          (and (PRIVMSG? message)
               (equal? (*my-nick*) (PRIVMSG-approximate-recipient message))
               (< 1 (length (PRIVMSG-text-words message)))
               (string-ci=? "source" (second (PRIVMSG-text-words message)))))
      (let ((source-string
             "not yet publically released, but the author would be willing if asked nicely"))
        (if (SOURCE? message)
            (fprintf op "NOTICE ~a :\u0001SOURCE ~a\0001~%"
                     (PRIVMSG-speaker message)
                     source-string)
          (reply source-string))))
     (else
      (case (message-command message)
        ((001)
         (printf "Joined a couple o' channels~%")
         (for-each (lambda (cn)
                     (fprintf op "JOIN ~a~%" cn))
                   (*initial-channel-names*)))

        ((366)
         (add-periodical!
          (lambda (datum my-channel)
            (pm my-channel "Apple sure sucks."))
          (*quote-and-headline-interval*)
          (lambda (m)
            ;; someone specifically asked
            ;; for a quote
            (let ((w (PRIVMSG-text-words m)))
              (and (< 1 (length w))
                   (string-ci=? "quote" (second w)))))
          (second (message-params message))
          #:id "funny quotes"))

        ((433)
         (error 'respond "Nick already in use!")
         )
        ((NOTICE)
         #t ;; if it's a whine about identd, warn that it's gonna be slow.
         )
        ((PING)
         #t ;; send a PONG
         )
        ((JOIN)
         (pm (car (message-params message))
             (format "Howdy, ~a" (message-prefix message))))

        (else
         (printf "Well, how would _you_ respond to ~s?~%" message)))))))

;(trace respond)

(define (start)
  (let-values (((ip op)
                (tcp-connect (*irc-server-name*) 6667)))

    ;; so we don't have to call flush-output all the time
    (file-stream-buffer-mode op 'line)

    (fprintf op "NICK ~a~%" (*my-nick*))
    (fprintf op "USER ~a unknown-host ~a :~a, ~a~%"
             (or (getenv "USER") "unknown")
             (*irc-server-name*)
             *client-name*
             (*client-version*))
    (printf "Sent NICK and USER~%")

    (let loop ()
      (let ((line (read-line ip 'return-linefeed)))
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
  (set! *periodicals* '()))
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
     (printf "~a periodicals:~%" (length *periodicals*))
     (for-each-periodical
      (lambda (d)
        (printf "periodical ~s: running: ~a; dead: ~a~%"
                (periodical-id d)
                (if (thread-running? (periodical-thread d))
                    "yes" " no")
                (if (thread-dead? (periodical-thread d))
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

;; Local Variables:
;; compile-command: "mzscheme -M errortrace -qtmv crap.ss -e \"(start)\""
;; End:
