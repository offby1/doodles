#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(test/text-ui crap-tests 'verbose)"
|#
(module crap mzscheme
(require (lib "async-channel.ss")
         (only (lib "1.ss" "srfi")
               first second third
               filter)
         (lib "trace.ss")
         (only (planet "port.ss" ("schematics" "port.plt" 1 0)) port->string)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "parse.ss")

;; a periodical is a thread that deals with a particular kind of message.
(define-struct periodical (thread consumer-proc id) (make-inspector))

;; if we ever connect to two servers at once, we'd want one instance
;; of this variable local to each server, instead of just one global
;; as it is now.
(define *periodicals* '())

(define (for-each-periodical proc)
  (for-each proc *periodicals*))

(define *task-custodian* (make-custodian))

(define (respond line op)
  ;; cull the dead periodicals.

  (printf "Before culling: ~a periodicals...~%" (length *periodicals*))
  (set! *periodicals* (filter (lambda (d)
                            (not (thread-dead? (periodical-thread d))))
                          *periodicals*))
  (printf "After culling: ~a periodicals...~%" (length *periodicals*))

  ;; parse the line into an optional prefix, a command, and parameters.
  (let ((message (parse-irc-message line)))
    (define (add-periodical! what-to-do
                         interval
                         when-else-to-do-it
                         where-to-do-it)

      (parameterize ((current-custodian *task-custodian*))

        (let* ((ch (make-async-channel))
               (task (thread (lambda ()
                               (let loop ()
                                 (let ((datum (sync/timeout interval ch)))
                                   (printf "periodic thread got datum ~s~%"
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
                                 (lambda (message)
                                   (async-channel-put ch message))
                                 (length *periodicals*))
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
       ((periodical-consumer-proc d) message)))

    (cond
     ((PRIVMSG? message)
      (when (regexp-match #rx"^die!" (PRIVMSG-text message))
        (let ((times-to-run 10))
          (add-periodical!
           (lambda (datum my-channel)
             (when (zero? times-to-run)
               (fprintf op "PRIVMSG ~a :Goodbye, cruel world~%"
                        my-channel)
               (kill-thread (current-thread)))
             (fprintf op "PRIVMSG ~a :~a~%"
                      my-channel times-to-run)
             (set! times-to-run (sub1 times-to-run)))
           3/2
           (lambda (m) #f)
           (first (message-params message)))))
      )
     (else
      (case (message-command message)
        ((001)
         (fprintf op "JOIN #emacs~%")
         (fprintf op "JOIN #bots~%"))

        ((353)
         ;; I suppose it's possible that we might get more than one 353
         ;; message for a given channel, in which case we should
         ;; probably not start a second thread for that channel.
         (add-periodical!
          (lambda (datum my-channel)
            (fprintf op "PRIVMSG ~a :Apple sure sucks.~%"
                     my-channel)
            (printf "waal, ah printed it~%"))
          10
          (lambda (m)
            ;; someone specifically asked
            ;; for a quote
            (regexp-match (pregexp "^.*? quote[[:space:]]*$") (PRIVMSG-text m)))
          (third (message-params message))))

        ((433)
         (error 'respond "Nick already in use!")
         )
        ((NOTICE)
         #t ;; if it's a whine about identd, warn that it's gonna be slow.
         )
        ((PING)
         #t ;; send a PONG
         )
        (else
         (printf "Well, how would _you_ respond to ~s?~%" line))))))
  )
(trace respond)

(define (start)
  (let-values (((ip op)
                (tcp-connect "localhost" 6667)))

    ;; so we don't have to call flush-output all the time
    (file-stream-buffer-mode op 'line)

    (fprintf op "NICK x~a~%" (current-seconds))
    (fprintf op "USER luser unknown-host localhost :rudybot, version whatever~%")
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
      (check-regexp-match
       #rx"JOIN #emacs"
       (read-line ip)
       "didn't join"))
    )

   (test-case
    "starts threads"
    (let-values (((ip op) (make-pipe)))
      (respond
       ":server 353 yournick = #channel :howdy"
       op)
      (sleep 1/10)
      (check-regexp-match
       #rx"Apple sure sucks.$"
       (read-line ip))

      (respond
       ":server 353 mynick <> #gully :howdy"
       op)
      (sleep 1/10)
      (check-regexp-match
       #rx"Apple sure sucks.$"
       (read-line ip))

      ))))

(provide (all-defined))
)

;; Local Variables:
;; compile-command: "mzscheme -M errortrace -qtmv crap.ss -e \"(start)\""
;; End:
