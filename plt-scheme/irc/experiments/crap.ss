#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(test/text-ui crap-tests 'verbose)"
|#
(module crap mzscheme
(require (only (lib "1.ss" "srfi")
               first second third
               filter)
         (only (lib "thread.ss")
               consumer-thread)
         (lib "trace.ss")
         (only (planet "port.ss" ("schematics" "port.plt" 1 0)) port->string)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "parse.ss")

;; a dealer is a thread that deals with a particular kind of message.
(define-struct dealer (thread consumer-proc id) (make-inspector))

;; if we ever connect to two servers at once, we'd want one instance
;; of this variable local to each server, instead of just one global
;; as it is now.
(define *dealers* '())

(define (for-each-dealer proc)
  (for-each proc *dealers*))

(define *task-custodian* (make-custodian))

(define (make-periodic-dealer what-to-do when-to-do-it)
  (parameterize ((current-custodian *task-custodian*))
    (let* ((ch (make-channel))
           (task (thread (lambda ()
                           (let loop ()
                             (let ((datum (sync/timeout 60 ch)))
                               (printf "periodic thread got datum ~s~%"
                                       datum)
                               (when (or

                                      ;; timeout -- channel has been
                                      ;; quiet for a while
                                      (not datum)
                                      (when-to-do-it datum))
                                 (what-to-do datum)))
                             (loop))))))

      (lambda (message)
        (channel-put ch message)))))

(define (respond line ip op)
  (printf "responding to ~s...~%" line)
  ;; cull the dead dealers.
  (let ()
    (printf "Before culling: ~a dealers...~%" (length *dealers*))
    (set! *dealers* (filter (lambda (d)
                              (not (thread-dead? (dealer-thread d))))
                            *dealers*))
    (printf "After culling: ~a dealers...~%" (length *dealers*)))

  ;; parse the line into an optional prefix, a command, and parameters.
  (let ((message (parse-irc-message line)))

    (define (add-dealer! proc)
      (parameterize ((current-custodian *task-custodian*))
      (let-values (((t c) (consumer-thread proc)))
        (set! *dealers* (cons (make-dealer
                               t
                               c
                               (length *dealers*))
                              *dealers*))

        ;; now that we've created a thread, have it run once,
        ;; since it won't otherwise get a chance to run until the
        ;; next time "respond" gets called.
        (c message)
        )))

    ;; pass the message to every dealer, to give them a chance to
    ;; ... deal with it
    (for-each-dealer
     (lambda (d)
       ((dealer-consumer-proc d) message)))

    (cond
     ((PRIVMSG? message)
      #t ;; respond cleverly
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
         (add-dealer!
          (let ((my-channel (third (message-params message))))
            (make-periodic-dealer
             (lambda (datum)
               (when (PRIVMSG? datum)
                 (fprintf op "PRIVMSG ~a :Apple sure sucks.~%"
                          (PRIVMSG-destination datum))
                 (printf "waal, ah printed it~%")))
             (lambda (m)
               ;; someone specifically asked
               ;; for a quote

               (and
                (PRIVMSG? m)
                (equal? (PRIVMSG-destination m) my-channel)
                (regexp-match (pregexp "^.*? quote[[:space:]]*$") (PRIVMSG-text m))))))))

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
            (respond line ip op)
            (loop)))))))


;; The first thing we do, let's kill all the dealers.
(define (kill-all-dealers!)
  (printf "About to shut down custodian what manages all these dudes: ~s~%"
          (custodian-managed-list *task-custodian* (current-custodian)))
  (custodian-shutdown-all *task-custodian*)
  (set! *task-custodian* (make-custodian))
  (set! *dealers* '()))

(define crap-tests

  (test-suite
   "crap"
   #:before
   (lambda ()
     (kill-all-dealers!))
   #:after
   (lambda ()
     (printf "~a dealers:~%" (length *dealers*))
     (for-each-dealer
      (lambda (d)
        (printf "dealer ~s: running: ~a; dead: ~a~%"
                (dealer-id d)
                (if (thread-running? (dealer-thread d))
                    "yes" " no")
                (if (thread-dead? (dealer-thread d))
                    "yes" " no"))))
     (printf "*task-custodian* manages all these dudes: ~s~%"
             (custodian-managed-list *task-custodian* (current-custodian)))
     )
   (test-case
    "join"
    (let-values (((ip op) (make-pipe)))
      (respond
       ":server 001 :welcome"
       (open-input-string "")
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
       ":server 353 :howdy"
       (open-input-string "this particular task doesn't bother reading
so it doesn't matter what we put here.")
       op)
      (sleep 1/10)
      (check-regexp-match
       #rx"Apple sure sucks.$"
       (read-line ip))

      (respond
       ":server 353 :howdy"
       (open-input-string "this particular task doesn't bother reading
so it doesn't matter what we put here.")
       op)
      (sleep 1/10)
      (check-regexp-match
       #rx"Apple sure sucks.$"
       (read-line ip))

      ))))

(provide (all-defined))
)
