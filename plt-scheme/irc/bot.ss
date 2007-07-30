#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; http://tools.ietf.org/html/rfc1459

(module bot mzscheme
(require (lib "async-channel.ss")
         (lib "trace.ss")
         (only (lib "pregexp.ss") pregexp-quote)
         (only (planet "rfc3339.ss" ("neil" "rfc3339.plt"))
               rfc3339-string->srfi19-date/constructor)
         (only (lib "1.ss" "srfi")
               first
               second
               third
               )
         (only (lib "13.ss" "srfi")
               string-join
               string-tokenize
               )
         (only (lib "14.ss" "srfi")
               char-set
               char-set-complement
               char-set:whitespace)
         (rename (lib "19.ss" "srfi") 19:make-date make-date)
         (only (lib "19.ss" "srfi")
               add-duration
               current-date
               date->time-utc
               make-time
               time-duration
               time-utc->date
               time>?
               )
         (only (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
               sxpath)
         (only (planet "zdate.ss" ("offby1" "offby1.plt")) zdate)
         "parse-message.ss"
         "globals.ss"
         "jordanb.ss"
         "planet-emacsen.ss"
         "vprintf.ss"
         "../web/quote-of-the-day.ss")
(provide callback)

(define (split str)
  (string-tokenize str (char-set-complement char-set:whitespace)))

(define (strip-leading-colon str)
  (if (char=? #\: (string-ref str 0))
        (substring str 1)
      str))

(print-hash-table #t)

;; http://tools.ietf.org/html/rfc1459#section-2.3.1
(define (parse-prefix str)
  (if (not str)
      '(#f #f #f)

    (let loop ((result (string-tokenize str (char-set-complement (char-set #\! #\@)))))
      (if (<= (length result ) 3)
          result
        (loop (cons #f result))))))

;; during normal operation we want our bot to act randomly.  But when
;; we're testing it, we want it to act predictably.  Thus we have a
;; parameter called *random?* which determines which way it acts.

;; I know that I could start it with a known seed when I test, but for
;; reasons I can't articulate, that seems less pleasant than simply
;; having "rnd" always return 0.
(define (rnd . args)
  (if (not (*random?*))
      0
    (apply random args)))

(define (random-choice seq)
  (list-ref seq (rnd (length seq))))

;; Calls THUNK every SECONDS seconds.  Calling the return value with
;; the symbol POSTPONE postpones the next call (i.e., it resets the
;; timer).  Calling the return value with any other value kills the
;; task permanently.

;; TODO -- maybe make a wrapper for this called
;; "do-when-channel-idle", which saves the task someplace out of the
;; way, monitors the named channel, and cancels the task as needed.
;; Right now those things are being done in "callback".
(define (do-in-loop seconds thunk)
  (let* ((s (make-semaphore))
         (t (thread (lambda ()
                      (let loop ()
                        (let ((postponed? (sync/timeout seconds s)))
                          (when (not postponed?)
                            (thunk)))
                        (loop))))))
    (lambda (command)
      (case command
        ((postpone POSTPONE) (semaphore-post s))
        (else (kill-thread t))))))

(define *jordanb-quote-tasks-by-channel* (make-hash-table 'equal))
(define *planet-emacs-task* #f)

(define-struct utterance (when what action?) (make-inspector))
(define put #f)

(define callback
  (let ((state 'initial))
    (lambda (line ip op)
      (define (do-something-clever
               message-tokens           ;what they said
               requestor                ;who said it
               channel-name             ;where they said it
               was-private?             ;how they said it
               CTCP-message?            ;was it a CTCP?
               )

        ;; maybe throw away some of the initial tokens, since they're
        ;; not interesting.

        ;; was-private?         CTCP-message?           which tokens
        ;; #f                   #f                      first two (channel name, my name)
        ;; #f                   #t                      none
        ;; #t                   #f                      first one (my name)
        ;; #t                   #t                      none
        (when (not CTCP-message?)
          (set! message-tokens
                ((if was-private? cdr cddr)
                 message-tokens)))
        (cond
         (CTCP-message?
          =>
          (lambda (msg)
            (vprintf "Ooh, ~s is a CTCP message yielding ~s~%"
                     message-tokens
                     msg)
            (cond
             ((string=? "VERSION" (first message-tokens))
              (put (format "NOTICE ~a :\001VERSION ~a (offby1@blarg.net):~a:~a\001"
                           requestor
                           *client-name*
                           *client-version-number*
                           *client-environment*))))))
         ((and (hash-table-get *jordanb-quote-tasks-by-channel* channel-name #f)
               (string-ci=? "shaddap" (first message-tokens)))
          ((hash-table-get *jordanb-quote-tasks-by-channel* channel-name) 'kill)
          ;; TODO -- maybe now do hash-table-remove! so that the next
          ;; time we see a 353, we recreate the task.
          )
         (else
          (let ((response-body
                 (cond
                  ;; if the requestor seems to be a bot, don't respond normally.
                  ((regexp-match (pregexp "bot[^[:space:][:alnum:]]*$") requestor)
                   "\u0001ACTION holds his tongue.\u0001")

                  ((and (string-ci=? "seen" (first message-tokens))
                        (< 1 (length message-tokens)))
                   (let* ((nick (second message-tokens))
                          (times-by-nick (hash-table-get
                                          times-by-nick-by-channel
                                          channel-name
                                          (make-hash-table 'equal)))
                          (data (hash-table-get times-by-nick nick #f)))
                     (vprintf "Sought key ~s in times-by-nick; got ~s~%"
                              nick data)
                     (cond
                      ((not data)
                       (format "I haven't seen ~a in ~a" nick channel-name))
                      ((utterance-action? data)
                       (format "~a's last action in ~a was at ~a: ~a ~a"
                               nick
                               channel-name
                               (zdate (utterance-when data))
                               nick
                               (utterance-what data)))
                      (else
                       (format "~a last spoke in ~a at ~a, saying \"~a\""
                               nick
                               channel-name
                               (zdate (utterance-when data))
                               (utterance-what data))))))

                  ((regexp-match #rx"(?i:^quote)( .*$)?" (first message-tokens))
                   (let try-again ()
                     (let ((r  (rnd 100)))
                       ;; we special-case zero for ease of testing.
                       (cond ((zero? r)
                              "I've laid out in my will that my heirs should continue working on my .emacs -- johnw")
                             ((< r 91)
                              ;; TODO: here's a cute idea -- if
                              ;; requestor appears to be jordanb
                              ;; himself, return something utterly
                              ;; unlike the usual jordanb quote --
                              ;; something saccharine and Hallmark-y
                              (one-jordanb-quote))
                             (else
                              (with-handlers
                                  ((exn:fail:network?
                                    (lambda (e)
                                      (vtprintf "Warning: quote-of-the-day yielded an exception: ~a~%"
                                                (exn-message e))
                                      (try-again))))
                                (let ((p (random-choice (quotes-of-the-day))))
                                  (string-append (car p)
                                                 "  --"
                                                 (cdr p)))))))))
                  (else
                   "\u0001ACTION is at a loss for words.\u0001"))))

            (put (format "PRIVMSG ~a :~a"
                         (if was-private? requestor channel-name)
                         response-body))))))

      (set! put
            (lambda
                (str)
              (let ((str (substring str 0 (min 510 (string-length str)))))
                (vtprintf "=> ~s~%" str)
                (display str op)
                (newline op)
                (flush-output op))))

      (let ((line (substring line 0 (min 510 (string-length line)))))
        (let-values (((prefix command params)
                      (parse-message line)))
          (let ((prefix (parse-prefix prefix)))
            (vtprintf "<= ~s -> prefix ~s; command ~s params ~s ~%"
                      line
                      prefix
                      command
                      params)
            (case state
              ((initial)
               (put (format "NICK ~a" (*my-nick*)))
               (put (format "USER ~a ~a ~a :~a, ~a"
                            (getenv "USER")
                            "unknown-host"
                            (*irc-server-name*)
                            *client-name*
                            (*client-version*)))
               (set! state 'waiting-for-login-confirmation)))

            (let ((command-number (and (regexp-match (pregexp "^[[:digit:]]{3}$") command )
                                       (string->number command)))
                  (command-symbol (and (regexp-match (pregexp "^[[:alpha:]]+$") command)
                                       (string->symbol command))))
              (case command-number
                ((001)
                 (set! state 'logged-in)
                 (vprintf "Ah, I see we logged in OK.~%")
                 (for-each (lambda (ch)
                             (put (format "JOIN ~a" ch)))
                           (*initial-channel-names*)))
                ((353)
                 ;; response to "NAMES".  This implies we've joined a
                 ;; channel (or issued a NAMES command ourselves,
                 ;; which we don't do, afaik)
                 (let* ((tokens (split params))
                        (channel (third tokens)))

                   (when (and (or
                               (string=? channel "#bots")
                               (string=? channel "#emacs")))
                     (when
                         ;; I wonder ... would it be better if I
                         ;; killed any existing task, and then
                         ;; replaced it with a new one?
                         (not (hash-table-get *jordanb-quote-tasks-by-channel* channel #f))
                       (hash-table-put!
                        *jordanb-quote-tasks-by-channel*
                        channel
                        (do-in-loop
                         (*jordanb-quote-interval*)
                         (lambda ()
                           (put (format "PRIVMSG ~a :~a"
                                        channel
                                        (one-jordanb-quote)))))))

                     (when (not *planet-emacs-task*)
                       (set! *planet-emacs-task*
                             (let ((the-queue (queue-of-entries))
                                   (number-spewed 0)
                                   (time-of-latest-spewed-entry
                                    (date->time-utc
                                     (rfc3339-string->srfi19-date/constructor
                                      "2000-00-00T00:00:00+00:00"
                                      19:make-date))))
                               (do-in-loop
                                ;; choosing the "correct" interval
                                ;; here is subtle.  Ideally the
                                ;; interval would have the property
                                ;; that the channel goes silent for
                                ;; this long just as often as someone
                                ;; posts a blog to planet emacs --
                                ;; that way we consume items from the
                                ;; channel at the same rate that
                                ;; queue-of-entries-since produces
                                ;; them.  Failing that, it's perhaps
                                ;; best for this number to be a bit
                                ;; smaller than that idea, so that
                                ;; this task finds nothing new
                                ;; occasionally.
                                20
                                (lambda ()
                                  (let ((datum (async-channel-try-get the-queue)))
                                    ;; spew any _new_ entries that we
                                    ;; haven't already spewed ... but
                                    ;; also spew the single newest entry
                                    ;; even if it's kind of old.
                                    (if (or
                                         (zero? number-spewed)
                                         (and datum
                                              (time>?
                                               (entry-timestamp datum)
                                               time-of-latest-spewed-entry)))
                                        (begin
                                          (put (format "PRIVMSG ~a :~a"
                                                       channel
                                                       (entry->string datum)))
                                          (set! number-spewed (add1 number-spewed))
                                          (when (time>?
                                                 (entry-timestamp datum)
                                                 time-of-latest-spewed-entry)
                                            (set! time-of-latest-spewed-entry
                                                  (entry-timestamp datum))))
                                      (vtprintf "Nothing new on planet emacs~%")))
                                  ))))))

                   ))
                ((433)
                 (vtprintf "Gaah!!  One of those \"nick already in use\" messages!~%")
                 (vtprintf "I don't know how to deal with that~%")
                 (vtprintf "Just start me up again and provide a different nick on the command line, I guess :-|~%")))

              (case command-symbol
                ((PRIVMSG)
                 (let* ((CTCP-message (cond
                                       ((regexp-match #rx"\u0001(.*)\u0001$" params)
                                        => second)
                                       (else #f)))
                        (tokens
                         (cond
                          (CTCP-message => split)
                          (else (split params))))
                        (tokens
                         ;; I can't remember why I'm removing this colon
                         (if (<= 2 (length tokens))
                             (cons (first tokens)
                                   (cons (regexp-replace
                                          #rx"^:"
                                          (second tokens)
                                          "")
                                         (cddr tokens)))
                           tokens))
                        (destination (car (split params)))
                        (dest-is-channel? (regexp-match #rx"^#" destination))
                        (source (car prefix)))

                   (vprintf "CTCP ~s Tokens ~s destination ~s source ~s~%"
                            CTCP-message
                            tokens destination source)
                   (when dest-is-channel?
                     (for-each
                      (lambda (task)
                        (when task
                          (task 'postpone)))
                      (list
                       (hash-table-get *jordanb-quote-tasks-by-channel* destination #f)
                       *planet-emacs-task*))

                     (let* ((times-by-nick (hash-table-get
                                            times-by-nick-by-channel
                                            destination
                                            (lambda ()
                                              (make-hash-table 'equal))))
                            (utterance (make-utterance
                                        (seconds->date (current-seconds))
                                        (string-join (cdr tokens))
                                        (not (not CTCP-message)))))
                       (hash-table-put! times-by-nick source utterance)
                       (hash-table-put! times-by-nick-by-channel destination times-by-nick)))

                   (unless (*passive?*)

                     (cond
                      ;; private message to us.
                      ((equal? (*my-nick*) destination)
                       (do-something-clever
                        tokens
                        source
                        destination
                        #t
                        CTCP-message))

                      ;; someone said something to the whole channel.
                      (dest-is-channel?

                       ;; ... but prefixed it with my nick.
                       (when (regexp-match
                              (regexp
                               (string-append
                                "^"
                                (pregexp-quote (*my-nick*))
                                "[:,]"))
                              (cadr tokens))
                         (do-something-clever
                          tokens
                          source
                          destination
                          #f
                          CTCP-message))))))
                 )
                ((NOTICE)
                 (when (regexp-match #rx"No identd \\(auth\\) response" params)
                   (fprintf
                    (current-error-port)
                    "This is one of those servers that wants us to run identd.  Be patient; it'll take two minutes to connect.~%")
                   ))
                ((PING)
                 (put (format "PONG ~a" params)))))))))))

(define times-by-nick-by-channel (make-hash-table 'equal))
)