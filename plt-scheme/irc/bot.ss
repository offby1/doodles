#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; http://tools.ietf.org/html/rfc1459

(module bot mzscheme
(require (lib "async-channel.ss")
         (lib "trace.ss")
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
               char-set-complement)
         (rename (lib "19.ss" "srfi") 19:make-date make-date)
         (only (lib "19.ss" "srfi")
               add-duration
               current-date
               date->time-utc
               make-time
               time-duration
               time-utc->date
               )
         (only (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
               sxpath)
         (only (planet "zdate.ss" ("offby1" "offby1.plt")) zdate)
         "parse-message.ss"
         "globals.ss"
         "jordan.ss"
         "planet-emacsen.ss"
         "vprintf.ss"
         "../web/quote-of-the-day.ss")
(provide callback)

(*client-version* "$Rev$")

(define (split str)
  (string-tokenize str (char-set-complement (char-set #\space))))

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

;; gaah.  I wish wish wish that PLT scheme supported Olin Shiver's
;; "SRE"s.  Maybe I should contribute to
;; http://www.omnigia.com/scheme/scsh-regexp/.
(define (regexp-quote str)
  (regexp-replace* #rx"." str "\\\\&"))

;; Calls THUNK every SECONDS seconds.  Calling the return value with
;; the symbol POSTPONE postpones the next call (i.e., it resets the
;; timer).  Calling the return value with any other value kills the
;; task permanently.
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

(define-struct utterance (when what) (make-inspector))

(define callback
  (let ((state 'initial))
    (lambda (line ip op)
      (define (put str)
        (let ((str (substring str 0 (min 510 (string-length str)))))
          (vprintf "=> ~s~%" str)
          (display str op)
          (newline op)
          (flush-output op)))

      (define (do-something-clever
               message-tokens           ;what they said
               requestor                ;who said it
               channel-name             ;where they said it
               was-private?             ;how they said it
               )

        (vprintf "message ~s requestor ~s channel-name ~s was-private? ~s~%"
                  message-tokens requestor channel-name was-private?)
        (cond
         ;; TODO -- proper CTCP decoding.  See
         ;; http://www.irchelp.org/irchelp/rfc/ctcpspec.html
         ((string=? "\u001VERSION\u001" (first message-tokens))
          (put (format "NOTICE ~a :\001VERSION ~a (offby1@blarg.net):~a:~a\001"
                       requestor
                       *client-name*
                       *client-version-number*
                       *client-environment*)))
         ((and (hash-table-get *jordanb-quote-tasks-by-channel* channel-name #f)
               (string-ci=? "shaddap" (first message-tokens)))
          ((hash-table-get *jordanb-quote-tasks-by-channel* channel-name) 'kill))
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
                     (if data
                         (format "~a last spoke at ~a, saying \"~a\""
                                 nick
                                 (zdate (utterance-when data))
                                 (utterance-what data))
                       (format "I haven't seen ~a" nick))))

                  ((regexp-match #rx"(?i:^quote)( .*$)?" (first message-tokens))
                   (let ((r  (rnd 100)))

                     ;; we special-case zero for ease of testing.
                     (cond ((zero? r)
                            "I've laid out in my will that my heirs should continue working on my .emacs -- johnw")
                           ((< r 91)
                            (one-jordanb-quote))
                           (else
                            (let ((p (random-choice (quotes-of-the-day))))
                              (string-append (car p)
                                             "  --"
                                             (cdr p)))))
                     ))
                  (else
                   (format "Well, ~a, I think ~a too."
                           requestor
                           (string-join message-tokens))))))

            (put (format "PRIVMSG ~a :~a"
                         (if was-private? requestor channel-name)
                         response-body))))))

      (let ((line (substring line 0 (min 510 (string-length line)))))
        (let-values (((prefix command params)
                      (parse-message line)))
          (let ((prefix (parse-prefix prefix)))
            (vprintf "<= ~s -> prefix ~s; command ~s params ~s ~%"
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
                            *client-version*))
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
                ((311 312 317)
                 (printf "Woot -- I got a ~a response to my WHOIS! ~s~%"
                         command-number
                         params))
                ((353)
                 ;; response to "NAMES".  This implies we've issued a
                 ;; NAMES command ourselves (which we don't do, afaik),
                 ;; or else we've joined a channel.
                 (let* ((tokens (split params))
                        (fellows (cdddr tokens))
                        (fellows (cons (strip-leading-colon (car fellows))
                                       (cdr fellows)))
                        (channel (third tokens)))

                   (when (and (or
                               (string=? channel "#bots")
                               (string=? channel "#emacs"))
                              ;; I wonder ... would it be better if I
                              ;; killed any existing task, and then
                              ;; replaced it with a new one?
                              (not (hash-table-get *jordanb-quote-tasks-by-channel* channel #f)))
                     (hash-table-put!
                      *jordanb-quote-tasks-by-channel*
                      channel
                      (do-in-loop
                       (*jordanb-quote-interval*)
                       (lambda ()
                         (put (format "PRIVMSG ~a :~a"
                                      channel
                                      (one-jordanb-quote)))))))

                   (when (and (string=? channel "#emacs")
                              (not *planet-emacs-task*))
                     (set! *planet-emacs-task*
                           (let ((the-channel
                                  (channel-of-entries-since
                                   (rfc3339-string->srfi19-date/constructor
                                    "2007-07-16T19:59:00+00:00"
                                    19:make-date)
                                   ;;(current-date)
                                   )))
                             (do-in-loop
                              ;; choosing the "correct" interval here is
                              ;; subtle.  Ideally the interval would
                              ;; have the property that the channel
                              ;; goes silent for this long
                              ;; just as often as someone posts a blog
                              ;; to planet emacs -- that way we
                              ;; consume items from the channel at the
                              ;; same rate that
                              ;; channel-of-entries-since produces
                              ;; them.  Failing that, it's perhaps
                              ;; best for this number to be a bit
                              ;; smaller than that idea, so that this
                              ;; task finds nothing new occasionally.
                              20
                              (lambda ()
                                (let ((datum (async-channel-try-get the-channel)))
                                  (if datum
                                      (put (format "PRIVMSG ~a :~a"
                                                   channel
                                                   (entry->string datum)))
                                    (vtprintf "Nothing new on planet emacs~%")))
                                )))))

                   (hash-table-put! denizens-by-channel
                                    channel
                                    fellows))))
              (case command-symbol
                ((PRIVMSG)
                 (let* ((tokens (split params))
                        (tokens (if (<= 2 (length tokens))
                                    (cons (first tokens)
                                          (cons (regexp-replace
                                                 #rx"^:"
                                                 (second tokens)
                                                 "")
                                                (cddr tokens)))
                                  tokens))
                        (destination (car tokens))
                        (dest-is-channel? (regexp-match #rx"^#" destination))
                        (source (car prefix)))
                   (when dest-is-channel?
                     (for-each
                      (lambda (task)
                        (when task
                          (task 'postpone)))
                      (list
                       (hash-table-get *jordanb-quote-tasks-by-channel* destination #f)
                       *planet-emacs-task*)))

                   (when dest-is-channel?
                     (let* ((times-by-nick (hash-table-get
                                            times-by-nick-by-channel
                                            destination
                                            (lambda ()
                                              (make-hash-table 'equal))))
                            (utterance (make-utterance
                                        (seconds->date (current-seconds))
                                        (string-join (cdr tokens)))))
                       (vprintf "putting key ~s, value ~s in times-by-nick~%"
                                source utterance)
                       (hash-table-put! times-by-nick source utterance)
                       (hash-table-put! times-by-nick-by-channel destination times-by-nick)))

                   (unless (*passive?*)

                     (vprintf "Tokens ~s; destination ~s source ~s~%"
                              tokens destination source)
                     (cond
                      ;; private message to us.
                      ((equal? (*my-nick*) destination)
                       (do-something-clever  (cdr tokens) source destination #t))

                      ;; someone said something to the whole channel.
                      (dest-is-channel?

                       ;; ... but prefixed it with my nick.
                       (when (regexp-match
                              (regexp
                               (string-append
                                "^"
                                (regexp-quote (*my-nick*))
                                "[:,]"))
                              (cadr tokens))
                         (do-something-clever  (cddr tokens) source destination #f))))))
                 )
                ((NOTICE)
                 (vprintf "Hmm, I notice ~s ~s ~s but have been told not to do anything clever~%"
                          prefix
                          command
                          params))
                ((PING)
                 (put (format "PONG ~a" params)))))))))))

(define denizens-by-channel (make-hash-table 'equal))
(define times-by-nick-by-channel (make-hash-table 'equal))
)