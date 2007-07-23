#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; http://tools.ietf.org/html/rfc1459

(module bot mzscheme
(require (lib "async-channel.ss")
         (lib "trace.ss")
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
         "parse-message.ss"
         "globals.ss"
         "jordan.ss"
         "../web/quote-of-the-day.ss")
(provide callback)

(define (split str)
  (string-tokenize str (char-set-complement (char-set #\space))))

(define (strip-leading-colon str)
  (if (char=? #\: (string-ref str 0))
        (substring str 1)
      str))

(print-hash-table #t)

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

;; TODO -- see if I can parameterize current-random-number-generator
;; instead; that'd be more reliable.  I'm not doing that now, though,
;; because it doesn't look like I can supply any arbitrary function
;; (i.e., one that always returns zero); instead it looks as if I must
;; supply a vector of integers from which PLT scheme constructs a
;; pseudo-random-number generator.

(define (rnd . args)
  (if (not (*random?*))
      0
    (apply random args)))

(define (maybe thunk)
  (if (zero? (rnd 10))
      (thunk)))

(define (random-choice seq)
  (list-ref seq (rnd (length seq))))

;; gaah.
(define (regexp-quote str)
  (regexp-replace* #rx"." str "\\\\&"))

(define (vprintf . args)
  (when (*verbose*)
    (apply printf args)))

;; Calls THUNK every SECONDS seconds.  Calling the return value with
;; the symbol CANCEL cancels the next call, and resets the timer.
;; Calling the return value with any other value kills the thread
;; permanently.
(define (do-in-loop-when-not-cancelled seconds thunk)
  (let* ((s (make-semaphore)            ;1 means "cancel"
            )
         (t (thread (lambda ()
                      (let loop ()
                        (let ((cancelled? (sync/timeout seconds s)))
                          (when (not cancelled?)
                            (thunk)))
                        (loop))))))
    (lambda (command)
      (case command
        ((cancel) (semaphore-post s))
        (else (kill-thread t))))))
(define *cancel-pending-silly-utterance* #f)

(define callback
  (let ((state 'initial))
    (lambda (line ip op)
      ;; TODO -- gack if str is > 510 characters long
      (define (put str)
        (vprintf "=> ~s~%" str)
        (display str op)
        (newline op)
        (flush-output op))

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
         ((regexp-match #rx"(?i:^census)( .*$)?" (first message-tokens))
          (put (format "NAMES ~a" channel-name)))
         ((and *cancel-pending-silly-utterance*
               (string-ci=? "shaddap" (first message-tokens)))
          (*cancel-pending-silly-utterance* 'kill))
         (else
          (let ((response-body
                 (if (regexp-match #rx"(?i:^quote)( .*$)?" (first message-tokens))
                     (let ((r  (rnd 100)))

                       ;; we special-case zero for ease of testing.
                       (cond ((zero? r)
                              "I've laid out in my will that my heirs should continue working on my .emacs -- johnw")
                             ((even? r)
                              (let ((p (random-choice (quotes-of-the-day))))
                                (string-append (car p)
                                               "  --"
                                               (cdr p))))
                             (else
                              (one-jordanb-quote)))
                       )
                   (format "Well, ~a, I think ~a too."
                           requestor
                           (string-join message-tokens)))))

            (put (format "PRIVMSG ~a :~a"
                         (if was-private? requestor channel-name)
                         response-body))))))

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
                      (match-tokens (cdddr tokens))
                      (fellows (cons (strip-leading-colon (car match-tokens))
                                     (cdr match-tokens)))
                      (channel (third tokens)))

                 (when (and (string=? channel "#emacs")
                            (not *cancel-pending-silly-utterance*))
                   (set!
                    *cancel-pending-silly-utterance*
                    (do-in-loop-when-not-cancelled
                     (* 20 60)
                     (lambda ()
                       (put (format "PRIVMSG ~a :~a"
                                    channel
                                    (one-jordanb-quote)))))))

                 ;; the first three tokens appear to be our nick, an
                 ;; =, then the channel name.  of the remaining
                 ;; tokens, the first begins with a colon, and any of
                 ;; them might also have a + or a @ in front.

                 (hash-table-put! denizens-by-channel
                                  channel
                                  fellows))))
            (case command-symbol
              ((PRIVMSG)
               (when *cancel-pending-silly-utterance*
                 (*cancel-pending-silly-utterance* 'cancel))
               (unless (*passive?*)
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
                        (source (car prefix)))

                   (vprintf "Tokens ~s; destination ~s source ~s~%"
                             tokens destination source)
                   (cond
                    ;; private message to us.
                    ((equal? (*my-nick*) destination)
                     (do-something-clever  (cdr tokens) source destination #t))

                    ;; someone typed "/me".
                    ((string=? "\u0001ACTION" (second tokens))
                     (maybe
                      (lambda ()
                        ;; mimic the action, unless it came from a bot
                        ;; (some bots are just like us, and mimic
                        ;; actions -- if we are responding to one such,
                        ;; we'll get into an infinite loop!)
                        (vprintf "Destination is ~s~%" destination)
                        (if (regexp-match (pregexp "bot[^[:space:][:alnum:]]*$") source)
                            (put (format "PRIVMSG ~a :Imagine I copied ~a by saying \"/me ~a\""
                                         destination
                                         source
                                         (regexp-replace*
                                          #rx"\u0001+"
                                          (string-join (cddr tokens))
                                          "")))
                          (put (format "PRIVMSG ~a :\u0001ACTION copies ~a and ~a\u0001"
                                       destination
                                       source
                                       (regexp-replace*
                                        #rx"\u0001+"
                                        (string-join (cddr tokens))
                                        "")))))))

                    ;; someone said something to the whole channel.
                    ((regexp-match #rx"^#" destination)
                     (when (regexp-match
                            (regexp
                             (string-append
                              "^"
                              (regexp-quote (*my-nick*))
                              "[:,]"))
                            (cadr tokens))
                       (do-something-clever  (cddr tokens) source destination #f)))))))
              ((NOTICE)
               (vprintf "Hmm, I notice ~s ~s ~s but have been told not to do anything clever~%"
                         prefix
                         command
                         params))
              ((PING)
               (put (format "PONG ~a" params))))))))))

(define denizens-by-channel (make-hash-table 'equal))

)