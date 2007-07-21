#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; http://tools.ietf.org/html/rfc1459

(module bot mzscheme
(require (lib "cmdline.ss")
         (lib "async-channel.ss")
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
         "test-irc-server.ss")

(define test-mode? #f)
(define *timeout-seconds* #f)
(define *client-name* "Eric Hanchrow's bot")
(define *client-version* "$Rev$")

;; *sigh*.  The version string with which we reply to CTCP can't have
;; a colon, but of course Subversion's keyword expansion inserted a
;; colon into *client-version*, so we have to parse out the number.
(define *client-version-number* (second (string-tokenize *client-version*) ))

(define *client-environment*
  (format "PLT scheme version ~a on ~a"
          (version)
          (system-type 'os)))

(define *echo-server-lines* #f)
(define *irc-server-name*
  "localhost"
  ;;"irc.freenode.net"
  )
(define *initial-channel-names* '())

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

(command-line
 "bot"
 (current-command-line-arguments)
 (once-each

  (("-s" "--host") host "Name of the IRC server to connect to"
   (set! *irc-server-name* host))

  (("-t" "--timeout") timeout "Wait this many seconds before exiting; infinite by default"
   (set! *timeout-seconds* (string->number timeout)))
  (("--test-mode") "Don't connect to a real IRC server; instead, use a simple built-in stub"
   (set! test-mode? #t)))
 (multi
  (("-c" "--channel") channel "A channel to join when starting"
   (set! *initial-channel-names* (cons channel *initial-channel-names*)))
  )
 )

(let-values (((ip op)
              (if test-mode?
                  (test-irc-server)
                (tcp-connect *irc-server-name* 6667))))

  (define denizens-by-channel (make-hash-table 'equal))

  (define callback
    (let ((state 'initial))
      (lambda (line)
        (let-values (((prefix command params)
                      (parse-message line)))
          (let ((prefix (parse-prefix prefix)))
            (when *echo-server-lines*
              (printf "<= ~s -> prefix ~s; command ~s params ~s ~%"
                      line
                      prefix
                      command
                      params))
            (case state
              ((initial)
               (put (format "NICK ~a" *my-nick*))
               (put (format "USER ~a ~a ~a :~a, ~a"
                            (getenv "USER")
                            "unknown-host"
                            *irc-server-name*
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
                 (printf "Ah, I see we logged in OK.~%")
                 (for-each (lambda (ch)
                             (put (format "JOIN ~a" ch)))
                           *initial-channel-names*))
                ((311 312 317)
                 (printf "Woot -- I got a ~a response to my WHOIS! ~s~%"
                         command-number
                         params))
                ((353)
                 ;; response to "NAMES"
                 (let* ((tokens (split params))
                        (match-tokens (cdddr tokens))
                        (fellows (cons (strip-leading-colon (car match-tokens))
                                       (cdr match-tokens))))

                   ;; the first three tokens appear to be our nick, an
                   ;; =, then the channel name.  of the remaining
                   ;; tokens, the first begins with a colon, and any of
                   ;; them might also have a + or a @ in front.

                   (hash-table-put! denizens-by-channel
                                    (third tokens)
                                    fellows)
                   (printf "hmm, some names ... ~s => ~s~%" params denizens-by-channel))))
              (case command-symbol
                ((PRIVMSG NOTICE)
                 (let* ((tokens (split params))
                        (destination (car tokens))
                        (source (car prefix)))

                   (cond
                    ((equal? *my-nick* destination)
                     (unless (eq? command-symbol 'NOTICE)
                       (do-something-clever  (cdr tokens) source destination #t)))
                    ((regexp-match #rx"^#" destination)
                     (when (string=? (cadr tokens) (string-append ":" *my-nick* ":"))
                       (unless (eq? command-symbol 'NOTICE)
                         (do-something-clever  (cddr tokens) source destination #f)))))))
                ((PING)
                 (put (format "PONG ~a" params))))))))))

  (define reader
    (thread
     (lambda ()
       (let loop ()
         (let ((line (read-line ip 'return-linefeed)))
           (if (eof-object? line)
               (printf "eof on server~%")
             (begin
               (callback line)
               (loop))))))))

  ;; TODO -- gack if str is > 510 characters long
  (define (put str)
    (printf "=> ~s~%" str)
    (display str op)
    (newline op)
    (flush-output op))

  (define (do-something-clever
           message-tokens               ;what they said
           requestor                    ;who said it
           channel-name                 ;where they said it
           was-private?                 ;how they said it
           )

    (printf "message ~s requestor ~s channel-name ~s was-private? ~s~%"
            message-tokens requestor channel-name was-private?)
    (cond
     ((string=? ":\u001VERSION\u001" (first message-tokens))
      (put (format "NOTICE ~a :\001VERSION ~a (offby1@blarg.net):~a:~a\001"
                   requestor
                   *client-name*
                   *client-version-number*
                   *client-environment*)))
     ((regexp-match #rx"(?i:^census)( .*$)?" (first message-tokens))
      (put (format "NAMES ~a" channel-name)))
     (else
      (let ((response-body
             (if (regexp-match #rx"(?i:^quote)( .*$)?" (first message-tokens))
                 (one-jordanb-quote)
               (format "Well, ~a; I think ~a too."
                       requestor
                       (string-join message-tokens)))))

        (put (format "PRIVMSG ~a :~a"
                     (if was-private? requestor channel-name)
                     response-body))))))

  (set! *echo-server-lines* #t)
  (sync/timeout *timeout-seconds* reader)
  (display "OK, I'm bored.")
  (newline))

)