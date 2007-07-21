#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; http://tools.ietf.org/html/rfc1459

(module bot mzscheme
(require (lib "async-channel.ss")
         (lib "trace.ss")
         (only (lib "1.ss" "srfi") third)
         (only (lib "13.ss" "srfi")
               string-join
               string-tokenize
               )
         (only (lib "14.ss" "srfi")
               char-set
               char-set-complement)
         "parse-message.ss"
         "jordan.ss")

(define *echo-server-lines* #f)
(define *my-nick* "fartbot")
(define *irc-server-name*
  ;;"localhost"
  "irc.freenode.net"
  )
(define *initial-channel-name* "##cinema")

(define (strip-leading-colon str)
  (if (char=? #\: (string-ref str 0))
        (substring str 1)
      str))

(define (tokens->string tokens)
  (strip-leading-colon  (string-join tokens)))

(print-hash-table #t)

(define (parse-prefix str)
  (if (not str)
      '(#f #f #f)
    (let loop ((result (string-tokenize str (char-set-complement (char-set #\! #\@)))))
      (if (<= (length result ) 3)
          result
        (loop (cons #f result))))))

(let-values (((ip op)
              (tcp-connect *irc-server-name* 6667)))

  (define denizens-by-channel (make-hash-table 'equal))

  (define callback
    (let ((state 'initial))
      (lambda (line)
        (let-values (((prefix command params)
                      (parse-message line)))
          (let ((prefix (parse-prefix prefix)))
            (when *echo-server-lines*
              (printf "<= ~s -> prefix ~s; comand ~s params ~s ~%"
                      line
                      prefix
                      command
                      params))
            (case state
              ((initial)
               (put (format "NICK ~a" *my-nick*))
               (put (format "USER ~a ~a ~a :~a"
                            (getenv "USER")
                            "unknown-host"
                            *irc-server-name*
                            "Eric Hanchrow's bot, $Rev$"))
               (set! state 'waiting-for-login-confirmation)))

            (let ((command-number (and (regexp-match (pregexp "^[[:digit:]]{3}$") command )
                                       (string->number command)))
                  (command-symbol (and (regexp-match (pregexp "^[[:alpha:]]+$") command)
                                       (string->symbol command))))
              (case command-number
                ((001)
                 (set! state 'logged-in)
                 (printf "Ah, I see we logged in OK.~%")
                 (put (format "JOIN ~a" *initial-channel-name*)))
                ((311 312 317)
                 (printf "Woot -- I got a ~a response to my WHOIS! ~s~%"
                         command-number
                         params))
                ((353)
                 ;; response to "NAMES"
                 (let* ((tokens (string-tokenize params))
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
                 (printf "Ooh -- a message " )
                 (let* ((tokens (string-tokenize params))
                        (destination (car tokens))
                        (source (car prefix)))

                   (cond
                    ((equal? *my-nick* destination)
                     (printf "for me only")
                     (unless (eq? command-symbol 'NOTICE)
                       (do-something-clever (tokens->string (cdr tokens)) source destination #t)))
                    ((not destination)
                     (printf "for noone in particular"))
                    ((regexp-match #rx"^#" destination)
                     (printf "for the channel ~a" destination)
                     (when (string=? (cadr tokens) (string-append ":" *my-nick* ":"))
                       (printf " (hey, it's for me!)")
                       (unless (eq? command-symbol 'NOTICE)
                         (do-something-clever (tokens->string (cddr tokens)) source destination #f)))
                     )
                    (else
                     (printf "for ... I dunno: ~s" destination))))
                 (printf " ~s~%" params))
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
           message                      ;what they said
           requestor                    ;who said it
           channel-name                 ;where they said it
           was-private?                 ;how they said it
           )
    (printf "Message: ~s~%" message)
    (cond
       ((regexp-match #rx"(?i:^census)( .*$)?" message)
        (put (format "NAMES ~a" channel-name)))
       (else
        (let ((response-body
               (if (regexp-match #rx"(?i:^quote)( .*$)?" message)
                   (one-jordanb-quote)
                 (format "Well, ~a; I think ~a too."
                         requestor
                         message))))


          (put (format "PRIVMSG ~a :~a"
                       (if was-private? requestor channel-name)
                       response-body))))))

  (set! *echo-server-lines* #t)
  (sync reader)
  (display "OK, I'm bored.")
  (newline))

)