#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; http://tools.ietf.org/html/rfc1459

(module bot mzscheme
(require (lib "async-channel.ss")
         (lib "trace.ss")
         (only (lib "13.ss" "srfi") string-tokenize)
         (only (lib "14.ss" "srfi")
               char-set
               char-set-complement)
         "parse-message.ss")

(define *echo-server-lines* #f)
(define *my-nick* "fartbot")
(define *irc-server-name* "localhost" )
(define *initial-channel-name* "#fart")

(let-values (((ip op)
              (tcp-connect *irc-server-name* 6667)))

  (define callback
    (let ((state 'initial))
      (lambda (line)
        (when *echo-server-lines* (printf "<= ~s~%" line))
        (let-values (((prefix command params)
                      (parse-message line)))
          (case state
            ((initial)
             (put (format "NICK ~a" *my-nick*))
             (put (format "USER ~a ~a ~a :~a"
                          (getenv "USER")
                          "unknown-host"
                          *irc-server-name*
                          "Eric Hanchrow's bot, $Rev$"))
             (set! state 'something-other-than-init)))

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
                       params)))
            (case command-symbol
              ((PRIVMSG NOTICE)
               (printf "Ooh -- a message " )
               (let* ((tokens (string-tokenize params))
                      (destination (and prefix (car tokens)))
                      (source (and prefix (car (string-tokenize prefix (char-set-complement (char-set #\! #\@)))))))
                 (cond
                  ((equal? *my-nick* destination)
                   (printf "for me only"))
                  ((not destination)
                   (printf "for noone in particular"))
                  ((regexp-match #rx"^#" destination)
                   (printf "for the channel ~a" destination)
                   (when (string=? (cadr tokens) (string-append ":" *my-nick* ":"))
                     (printf " (hey, it's for me!)")
                     (do-something-clever (cddr tokens) source destination))
                   )
                  (else
                   (printf "for ... I dunno: ~s" destination))))
               (printf " ~s~%" params))
              ((PING)
               (put (format "PONG ~a" params)))))))))

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

  (define (do-something-clever tokens requestor channel-name)
    (put (format "PRIVMSG ~a :Well, ~a; I think ~a too.~%"
                 channel-name
                 requestor
                 tokens)))

  (set! *echo-server-lines* #t)
  (sync/timeout (* 5 60) reader)
  (display "OK, I'm bored.")
  (newline))

)