#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; http://tools.ietf.org/html/rfc1459

(module irc mzscheme
(require (lib "async-channel.ss")
         (lib "trace.ss")
         "parse-message.ss")
(define *echo-server-lines* #f)
(let-values (((ip op)
              (tcp-connect "localhost" 6667)))

  (define callback
    (let ((state 'init))
      (lambda (line)
        (when *echo-server-lines* (printf "<= ~s~%" line))
        (let-values (((prefix command params)
                      (parse-message line)))
          (case state
            ((init)
             (put "NICK carter")
             (put "USER erich debian irc.freenode.org :Eric Hanchrow")
             (set! state 'something-other-than-init)))

          (let ((command-number (and (regexp-match (pregexp "^[[:digit:]]{3}$") command )
                                     (string->number command)))
                (command-string (and (regexp-match (pregexp "^[[:alpha:]]+$") command)
                                     (string->symbol command))))
            (case command-number
              ((250)
               (put "WHOIS carter"))
              ((311 312 317)
               (printf "Woot -- I got a ~a response to my WHOIS! ~s~%"
                       command-number
                       params)))
            (case command-string
             ((PRIVMSG)
              (printf "Ooh -- a message for someone -- ~s~%" params))))))))

  (define reader
    (thread
     (lambda ()
       (let loop ()
         (let ((line (read-line ip)))
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

  (set! *echo-server-lines* #t)
  (sync/timeout (* 5 60) reader)
  (display "OK, I'm bored.")
  (newline))

)