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
(define *echo-server-lines* (make-parameter #f))
(let-values (((ip op)
              (tcp-connect "localhost" 6667)))

  (define callback
    (let ((state 'init))
      (lambda (line)
        (when (*echo-server-lines*) (printf "<= ~s~%" line))
        (let-values (((prefix command params)
                      (parse-message line)))
          (case state
            ((init)
             (put "NICK carter")
             (put "USER erich debian irc.freenode.org :Eric Hanchrow"))
            ((250)
             (put "WHOIS carter"))
            ((311 312 317)
             (printf "Woot -- I got a ~a response to my WHOIS! ~s~%"
                     state
                     params))
            (else
             (fprintf (current-error-port)
                      "Uh oh, I don't know what to do in state ~s.~%"
                      state ))
            )
          ;; transition to next state.
          (cond
           ((eq? state 'init)
            (set! state 'unknown))
           ((string=? command "NOTICE")
            ;; no change.
            )
           ((regexp-match (pregexp "^[[:digit:]]{3}$") command )
            (set! state (string->number command))))
          ))))

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

  (sync/timeout 5 reader)
  (display "OK, I'm bored.")
  (newline))

)