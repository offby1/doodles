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
(let-values (((ip op)
              (tcp-connect "localhost" 6667)))

  (define callback
    (let ((state 'init))
      (lambda (line)
        (printf "<= ~s~%" line)
        (let-values (((prefix command params)
                      (parse-message line)))
          (case state
            ((init)
             (put "NICK carter")
             (put "USER erich debian irc.freenode.org :Eric Hanchrow"))
            ((ready-for-action)
             (put "WHOIS carter"))
            (else
             (fprintf (current-error-port)
                      "Uh oh, I don't know what to do in state ~s.~%"
                      state ))
            )
          ;; transition to next state.
          (cond
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