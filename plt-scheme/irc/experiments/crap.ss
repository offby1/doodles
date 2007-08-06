#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(test/text-ui crap-tests 'verbose)"
|#
(module crap mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define-struct message (prefix command params))

(define (parse-irc-message string)
  (make-message "some bogus prefix"
                1
                (list "some bogus parameters")))

(define *server-output-port* (make-parameter (current-output-port)))
(define (respond line ip)
  ;; parse the line into an optional prefix, a command, and parameters.
  (let ((message (parse-irc-message line)))
    (case (message-command message)
      ((001)
       (fprintf
        (*server-output-port*)
        "JOIN #emacs")
       )
      ((353)
       #t ;; start tasks for this channel
       )

      ((433)
       #t ;; gaah! "Nick already in use!"
       )
      ((NOTICE)
       #t ;; if it's a whine about identd, warn that it's gonna be slow.
       )
      ((PING)
       #t ;; send a PONG
       )
      ((PRIVMSG)
       #t ;; respond cleverly
       )
      (else
       (printf "Well, how would _you_ respond to ~s?~%" line))))
  )

(define (start)
  (let-values (((ip op)
                (tcp-connect "localhost" 6667)))

    (*server-output-port* op)

    ;; so we don't have to call flush-output all the time
    (file-stream-buffer-mode (*server-output-port*) 'line)

    (let loop ()
      (let ((line (read-line ip 'return-linefeed)))
        (if (eof-object? line)
            ;; TODO: maybe reconnect
            (printf "eof on server~%")
          (begin
            (respond line ip)
            (loop)))))))

(define-check (check-response input expected-output)
  (let ((os (open-output-string)))
    (parameterize ((*server-output-port* os))
      (respond input (open-input-string ""))
      (let ((actual-output (get-output-string os)))
        (when (not (string=? actual-output expected-output))
          (with-check-info*
           (list (make-check-actual actual-output)
                 (make-check-expected expected-output))
           (lambda () (fail-check))))))))

(define crap-tests

  (test-suite
   "crap"
   (test-case
    "join"
    (check-response ":server 001 :welcome" "JOIN #emacs" "didn't join")
    (check-response ":server 001 :welcome" "JOIN #emacs" "didn't join second time"))

   (test-case
    "starts threads"
    (respond
     ":server 353 :howdy"
     (open-input-string ""))
    (fail "where should the thread go?")
    )
   ))

(provide (all-defined))
)
