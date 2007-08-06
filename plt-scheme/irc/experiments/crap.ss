#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(test/text-ui crap-tests 'verbose)"
|#
(module crap mzscheme
(require (lib "trace.ss")
         (only (planet "port.ss" ("schematics" "port.plt" 1 0)) port->string)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "parse.ss")

(define (respond line ip op)
  ;; parse the line into an optional prefix, a command, and parameters.
  (let ((message (parse-irc-message line)))
    (case (message-command message)
      ((001)
       (fprintf
        op
        "JOIN #emacs")
       )
      ((353)
       (thread (lambda ()
                 (fprintf op "Apple sure sucks.~%")
                 (printf "waal, ah printed it~%")))
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
;;(trace respond)

(define (start)
  (let-values (((ip op)
                (tcp-connect "localhost" 6667)))

    ;; so we don't have to call flush-output all the time
    (file-stream-buffer-mode op 'line)

    (let loop ()
      (let ((line (read-line ip 'return-linefeed)))
        (if (eof-object? line)
            ;; TODO: maybe reconnect
            (printf "eof on server~%")
          (begin
            (respond line ip op)
            (loop)))))))

(define (get-response input)
  (let ((os (open-output-string)))
    (respond
     (cond
      ((string? input)
       input)
      ((procedure? input)
       (input))
      (else
       (error 'get-response)))
     (open-input-string "")
     os)
    (get-output-string os)))

(trace get-response)



(define-check (check-response input expected-output)
  (let ((actual-output (get-response input)))
    (when (not (string=? actual-output expected-output))
      (with-check-info*
       (list (make-check-actual actual-output)
             (make-check-expected expected-output))
       (lambda () (fail-check))))))

(define crap-tests

  (test-suite
   "crap"
   (test-case
    "join"
    (check-response ":server 001 :welcome" "JOIN #emacs" "didn't join")
    (check-response ":server 001 :welcome" "JOIN #emacs" "didn't join second time"))

   (test-case
    "starts threads"
    (let-values (((ip op) (make-pipe)))
      (let ((task
             (respond
              ":server 353 :howdy"
              (open-input-string "this particular task doesn't bother reading
so it doesn't matter what we put here.")
              op)))

        (sleep 1/10)
        (check-equal?
         (read-line ip)
         "Apple sure sucks."))))))

(provide (all-defined))
)
