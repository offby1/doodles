#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module run-bot mzscheme
(require  (lib "cmdline.ss")
          "bot.ss"
          "globals.ss"
          "a-stub-IRC-server.ss")
(command-line
 "bot"
 (current-command-line-arguments)
 (once-each

  (("-s" "--host") host "Name of the IRC server to connect to"
   (*irc-server-name* host))

  (("-t" "--timeout") timeout "Wait this many seconds before exiting; infinite by default"
   (*timeout-seconds* (string->number timeout)))
  (("--passive") "Never say anything more than necessary -- in effect just log traffic to stdout"
   (*passive?* #t))
  (("-n" "--nick") nick "The nick I will be known by"
   (*my-nick* nick))
  )

 (multi
  (("-c" "--channel") channel "A channel to join when starting"
   (*initial-channel-names* (cons channel (*initial-channel-names*))))))

(let-values (((ip op)
              (if (*test-mode?*)
                  (stub-irc-server)
                (tcp-connect (*irc-server-name*) 6667))))

  (let loop ()
    (let ((line (read-line ip 'return-linefeed)))
      (if (eof-object? line)
          (printf "eof on server~%")
        (begin
          (callback line ip op)
          (loop))))))
)