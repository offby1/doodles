#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module run-bot mzscheme
(require  (lib "cmdline.ss")
          (only (lib "etc.ss")
                this-expression-source-directory)
          "bot.ss"
          "globals.ss"
          "system.ss"
          (only "planet-emacsen.ss" planet-emacsen-input-port))
(planet-emacsen-input-port
 (open-input-file
  (build-path
   (this-expression-source-directory)
   "example-planet-emacsen.xml")))
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
  (("-j" "--jordan") secs "Seconds to wait before emitting a jordanb quote"
   (*jordanb-quote-interval* (string->number secs)))
  (("--planet") "Actually hit planet.emacsen.org, rather than using test data"
   (planet-emacsen-input-port #f))
  (("-v" "--verbose")
    "Spew I/O to stdout"
    (*verbose* #t))
  )

 (multi
  (("-c" "--channel") channel "A channel to join when starting"
   (*initial-channel-names* (cons channel (*initial-channel-names*))))))

(let ((local-irc?   (not (not (string=? "localhost" (*irc-server-name*)))))
      (local-atom?  (not (not (planet-emacsen-input-port)))))
  (fprintf
   (current-error-port)
   "irc server name: ~s; name of port for planet.emacsen.org: ~s~%"
   (*irc-server-name*)
   (object-name (planet-emacsen-input-port)))
  ;; if we're talking to something other than localhost, we should
  ;; probably be hitting planet.emacsen for real
  (when (not (equal? local-atom? local-irc?))
    (fprintf (current-error-port)
             "WARNING: you're connecting to IRC server ~a but using ~s for your planet.emacsen feed~%"
             (*irc-server-name*)
             (or (object-name (planet-emacsen-input-port))
                 '|the actual Atom feed|))
    (sleep 10))

  ;; if we're talking to a remote server, let's take some time to
  ;; identify as best we can.
  (let* ((svnversion (find-executable-path "svnversion"))
         (version-string
          (and
           svnversion
           (regexp-replace
            #rx"\n$"
            (system-args->string
             svnversion
             (path->string (this-expression-source-directory)))
            ""))))
    (when version-string
      (*client-version*
       version-string))
    (printf "Our version string is ~s~%" (*client-version*)))
  )

(thread
 (lambda ()
   (parameterize
       ((current-namespace
         (module->namespace "bot.ss")))
     (fprintf
      (current-error-port)
      "Welcome to the ~a namespace.  Use your power only for good.~%"
      (object-name (current-namespace)))
     (dynamic-require '(lib "rep.ss" "readline") #f)
     (read-eval-print-loop))))

(let-values (((ip op)
              (tcp-connect (*irc-server-name*) 6667)))

  (let loop ()
    (let ((line (read-line ip 'return-linefeed)))
      (if (eof-object? line)
          (printf "eof on server~%")
        (begin
          (callback line ip op)
          (loop))))))
)