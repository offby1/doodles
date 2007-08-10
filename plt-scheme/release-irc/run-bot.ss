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
          (only "planet-emacsen.ss" *planet-poll-interval*)
          "quotes.ss"
          "system.ss"
          "vprintf.ss"
          )

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
  (("-q" "--quote-and-headline-interval")
   secs "Seconds of channel silence required to emit a funny quote or a headline or whatever"
   (*quote-and-headline-interval* (string->number secs)))
  (("-p" "--password")
   pw "Password for NICKSERV"
   (*nickserv-password* pw))
  (("--planet") "Actually hit planet.emacsen.org, rather than using test data"
   (*use-real-atom-feed?* #t))
  (("-l" "--logfile") lfn "Name of file to log to.  Default is stdout"
   (*log-output-port* (open-output-file lfn 'truncate/replace)))
  (("-v" "--verbose")
    "Spew debug stuff to logfile"
    (verbose!))
  )

 (multi
  (("-c" "--channel") channel "A channel to join when starting"
   (*initial-channel-names* (cons channel (*initial-channel-names*))))))

(let ((remote-irc? (not (string=? "localhost" (*irc-server-name*))))
      (feed-description (if (*use-real-atom-feed?*) "real" "fake")))

  (fprintf
   (current-error-port)
   "irc server name: ~s; using ~a Atom feed~%"
   (*irc-server-name*)
   feed-description)
  ;; if we're talking to something other than localhost, we should
  ;; probably be hitting planet.emacsen for real
  (when (not (equal? (*use-real-atom-feed?*) remote-irc?))
    (fprintf (current-error-port)
             "WARNING: you're connecting to IRC server ~s but using a ~a Atom feed~%"
             (*irc-server-name*)
             feed-description)
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
    (vtprintf "Our version string is ~s~%" (*client-version*)))
  )

(print-hash-table #t)
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

(start)
)