#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui bot-tests 'verbose))"
|#
(module bot-tests mzscheme
(require (only (lib "file.ss") put-preferences)
         (lib "kw.ss")
         (only (lib "pregexp.ss") pregexp-quote)
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "bot.ss"
         "globals.ss"
         "vprintf.ss")

;; TODO -- write proper "check-response", so that it gives meaningful
;; spew when it fails
(define/kw (got-response? input regexp #:key [timeout-seconds 1])
  (let-values (((ip op) (make-pipe)))
    (respond input op)
    (expect/timeout ip regexp 1)))

;; returns #f if we didn't find what we're looking for.

(define/kw (expect/timeout ip regex seconds)
  (let* ((ch (make-channel))
         (reader
          (thread
           (lambda ()
             (let loop ()
               (vprintf "expect/timeout about to look for ~s from ~s ...~%"
                        regex
                        (object-name ip))
               (let ((line (read-line ip)))
                 (cond
                  ((eof-object? line)
                   (vprintf "expect/timeout: eof~%")
                   (channel-put ch #f))
                  ((regexp-match regex line)
                   (vprintf "expect/timeout: Got match!~%")
                   (channel-put ch #t))
                  (else
                   (vprintf "expect/timeout: nope, got ~s; retrying~%")
                   (loop)))

                 ))))))
    (and (sync/timeout seconds ch)
         ch)))

(trace expect/timeout)

(define bot-tests

  (test-suite
   "crap"
   (test-case
    "join"
    (parameterize ((*initial-channel-names* (list "#bots")))
      (let-values (((ip op) (make-pipe)))
        (respond
         ":server 001 :welcome"
         op)

        (check-not-false
         (expect/timeout ip #rx"JOIN #bots" 1)
         "didn't join")))
    )
   (test-case
    "short semi-private message"
    (check-not-false
     (got-response?
      (format
       ":fledermaus!n=vivek@pdpc/supporter/active/fledermaus PRIVMSG #emacs :~a: "
       (*my-nick*))
      (pregexp-quote "PRIVMSG #emacs :Eh?  Speak up."))
     ))

   (test-case
    "multiple rapid commands"
    (before
     (begin
       (printf "Yup, calling 'verbose!'~%")
       (verbose!)

       (put-preferences (list (*atom-timestamp-preference-name*))
                        (list #f))

       ;; make sure there's no news (so he can't immediately respond to
       ;; the commands we're about to give).

       ;; set the news-spew-interval to infinite (so that he spews only
       ;; on demand)

       ;; start the news thread
       (respond
        ":a. 366 yo #scheme-bots :End of /NAMES list."
        (open-output-string)))

     ;; send a bunch of "news" commands.

     ;; put a bunch of items into the news async.

     ;; make sure he only responds once.
     (check-not-false
      (got-response?
       ":a!b@c PRIVMSG #scheme-bots :news"
       #rx"Managing several radio feeds with MusicPD and Icecast"
       #:timeout-seconds 2))
     (check-false
      (got-response?
       ":a!b@c PRIVMSG #scheme-bots :news"
       #rx"."))

     ;; paranoia
     (sleep 2)
     (check-false
      (got-response?
       ":a!b@c PRIVMSG #scheme-bots :news"
       #rx"."))))))

(provide (all-defined))
)
