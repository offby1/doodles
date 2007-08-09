#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui direct-bot-command-evt-tests 'verbose))"
|#
(module direct-bot-command-evt mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only "globals.ss" *my-nick*))

;; this event is ready when someone in the named channel says the
;; named word "to" us (which means: they say our nick, a colon,
;; followed by the word)
(define direct-bot-command-evt values)


(define direct-bot-command-evt-tests

  (test-suite
   "direct-bot-command-evt"
   (test-case
    "yow"
    (let ((e (direct-bot-command-evt "#snorkly" "doit!")))
      ((direct-bot-command-evt-input-examiner e)
       (format ":x!y@z PRIVMSG #snorkly :~a: doit!" (*my-nick*)))
      (check-not-false (sync/timeout 1/1000 e))))))

(provide (all-defined))
)
