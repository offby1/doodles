#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui bot-tests 'verbose))"
|#
(module bot-tests mzscheme
(require (only (lib "pregexp.ss") pregexp-quote)
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "bot.ss"
         (only "globals.ss" *initial-channel-names*)
         (only "parse.ss"
               parse-irc-message)
         "vprintf.ss")

;; TODO -- write proper "check-response", so that it gives meaningful
;; spew when it fails
(define (got-response? input regexp)
  (let-values (((ip op) (make-pipe)))
    (respond (parse-irc-message input) op)
    (expect/timeout ip regexp 1)))

;; returns #f if we didn't find what we're looking for.

(define (expect/timeout ip regex seconds)
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
                   (vprintf "expect/timeout: nope; retrying~%")
                   (loop)))

                 ))))))
    (and (sync/timeout seconds ch)
         ch)))

(define bot-tests

  (test-suite
   "crap"
   (test-case
    "join"
    (parameterize ((*initial-channel-names* (list "#bots")))
      (let-values (((ip op) (make-pipe)))
        (respond
         (parse-irc-message ":server 001 :welcome")
         op)
        (check-not-false
         (expect/timeout ip #rx"JOIN #bots" 1)
         "didn't join"))))
   (test-case
    "short semi-private message"
    (check-not-false
     (got-response?
      ":fledermaus!n=vivek@pdpc/supporter/active/fledermaus PRIVMSG #emacs :rudybot: "
      (pregexp-quote "PRIVMSG #emacs :Eh?  Speak up."))
     ))
     ))

(provide (all-defined))
)
