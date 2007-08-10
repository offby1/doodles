#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui direct-bot-command-evt-tests 'verbose))"
|#
(module direct-bot-command-evt mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (lib "13.ss" "srfi")
               string-tokenize
               )
         (only (lib "14.ss" "srfi")
               char-set
               char-set-complement
               char-set:whitespace
               )
         (only "globals.ss"
               *my-nick*
               verbose!)
         "parse.ss"
         "vprintf.ss")

(define-values (struct:direct-bot-command-evt
                make-direct-bot-command-evt
                direct-bot-command-evt?
                direct-bot-command-evt-ref
                direct-bot-command-evt-set!)
    (make-struct-type 'direct-bot-command-evt #f 2 0
                      #f (list (cons prop:evt 0))
                      (make-inspector) #f '(0 1)))

;; this event is ready when someone in the named channel says the
;; named word "to" us (which means: they say our nick, a colon,
;; followed by the word)
(define (public-make-direct-bot-command-evt channel-name magic-phrase)
  (let ((s (make-semaphore))
        (sought-tokens (string-tokenize magic-phrase (char-set-complement char-set:whitespace))))
    (make-direct-bot-command-evt
     s
     (lambda (irc-message)
       (vprintf "~s~%" irc-message)
       (when (and (PRIVMSG? irc-message)
                  (equal? (PRIVMSG-destination irc-message)
                          channel-name)
                  (< 1 (length (PRIVMSG-text-words irc-message)))
                  (equal? sought-tokens (cdr (PRIVMSG-text-words irc-message))))

         (semaphore-post s))))))

(define (direct-bot-command-evt-input-examiner e)
  (direct-bot-command-evt-ref e 1))



(define direct-bot-command-evt-tests

  (test-suite
   "direct-bot-command-evt"
   (test-case
    "yow"
    (verbose!)
    (let ((e (public-make-direct-bot-command-evt "#snorkly" "doit!")))

      (define (check-pair ch w should-trigger?)
        ((direct-bot-command-evt-input-examiner e)
         (parse-irc-message (format ":x!y@z PRIVMSG ~a :~a: ~a"
                                    ch
                                    (*my-nick*)
                                    w)))
        (if should-trigger?
            (check-not-false (sync/timeout 1/1000 e))
          (check-false (sync/timeout 1/100 e))))

      (check-pair "#snorkly" "doit!"    #t)
      (check-pair "#stupid"  "doit!"    #f)
      (check-pair "#stupid"  "get bent" #f)
      (check-pair "#snorkly" "get bent" #f)
      ))))

(provide
 direct-bot-command-evt-tests
 direct-bot-command-evt-input-examiner
 (rename
  public-make-direct-bot-command-evt make-direct-bot-command-evt
  ))
)
