#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui bot-tests 'verbose))"
|#
(module bot-tests mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "bot.ss"
         (only "globals.ss" *initial-channel-names*)
         "vprintf.ss")

(require/expose
 "bot.ss"
 (
  *periodicals-by-id*
  *task-custodian*
  for-each-periodical
  periodical-back-to-sleep
  periodical-channel-of-interest
  periodical-do-it-now!
  periodical-id
  periodical-thread
  ))

;; The first thing we do, let's kill all the periodicals.

(define (kill-all-periodicals!)
  (printf "About to shut down custodian what manages all these dudes: ~s~%"
          (custodian-managed-list *task-custodian* (current-custodian)))
  (custodian-shutdown-all *task-custodian*)
  (set! *task-custodian* (make-custodian))
  (set! *periodicals-by-id* (make-hash-table 'equal)))

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
   #:after
   (lambda ()
     (printf "~a periodicals:~%" (hash-table-count *periodicals-by-id*))
     (for-each-periodical
      (lambda (id p)
        (printf "periodical ~s: running: ~a; dead: ~a~%"
                (periodical-id p)
                (if (thread-running? (periodical-thread p))
                    "yes" " no")
                (if (thread-dead? (periodical-thread p))
                    "yes" " no"))))
     (printf "*task-custodian* manages all these dudes: ~s~%"
             (custodian-managed-list *task-custodian* (current-custodian)))
     )
   (test-case
    "join"
    (before
     kill-all-periodicals!
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
    (check-not-exn
     (lambda ()
       (respond
        ":fledermaus!n=vivek@pdpc/supporter/active/fledermaus PRIVMSG #emacs :rudybot: "
        (open-output-string)))))
     ))

(provide (all-defined))
)
