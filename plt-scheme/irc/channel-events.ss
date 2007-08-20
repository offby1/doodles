#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui channel-events-tests 'verbose))"
|#
(module channel-events mzscheme
(require (lib "kw.ss")
         (lib "async-channel.ss")
         (lib "trace.ss")
         (only (lib "1.ss" "srfi") any)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "assert.ss" ("offby1" "offby1.plt")) check-type)
         "alarm-with-snooze.ss"
         (only "globals.ss" register-version-string)
         "parse.ss")
(register-version-string "$Id$")

(define-values (struct:channel-idle-event make-channel-idle-event channel-idle-event? channel-idle-event-ref channel-idle-event-set!)
    (make-struct-type 'channel-idle-event #f 2 0
                      #f (list (cons prop:evt 0))
                      (make-inspector) #f '(0 1)))

;(trace make-channel-idle-event)
(define (channel-idle-event-input-examiner r)
  (channel-idle-event-ref r 1))

;(trace channel-idle-event-input-examiner)

(define (internal-make-channel-idle-event criterion interval)
  (let ((alarm (make-alarm-with-snooze
                interval
                #:periodic? #t)))
    (make-channel-idle-event
     alarm
     (lambda (irc-message)
       (when (criterion irc-message)

         ((alarm-with-snooze-snooze-button alarm)))
       #f                               ;so that the main loop doesn't
                                        ;think we've handled the
                                        ;current message
       ))))

(define-values (struct:channel-request-event make-channel-request-event channel-request-event? channel-request-event-ref channel-request-event-set!)
    (make-struct-type 'channel-request-event #f 2 0
                      #f (list (cons prop:evt 0))
                      (make-inspector) #f '(0 1)))

(define (channel-request-event-input-examiner c)
  (channel-request-event-ref c 1))

(define (internal-make-channel-request-event criterion)
  (let ((c (make-async-channel)))
    (make-channel-request-event
     c
     (lambda (message)
       (and (criterion message)
            (async-channel-put c message))
       ))))

(define/kw (make-channel-message-event criterion #:key [timeout #f])
  (check-type 'make-channel-message-event procedure? criterion)
  (when timeout
    (check-type 'make-channel-message-event real? timeout)
    (check-type 'make-channel-message-event positive? timeout))
  (if timeout
      (internal-make-channel-idle-event criterion timeout)
    (internal-make-channel-request-event criterion)))

(define (channel-message-event? thing)
  (or (channel-idle-event? thing)
      (channel-request-event? thing)))

(define (channel-message-event-input-examiner e)
  (check-type 'channel-message-event-input-examiner channel-message-event? e)
  (if (channel-idle-event? e)
      (channel-idle-event-input-examiner e)
    (channel-request-event-input-examiner e)))


(define (on-snooze? m)
  (and (PRIVMSG? m)
       (member "#snooze" (PRIVMSG-receivers m))))

(define channel-events-tests

  (test-suite
   "channel-events"
   (test-case
    "channel goes idle when we're not yammering at it"
    (let* ((e (make-channel-message-event on-snooze? #:timeout 1/10)))
      (check-not-false (sync/timeout 2/10 e))))

   (test-case
    "channel doesn't go idle when we are yammering at it"
    (let* ((e (make-channel-message-event on-snooze? #:timeout 1/10))
           (make-yammerer
            (lambda (string)
              (thread (lambda ()
                        (let loop ((x 10))
                          (when (positive? x)
                            ((channel-idle-event-input-examiner e)
                             (parse-irc-message string))
                            (sleep 1/20)
                            (loop (sub1 x))))
                        )))))
      (let ((relevant (make-yammerer ":x!x@z PRIVMSG #snooze :wakey wakey")))
        (check-false (sync/timeout 2/10 e))
        (kill-thread relevant)
        (let ((irrelevant (make-yammerer ":x!x@z PRIVMSG #other-channel :wakey wakey")))
          (check-not-false (sync/timeout 2/10 e))
          (kill-thread irrelevant)))))))

(provide
 channel-events-tests

 channel-message-event?
 channel-message-event-input-examiner
 make-channel-message-event
))

