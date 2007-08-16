#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui channel-idle-event-tests 'verbose))"
|#
(module channel-events mzscheme
(require (lib "async-channel.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
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

(define (public-make-channel-idle-event channel-name interval)
  (let ((alarm (make-alarm-with-snooze
                interval
                #:id (format "signals when channel ~s has been idle for ~a seconds"
                             channel-name
                             interval)
                #:periodic? #f)))
    (make-channel-idle-event
     alarm
     (lambda (irc-message)
       (when (and (PRIVMSG? irc-message)
                  (equal? (PRIVMSG-destination irc-message)
                          channel-name))
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

(define (public-make-channel-request-event input-examiner)
  (let ((c (make-async-channel)))
    (make-channel-request-event
     c
     (lambda (message)
       (and (input-examiner message)
            (async-channel-put c message))
       ))))



(define channel-events-tests

  (test-suite
   "channel-events"
   (test-case
    "channel goes idle when we're not yammering at it"
    (let* ((channel-idle-event (public-make-channel-idle-event "#snooze" 1/10)))
      (check-not-false (sync/timeout 2/10 channel-idle-event))))

   (test-case
    "channel doesn't go idle when we are yammering at it"
    (let* ((cie (public-make-channel-idle-event "#snooze" 1/10))
           (make-yammerer
            (lambda (string)
              (thread (lambda ()
                        (let loop ((x 10))
                          (when (positive? x)
                            ((channel-idle-event-input-examiner cie)
                             (parse-irc-message string))
                            (sleep 1/20)
                            (loop (sub1 x))))
                        )))))
      (let ((relevant (make-yammerer ":x!x@z PRIVMSG #snooze :wakey wakey")))
        (check-false (sync/timeout 2/10 cie))
        (kill-thread relevant)
        (let ((irrelevant (make-yammerer ":x!x@z PRIVMSG #other-channel :wakey wakey")))
          (check-not-false (sync/timeout 2/10 cie))
          (kill-thread irrelevant))))
    )
   ))

(provide
 channel-events-tests

 channel-idle-event? channel-idle-event-input-examiner
 (rename public-make-channel-idle-event make-channel-idle-event)

 channel-request-event? channel-request-event-input-examiner
 (rename public-make-channel-request-event make-channel-request-event)))

