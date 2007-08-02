#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module generator mzscheme
(define (generator seq)
  (let* ((ch (make-channel))
         (writer
          (thread
           (lambda ()
             (let loop ((seq seq))
               (when (null? seq)
                 (channel-put ch #f))
               (channel-put ch (list (car seq)))
               (loop (cdr seq)))))))
    (lambda ()
      (when (thread-dead? writer)
        (raise (make-exn:fail "generator exhausted" (current-continuation-marks))))
      (let ((datum (channel-get  ch)))
        (when (not datum)
          (kill-thread writer)
          (raise (make-exn:fail "generator exhausted" (current-continuation-marks))))
        (car datum)))))
)
