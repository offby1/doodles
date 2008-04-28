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
               (channel-put
                ch
                (and (pair? seq) (list (car seq))))
               (when (pair? seq)
                 (loop (cdr seq))))))))
    (lambda ()
      (let ((datum (channel-get  ch)))
        (when (not datum)
          (raise (make-exn:fail "generator exhausted" (current-continuation-marks))))
        (car datum)))))

(define g (generator (list 1 2 3)))
(printf "~a ~a ~a~%" (g)
        (g)
        (g))
(printf "One last time ... ")
(flush-output)
(g)
)
