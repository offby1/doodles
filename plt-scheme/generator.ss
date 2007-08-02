#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module generator mzscheme
(define (generator seq)
  (let* ((ch (make-channel))
         (writer  (thread
                   (lambda ()
                     (let loop ((seq seq))
                       (if (null? seq)
                           (begin
                             (channel-put ch #f)
                             (loop '()))
                         (begin
                           (channel-put ch (list (car seq)))
                           (loop (cdr seq)))))))))
    (lambda ()
      (channel-get ch))))
)
