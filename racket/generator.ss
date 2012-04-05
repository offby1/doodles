#! /usr/bin/env mzscheme
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
|#

#lang scheme

(define (generator seq)
  (let* ((ch (make-channel))
         (writer
          (thread
           (lambda ()
             (let loop ((seq seq))
               (if (null? seq)
                   (set! ch #f)
                   (begin
                     (channel-put
                      ch
                      (car seq))
                     (loop (cdr seq)))))))))

    (lambda ()
      (and (channel? ch)
           (box (channel-get  ch))))))

(define g (generator (list 1 2 3)))
(printf "~a ~a ~a~%" (g)
        (g)
        (g))
(printf "One last time ... ")
(flush-output)
(g)
