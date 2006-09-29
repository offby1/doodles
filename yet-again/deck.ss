#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qr "$0" ${1+"$@"}
|#

(require (lib "errortrace.ss" "errortrace"))

(profiling-enabled #t)
(profiling-record-enabled #t)
;(profile-paths-enabled #t)

(require "card.ss"
         "bridge.ss"
         "history.ss"
         (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi") iota take))
(define *ranks* 5)                      ;should of course be 13, but
                                        ;... *sigh* ... that's too
                                        ;slow
(define *deck*
  (let loop ((suits *suits*)
             (result '()))
    (if (null? suits)
        (sort result (lambda (a b)
                       (< (card-rank a)
                          (card-rank b))))
      (loop (cdr suits)
            (append
             (let loop ((ranks (iota *ranks* 2))
                        (result '()))
               (if (null? ranks)
                   result
                 (loop (cdr ranks)
                       (cons (make-card (car suits)
                                   (car ranks))
                             result))))
             result)))))

(define east (take *deck* *ranks*))
(set! *deck*  (list-tail *deck* *ranks*))

(define south (take *deck* *ranks*))
(set! *deck* (list-tail *deck* *ranks*))

(define west (take *deck* *ranks*))
(set! *deck* (list-tail *deck* *ranks*))

(define north (take *deck* *ranks*))

(printf "North plays ~s~%"
        (choose-card (make-history (list))
                     (list north east south west)
                     #t))
(output-profile-results #t #t)
