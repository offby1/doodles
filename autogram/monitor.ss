#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module monitor mzscheme
;; this seems overly complex.
(provide monitor)
(require (lib "round.scm" "offby1")
         "globals.ss"
         "num-string-commas.ss")
(define (monitor max-tries)
  (parameterize
   ((current-output-port (current-error-port)))

   (printf "Will bail after ~a tries.~%"
           (num-string-commas max-tries))
   (let loop ((current-tries (*tries*))
              (previous-tries #f)
              (last-cpu #f)
              (last-wall #f))

     (let ((now-cpu (current-process-milliseconds))
           (now-wall (current-inexact-milliseconds))
           (remaining-tries (- max-tries current-tries)))
       (nl)
       (printf "~a tries" (num-string-commas (my-round current-tries 2)))

       (let ((tries-per-wallclock-second
              (and previous-tries
                   (/  (* 1000 (- current-tries previous-tries))
                       (max 1 (- now-wall last-wall))))
              ))
         (when tries-per-wallclock-second
           (printf " (~a tries per CPU second;"
                   (num-string-commas
                    (my-round
                     (/ (* 1000 (- current-tries previous-tries))
                        (max 1 (- now-cpu last-cpu)))
                     2)))

           (printf " ~a per wallclock second)"
                   (num-string-commas
                    (round (my-round tries-per-wallclock-second 2)))))

         (printf
          " (~a% done)"
          (exact->inexact (my-round (/ (* 100 current-tries) max-tries) 2)))

         (when tries-per-wallclock-second
           (printf " Estimated remaining time: ~a seconds"
                   (my-round (/ remaining-tries tries-per-wallclock-second) 2)))
         )
       (printf "~%")
       (sleep 5)
       (loop
        (*tries*)
        current-tries
        now-cpu
        now-wall))))))