#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module monitor mzscheme
;; this seems overly complex.
(provide monitor once-more-and-then-quit)
(require (lib "round.scm" "offby1")
         (lib "date.ss")
         "globals.ss"
         "num-string-commas.ss")
(define (seconds->string s)
  (let* ((s (inexact->exact (round s)))
         (minutes (quotient s 60))
         (hour    (quotient minutes 60))
         (day     (quotient hour 24)))
    (format "~a days, ~a hours, ~a minutes, ~a seconds"
            day
            (remainder hour 24)
            (remainder minutes 60)
            (remainder s 60))))
(define *trigger-o-death* (make-semaphore 0))
(define (once-more-and-then-quit)
  (semaphore-post *trigger-o-death*))
(define (monitor max-tries)
  (parameterize
   ((current-output-port (current-error-port)))

   (let loop ((current-tries (*tries*))
              (previous-tries #f)
              (last-cpu #f)
              (last-wall #f)
              (last-time #f))
     (printf "Current memory use: ~a bytes~%" (num-string-commas (my-round (current-memory-use) 2)))

     (let ((now-cpu (current-process-milliseconds))
           (now-wall-ms (current-inexact-milliseconds))
           (remaining-tries (- max-tries current-tries)))
       (nl)
       (printf "~a tries" (num-string-commas (my-round current-tries 2)))

       (let ((tries-per-wallclock-second
              (and previous-tries
                   (/  (* 1000 (- current-tries previous-tries))
                       (max 1 (- now-wall-ms last-wall))))
              ))
         (when tries-per-wallclock-second
           (printf " (~a tries per CPU second;"
                   (num-string-commas
                    (round
                     (my-round
                      (/ (* 1000 (- current-tries previous-tries))
                         (max 1 (- now-cpu last-cpu)))
                      2))))

           (printf " ~a per wallclock second)"
                   (num-string-commas
                    (round (my-round tries-per-wallclock-second 2)))))

         (printf
          " (~a% done)"
          (exact->inexact (my-round (/ (* 100 current-tries) max-tries) 2)))

         (when (and (number? tries-per-wallclock-second)
                    (positive? tries-per-wallclock-second))
           (let ((remaining-seconds  (/ remaining-tries tries-per-wallclock-second)))
             (printf " ETA ")
             (if (< remaining-seconds (* 365 24 3600))
                 (let ((ETA (+ (/ now-wall-ms 1000) remaining-seconds)))
                   (printf "~a"
                           (date->string (seconds->date (inexact->exact (round ETA)))  #t)))
               (printf "a long, long time from now"))))
         )
       (printf "~%")
       (unless last-time
         (loop
          (*tries*)
          current-tries
          now-cpu
          now-wall-ms
          (sync/timeout 30 *trigger-o-death*)))
       )))))
