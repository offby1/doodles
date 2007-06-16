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
(define-struct sample (wall-ms
                       cpu-ms
                       bytes-used
                       unique-tries
                       loop-passes) #f)
(define *the-samples* '())

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

   (let loop ((last-time #f))
     (collect-garbage)
     (let ((this-sample (make-sample
                         (current-inexact-milliseconds)
                         (current-process-milliseconds)
                         (current-memory-use)
                         (*tries*)
                         (*loop-passes*))))
       (set! *the-samples* (cons this-sample *the-samples*))
       (let* ((this-cpu-ms     (sample-cpu-ms       this-sample))
              (current-tries   (sample-unique-tries this-sample))
              (this-wall-ms    (sample-wall-ms      this-sample))
              (this-bytes      (sample-bytes-used   this-sample))
              (remaining-tries (- max-tries current-tries)))
         (nl)
         (printf "Current memory use: ~a bytes; ~a tries (~a% done)"
                 (num-string-commas (round (my-round this-cpu-ms 2)))
                 (num-string-commas (my-round current-tries 2))
                 (exact->inexact (my-round (/ (* 100 current-tries) max-tries) 2)))

         (when (not (null? (cdr *the-samples*)))
           (let* ((last-sample (cadr *the-samples*))
                  (delta-cpu-ms  (max 1 (- this-cpu-ms   (sample-cpu-ms       last-sample))))
                  (delta-wall-ms (max 1 (- this-wall-ms  (sample-wall-ms      last-sample))))
                  (delta-tries   (max 1 (- current-tries (sample-unique-tries last-sample))))
                  (tries-per-cpu-second       (/  (* 1000 delta-tries) delta-cpu-ms ))
                  (tries-per-wallclock-second (/  (* 1000 delta-tries) delta-wall-ms))
                  (remaining-seconds  (/ remaining-tries tries-per-wallclock-second))
                  (delta-bytes   (max 1 (- this-bytes    (sample-bytes-used   last-sample)))))
             (nl)
             (printf "~a tries per CPU second; ~a per wallclock second; ~a bytes per try"
                     (num-string-commas (round (my-round tries-per-cpu-second 2)))
                     (num-string-commas (round (my-round tries-per-wallclock-second 2)))
                     (num-string-commas (round (my-round (/ delta-bytes delta-tries) 2))))

             (printf " ETA ")
             (if (< remaining-seconds (* 365 24 3600))
                 (let ((ETA (+ (/ this-wall-ms 1000) remaining-seconds)))
                   (printf "~a"
                           (date->string (seconds->date (inexact->exact (round ETA)))  #t)))
               (printf "a long, long time from now"))))
         (printf "~%")
         (unless last-time
           (loop
            (sync/timeout 30 *trigger-o-death*)))))))))