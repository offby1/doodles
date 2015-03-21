#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

#lang scheme
;; this seems overly complex.
(provide monitor once-more-and-then-quit)
(require (planet offby1/offby1/round)
         (planet neil/numspell:1:0/numspell)
         (lib "date.ss")
         "globals.ss")
(define-struct sample (cpu-ms
                       bytes-used
                       unique-tries
                       loop-passes) #:transparent)
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

   (let loop ((last-time? #f))
     (collect-garbage)
     (let ((this-sample (make-sample
                         (current-process-milliseconds)
                         (current-memory-use *worker-custodian*)
                         (*tries*)
                         (*loop-passes*))))
       (set! *the-samples* (cons this-sample *the-samples*))
       (let* ((this-cpu-ms     (sample-cpu-ms       this-sample))
              (current-tries   (sample-unique-tries this-sample))

              (this-bytes      (sample-bytes-used   this-sample))
              (remaining-tries (- max-tries current-tries)))
         (nl)
         (printf "~a/~a tries/loops; ~a/~a bytes used (~a% done)"
                 (number->english (my-round current-tries 2))
                 (number->english (my-round (sample-loop-passes this-sample) 2))
                 (number->english (round (my-round this-bytes 2)))
                 (number->english *max-worker-mem*)

                 (exact->inexact (my-round (/ (* 100 this-bytes) *max-worker-mem*) 2))
                 )

         (when (not (null? (cdr *the-samples*)))
           (let* ((last-sample (cadr *the-samples*))
                  (delta-cpu-ms  (max 1 (- this-cpu-ms   (sample-cpu-ms       last-sample))))

                  (delta-tries   (max 1 (- current-tries (sample-unique-tries last-sample))))
                  (tries-per-cpu-second       (/  (* 1000 delta-tries) delta-cpu-ms ))


                  (delta-bytes   (max 1 (- this-bytes    (sample-bytes-used   last-sample)))))
             (nl)
             (printf "~a tries per CPU second; ~a bytes per try"
                     (number->english (round (my-round tries-per-cpu-second 2)))

                     (number->english (round (my-round (/ delta-bytes delta-tries) 2))))


             ))
         (printf "~%")
         (unless last-time?
           (loop
            (sync/timeout 3 *trigger-o-death*))))))))