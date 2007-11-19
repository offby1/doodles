(module pb-test mzscheme
(require "progress-bar.ss"
         (lib "class.ss"))

(define *work-units* 10)
(define *interval-seconds* 1/2)

(define work-thread #f)
(define pb
  (new pb%
       (label "Progress!")
       (work-to-do 1)
       (cancel-callback (lambda (button event)
                          (kill-thread work-thread)))))

(set! work-thread
      (thread
       (lambda ()
         (sleep *interval-seconds*)
         (send pb set-work-to-do! *work-units*)
         (sleep *interval-seconds*)
         (let loop ((x 0))
           (when (< x *work-units*)
             (send pb advance!)
             (sleep *interval-seconds*)
             (loop (add1 x))))
         (send pb show #f))))

(send pb show #t)
)