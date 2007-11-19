(module pb-test mzscheme
(require "progress-bar.ss"
         (lib "class.ss"))

(define *work-units* 100)

(define work-thread #f)
(define pb
  (new pb%
       (label "A pb")
       (work-to-do *work-units*)
       (cancel-callback (lambda (button event)
                          (kill-thread work-thread)
                          ))))

(set! work-thread
      (thread
       (lambda ()
         (let loop ((x 0))
           (when (< x *work-units*)
             (send pb advance!)
             (sleep 1/20)
             (loop (add1 x))))
         (send pb show #f))))

(send pb show #t)
)