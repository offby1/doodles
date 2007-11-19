(module progress-bar mzscheme
(require
 (lib "class.ss")
 (lib "mred.ss" "mred"))

(file-stream-buffer-mode (current-error-port) 'line)

(define pb%
  (class dialog%
    (init work-to-do cancel-callback)
    (public advance!)
    (super-new)

    (define (advance!)
      (send gauge set-value (add1 (send gauge get-value))))

    (define cancel-button
      (new button%
           (label "Cancel")
           (parent this)
           (callback cancel-callback)))
    (define gauge
      (new gauge%
           (label #f)
           (range work-to-do)
           (parent this)))))

(letrec ((work-thread
          (thread
           (lambda ()
             (sleep 1) ;; BUGBUG -- wait until the pb is ready
             (let loop ((x 0))
               (send pb advance!)
               (sleep 1/20)
               (loop (add1 x))))))
         (pb
          (new pb%
               (label "A pb")
               (work-to-do 100)
               (cancel-callback (lambda (button event)
                                  (kill-thread work-thread)
                                  (send pb show #f))))))

  (send pb show #t))

)