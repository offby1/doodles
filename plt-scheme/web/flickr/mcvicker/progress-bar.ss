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
           (callback
            (lambda (button event)
              (cancel-callback button event)
              (send this show #f)))))
    (define gauge
      (new gauge%
           (label #f)
           (range work-to-do)
           (parent this)))))

(define work-thread #f)
(define pb
  (new pb%
       (label "A pb")
       (work-to-do 100)
       (cancel-callback (lambda (button event)
                          (kill-thread work-thread)
                          ))))

(set! work-thread
      (thread
       (lambda ()
         (let loop ((x 0))
           (send pb advance!)
           (sleep 1/20)
           (loop (add1 x))))))

(send pb show #t)
)