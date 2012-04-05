(module progress-bar mzscheme
(require
 (lib "class.ss")
 (lib "mred.ss" "mred"))

(define pb%
  (class dialog%
    (init  work-to-do)
    (init-field worker-proc)
    (public start!)
    (public advance!)
    (public set-work-to-do!)
    (define th #f)
    (super-new)

    (define (start!)
      (set! th (thread (lambda () (worker-proc this))))
      (send this show #t))

    (define vpane
      (new vertical-pane% (parent this)))

    (define (advance!)
      (send gauge set-value (add1 (send gauge get-value)))
      (send text set-label
            (format
             "~a/~a"
             (send gauge get-value)
             (send gauge get-range))))

    (define (set-work-to-do! x)
      (send gauge set-range x))

    (define gauge
      (new gauge%
           (label "")
           (range work-to-do)
           (parent vpane)))

    (define text
      (new message%
           ;; this label has to be at least as big as whatever we'll
           ;; subsequently set it to, lest those subsequent values be
           ;; trunacted :-|
           (label "Nothing yet")
           (parent vpane)))

    (define cancel-button
      (new button%
           (label "Cancel")
           (parent vpane)
           (callback
            (lambda (button event)
              (kill-thread th)
              (send this show #f)))))))

(provide pb%)

)