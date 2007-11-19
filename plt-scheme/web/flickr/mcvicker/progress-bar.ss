(module progress-bar mzscheme
(require
 (lib "class.ss")
 (lib "mred.ss" "mred"))

(define pb%
  (class dialog%
    (init work-to-do cancel-callback)
    (public advance!)
    (super-new)

    (define vpane
      (new vertical-pane% (parent this)))

    (define (advance!)
      (send gauge set-value (add1 (send gauge get-value)))
      (send text set-label
            (format
             "~a/~a"
             (send gauge get-value)
             (send gauge get-range))))

    (define gauge
      (new gauge%
           (label "")
           (range work-to-do)
           (parent vpane)))

    (define text
      (new message%
           (label "Nuttin' yet")
           (parent vpane)))

    (define cancel-button
      (new button%
           (label "Cancel")
           (parent vpane)
           (callback
            (lambda (button event)
              (send this show #f)
              (cancel-callback button event)))))))

(provide pb%)

)