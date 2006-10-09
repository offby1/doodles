#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; Predict Score with Partial Knowledge
(module pspk mzscheme
(display "$Id$" (current-error-port))
(newline (current-error-port))

(define (ps/pk hands/pk history num-rands)
  (cond

   ;; very unlikely, but who knows?  Maybe one of the opponents went
   ;; to the bathroom and we got a peek.
   ((every fully-known? hands/pk)
    (dds:predict-score (->dds-style-hands hands/pk) history))

   ((zero? num-hands)
    (error "How the hell am I supposed to predict the rest of the game when I'm not allowed to guess what the opponents have?!"))

   (else
    ;; the real work
    (most-common (map (lambda (seed)
                        (dds:predict-score (generate-random-hands-conforming-to hands/pk seed ))
                        (iota num-rands))))
    )


   )
  )

)
