#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mred --no-init-file --mute-banner --version --require "$0"
|#

;; from  "Gregory Cooper" <greg@cs.brown.edu>

(module frtime (lib "frtime.ss" "frtime")
(require (lib "simple.ss" "frtime" "demos" "gui"))

(define frame (new ft-frame% [shown #t]))

(define check-box (new ft-check-box% [parent frame] [label "Enable the button?"]))
(define enabled? (send check-box get-value-b))

(define button (new ft-button% [parent frame] [label "Click me!"]
                    [enabled enabled?]))
(define clicks (send button get-value-e))

(define message (new ft-message% [parent frame]
                     [label (format "~a clicks so far"
                                    (accum-b (map-e (lambda (_) add1)
                                                    clicks) 0))]))
)