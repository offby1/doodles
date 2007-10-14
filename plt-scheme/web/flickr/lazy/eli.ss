#!/bin/sh
#| -*- scheme -*-
exec mzscheme -qr "$0" "$@"
|#

(module external-stream (lib "lazy.ss" "lazy")
(require "get-one-batch.ss")
(define (get-all-photos first-page)
  ;; note that this takes *everything*, to infinity (not beyond)
  (append (get-one-batch #:page first-page) (get-all-photos (add1 first-page))))
(provide stuff)
(define stuff (get-all-photos 1)))

;; client code for the above

(require (lib "force.ss" "lazy"))

(require external-stream)
(printf ">>> stuff = ~s\n" stuff) ; note: no reading when you get here
(printf ">>> forced stuff = ~s\n" (! stuff)) ; forces a read to get a cons
;; show the first 10 items -- try this and see that you don't have to
;; type in an 11th item to get it to stop
(let loop ([i 0] [stuff (! stuff)])
  (printf "stuff[~s] = ~s\n" i (! (car stuff)))
  (when (< i 9) (loop (add1 i) (! (cdr stuff)))))
