#!/bin/sh
#| -*- scheme -*-
exec mzscheme -qr "$0" "$@"
|#

;; client code for the above

(require (lib "force.ss" "lazy")
         "lazy-photo-stream.ss")

(printf ">>> photo-stream = ~s\n" photo-stream) ; note: no reading when you get here
(printf ">>> forced photo-stream = ~s\n" (! photo-stream)) ; forces a read to get a cons
;; show the first 10 items -- try this and see that you don't have to
;; type in an 11th item to get it to stop
(let loop ([i 0] [photo-stream (! photo-stream)])
  (printf "photo-stream[~s] = ~s\n" i (! (car photo-stream)))
  (when (< i 9) (loop (add1 i) (! (cdr photo-stream)))))
