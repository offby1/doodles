#!/bin/sh
#| -*- scheme -*-
exec mzscheme -qr "$0" "$@"
|#

;; A `fake' library that deals with the external resource
(module external mzscheme
  (provide get-stuff)
  (define (get-stuff)
    (printf "Enter a bunch of things: ")
    (flush-output)
    (parameterize ([current-input-port (open-input-string (read-line))])
      (let loop ([r '()])
        (let ([x (read)])
          (if (eof-object? x) (reverse! r) (loop (cons x r))))))))

(module external-stream (lib "lazy.ss" "lazy")
  (require external)
  (define (get-all-stuffs)
    ;; note that this takes *everything*, to infinity (not beyond)
    (append (get-stuff) (get-all-stuffs)))
  (provide stuff)
  (define stuff (get-all-stuffs)))

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
