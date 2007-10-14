#!/bin/sh
#| -*- scheme -*-
exec mzscheme -qr "$0" "$@"
|#

;; A `fake' library that deals with the external resource
(module external mzscheme
(provide get-one-batch)
(require (lib "kw.ss")
         (only (lib "1.ss" "srfi") iota))
(define/kw (get-one-batch  #:key
                           [page 1]
                           [per_page 3])
  (fprintf (current-error-port)
           "Getting at most ~a photos from page ~a~%"
           per_page page)
  (map
   (lambda (n)
     `(photo
       (@ (title "Yours Truly")
          (server "2305")
          (secret "c8c4e9bf53")
          (owner "20825469@N00")
          (ispublic "1")
          (isfriend "0")
          (isfamily "0")
          (id ,(number->string n))
          (farm "3"))))
   (iota per_page (* (sub1 page) per_page)))))

(module external-stream (lib "lazy.ss" "lazy")
(require external)
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
