#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#

(module lazy
mzscheme
;;(lib "lazy.ss" "lazy")
(require (lib "kw.ss")
         (lib "trace.ss")
         (only (lib "1.ss" "srfi") iota))

;; a quick stub that acts vaguely like flickr.photos.search

(define/kw (get-buncha-photos
            #:key
            [page 1]
            [per_page 3])
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
   (iota per_page (* (sub1 page) per_page)))  )
(define/kw (get-one-page #:key
                        [page 1]
                        [per_page 3])
  (fprintf (current-error-port)
           "Snarfing ~a..." per_page)
  (begin0

      (let* (
             ;; completely arbitrary
             (total 10)
             (num-on-last-page (remainder total per_page))
             (pages (+ (quotient total per_page)
                       (if (zero? num-on-last-page)
                           0
                           1))))

        ;; Yeah, this is ugly.  Do better.
        (when (zero? num-on-last-page)
          (set! num-on-last-page per_page))

        (cons '*TOP*
              (cons 'photos
                    (cons
                     `(@ (total ,(number->string total))
                         (perpage ,(number->string per_page))
                         (pages ,(number->string pages))
                         (page ,(number->string page)))
                     (get-buncha-photos #:page page #:per_page per_page)))))

    (fprintf (current-error-port)
             "done~%"))
    )
(provide get-one-page)

)
