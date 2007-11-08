#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui lazy-photo-stream-tests 'verbose))"
|#
(module lazy-photo-stream (lib "lazy.ss" "lazy")
(require (lib "kw.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
               sxpath)
         "get-one-batch.ss"
         "../flickr.ss")

(define/kw (get-all-photos first-page #:key
                           [per_page 3]
                           [auth_token #f])
  ;; note that this takes *everything*, to infinity (not beyond)
  (let ((b (get-one-batch #:page first-page
                          #:per_page per_page
                          #:auth_token auth_token)))
    (if (null? b)
        b
        (append b (get-all-photos (add1 first-page)
                                  #:per_page per_page)))))

(define/kw (photo-stream
            #:key [auth_token #f])
  (get-all-photos
   1
   #:per_page 10
   #:auth_token auth_token))

(provide photo-stream)


(define lazy-photo-stream-tests

  (test-suite
   "lazy-photo-stream"
   (test-case
    "yow"
    (check-regexp-match
     #rx"bar"
     "foo"))))


)
