#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module lazy-photo-stream (lib "lazy.ss" "lazy")
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
               sxpath)
         "get-one-batch.ss"
         "../flickr.ss")

(define (get-all-photos first-page per_page auth_token)
  (let ((b (get-one-batch first-page per_page auth_token)))
    (if (null? b)
        b
        (append
         b
         (get-all-photos
          (add1 first-page)
          per_page
          auth_token)))))

;;(trace get-all-photos)

(define (photo-stream auth_token)
  (get-all-photos
   1
   100
   auth_token))
;;(trace photo-stream)

(provide photo-stream))

