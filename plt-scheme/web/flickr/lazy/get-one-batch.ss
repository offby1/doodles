#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module get-one-batch mzscheme
(require (lib "kw.ss")
         (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
               sxpath)
         "../flickr.ss")

(define *my-NSID*
  "20825469@N00"
;;; (car ((sxpath '(user @ nsid *text*))
;;;                         (flickr.people.findByUsername
;;;                          'username "offby1")))
  )

;; returns a simple list of photos, not an actual page.
(define/kw (get-one-batch  #:key
                           [page 1]
                           [per_page 3])
  (fprintf (current-error-port)
           "Getting at most ~a photos from page ~a~%"
           per_page page)
  (flush-output (current-error-port))

  ((sxpath '(photos (photo)))
   (flickr.photos.search
    'user_id  *my-NSID*
    'page      page
    'per_page per_page) ))



(provide get-one-batch)
)
