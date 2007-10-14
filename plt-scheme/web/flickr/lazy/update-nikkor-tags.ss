#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0"
|#
(module update-nikkor-tags (lib "mz-without-promises.ss" "lazy")
(require (only (lib "1.ss" "srfi") iota)
         (only (lib "pretty.ss")
               pretty-display
               pretty-print)
         (lib "kw.ss")
         (lib "trace.ss")
         (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
               sxpath)
         (only (lib "promise.ss" "lazy") force)
         "lazy.ss")

;; for each of my flickr photos
;;   if it was taken with my D200
;;     find the focal length and aperture
;;     find a tag that describes that lens
;;     add that tag to the picture

(define *my-NSID*
  "20825469@N00"
;;; (car ((sxpath '(user @ nsid *text*))
;;;                         (flickr.people.findByUsername
;;;                          'username "offby1")))
  )

(printf "Golly, here's all my photos:~%")
(let loop ((photos (snarf)))
  (when (not (null? photos))
    (for-each pretty-display (car (force photos)))
;;;     (loop (cdr photos))
    ))



)
