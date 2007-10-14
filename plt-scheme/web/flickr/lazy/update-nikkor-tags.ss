#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0"
|#
(module update-nikkor-tags (lib "mz-without-promises.ss" "lazy")
(require
 (only (lib "pretty.ss")
       pretty-display
       pretty-print)
 (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
       sxpath)
 (lib "force.ss" "lazy")
 "lazy-photo-stream.ss")

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


(printf ">>> photo-stream = ~s\n" photo-stream) ; note: no reading when you get here
(printf ">>> forced photo-stream = ~s\n" (! photo-stream)) ; forces a read to get a cons
;; show the first 10 items -- try this and see that you don't have to
;; type in an 11th item to get it to stop
(let loop ([i 0] [photo-stream (! photo-stream)])
  (printf "photo-stream[~s] = ~s\n" i (! (car photo-stream)))
  (when (< i 9) (loop (add1 i) (! (cdr photo-stream)))))



)
