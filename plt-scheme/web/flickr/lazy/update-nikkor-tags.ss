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
 (lib "force.ss" "lazy")
 "lazy-photo-stream.ss")

;; for each of my flickr photos
;;   if it was taken with my D200
;;     find the focal length and aperture
;;     find a tag that describes that lens
;;     add that tag to the picture

(printf ">>> photo-stream = ~s\n" photo-stream) ; note: no reading when you get here
(printf ">>> forced photo-stream = ~s\n" (! photo-stream)) ; forces a read to get a cons

(let loop ([photo-stream (! photo-stream)])
  (when (not (null? photo-stream))
    (pretty-print (! (car photo-stream)))
    (loop  (! (cdr photo-stream)))))

)
