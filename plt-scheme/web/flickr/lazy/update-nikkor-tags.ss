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
 (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
       sxpath)
 "../flickr.ss"
 "lazy-photo-stream.ss")

;; for each of my flickr photos
;;   if it was taken with my D200
;;     find the focal length and aperture
;;     find a tag that describes that lens
;;     add that tag to the picture

(let loop ([i 0] [photo-stream (! photo-stream)])
  (when (and (not (null? photo-stream))
             (< i 3))

    (let ((p (! (car photo-stream))))
      (let ((id (car ((sxpath '(@ id *text*)) p))))
        (printf "Photo ~s: " id)
        (let ((exif (flickr.photos.getExif
                     'photo_id id)))
          (pretty-print p)
          (pretty-print exif))))

    (loop
     (add1 i)
     (! (cdr photo-stream)))))

)
