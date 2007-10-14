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
             (< i 1))

    (let ((p (! (car photo-stream))))
      (let ((id (car ((sxpath '(@ id *text*)) p))))
        (printf "Photo ~s: " id)
        (let ((exif (flickr.photos.getExif
                     'photo_id id)))
          (let ((model-name (car ((sxpath
                                   '(photo
                                     (exif (@ (equal? (label "Model"))))
                                     raw
                                     *text*))
                                  exif))))
            (printf "This photo was taken with a ~s: " model-name)
            (when (equal? "NIKON D200" model-name)
              (let ((lens-data (car ((sxpath '(photo
                                               (exif (@ (equal? (label "Lens Min/Max Focal Length, Min/Max Aperture"))))
                                               raw
                                               *text*))
                                     exif))))
                (printf "(lens data: ~s) " lens-data)))
            (pretty-print p)))))

    (loop
     (add1 i)
     (! (cdr photo-stream)))))

)
