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
 (only (lib "13.ss" "srfi")
       string-tokenize
       )
 (only (lib "14.ss" "srfi")
       char-set
       char-set-complement
       )
 "../flickr.ss"
 "lazy-photo-stream.ss")

;; for each of my flickr photos
;;   if it was taken with my D200
;;     find the focal length and aperture
;;     find a tag that describes that lens
;;     add that tag to the picture

(define (lens-data->string min-focal-length max-focal-length
                           min-aperture max-aperture)
  ;; "30-70mm f/2.8-3.5", e.g.
  (format "~a~amm f/~a~a"
          min-focal-length
          (if (< min-focal-length max-focal-length )
              (format "-~a" max-focal-length)
              "")

          min-aperture
          (if (< min-aperture max-aperture)
              (format "-~a" max-aperture)
              "")))

(let loop ([i 0] [photo-stream (! photo-stream)])
  (when (and (not (null? photo-stream))
             (< i 1))

    (let ((p (! (car photo-stream))))
      (let ((id (car ((sxpath '(@ id *text*)) p))))
        (printf "Photo ~s: " (car ((sxpath '(@ title *text*)) p)))
        (let ((exif (flickr.photos.getExif
                     'photo_id id)))
          (let ((model-name (car ((sxpath
                                   '(photo
                                     (exif (@ (equal? (label "Model"))))
                                     raw
                                     *text*))
                                  exif))))
            (when (equal? "NIKON D200" model-name)
              (let* ((lens-data
                      (car
                       ((sxpath
                         '(photo
                           (exif
                            (@
                             (equal?
                              (label
                               "Lens Min/Max Focal Length, Min/Max Aperture"))))
                           raw
                           *text*))
                        exif)))
                     (parsed
                      (string-tokenize
                       lens-data
                       (char-set-complement (char-set #\, #\newline)))))
                (let ((tag (apply
                            lens-data->string
                            (map exact->inexact (map string->number parsed)))))
;;;                   (flickr.photos.addTags
;;;                    'photo_id id
;;;                    'tags tag)
                  (printf "(pretend I added the tag ~s)~%" tag)
                  )))
            ))))

    (loop
     (add1 i)
     (! (cdr photo-stream)))))
)
