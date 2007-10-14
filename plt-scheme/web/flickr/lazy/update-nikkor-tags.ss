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
 (lib "sendurl.ss" "net")
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
(*verbose* #t)
(define *the-auth-frob* (car ((sxpath '(frob *text*)) (flickr.auth.getFrob))))

(printf "Here dat frob, boss: ~s~%" *the-auth-frob*)

;; It appears we only need to do this once -- that is, I did it once,
;; it worked fine, I got a token; then the second time I did this, the
;; web browser said I was already authenticated.  So it'd be nice to
;; not do this if I don't have to.
(define *login-url* (get-login-url *the-auth-frob* "write"))
(printf "Your web browser should open; tell it that it's OK to let this app mess with flickr!~%")
(flush-output)
(sleep 2)
(send-url *login-url* #f)
(sleep 10)

(define *the-token* (car ((sxpath '(auth token *text*)) (flickr.auth.getToken
                     'frob *the-auth-frob*))))
(printf "Here dat token, boss: ~s~%" *the-token*)
(exit 0)

(let loop ([i 0] [photo-stream (! photo-stream)])
  (when (and (not (null? photo-stream))
;;;              (< i 4)
             )

    (let ((p (! (car photo-stream))))
      (let ((id    (car ((sxpath '(@ id *text*)) p)))
            (title (car ((sxpath '(@ title *text*)) p))))
        (printf "~s (~a) :" title id) (flush-output)
        (let ((exif (flickr.photos.getExif
                     'photo_id id)))
          (let ((model-name ((sxpath
                              '(photo
                                (exif (@ (equal? (label "Model"))))
                                raw
                                *text*))
                             exif)))
            (when (and (not (null? model-name))
                       (equal? "NIKON D200" (car model-name)))
              (let ((lens-data
                     ((sxpath
                       '(photo
                         (exif
                          (@
                           (equal?
                            (label
                             "Lens Min/Max Focal Length, Min/Max Aperture"))))
                         raw
                         *text*))
                      exif))
                    )
                (when (not (null? lens-data))
                  (let* ((parsed-exact-numbers
                          (map string->number
                               (string-tokenize
                                (car lens-data)
                                (char-set-complement (char-set #\, #\newline))))))

                    ;; sometimes the four numbers come back as all 0
                    ;; ... no idea why
                    (when (not (member 0 parsed-exact-numbers))

                      (let ((tag (apply
                                  lens-data->string
                                  (map exact->inexact parsed-exact-numbers))))
                        (flickr.photos.addTags
                         'photo_id id
                         'tags tag)
                        (printf " => ~s" tag)))
                    ))))
            (printf "~%")
            ))))

    (loop
     (add1 i)
     (! (cdr photo-stream)))))
)
