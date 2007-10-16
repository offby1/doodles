#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0"
|#
(module update-nikkor-tags (lib "mz-without-promises.ss" "lazy")
(require
 (lib "cmdline.ss")
 (planet "xmlrpc.ss" ("schematics" "xmlrpc.plt" ))
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
  ;; "30-70mmf/2.8-3.5", e.g.
  (format "~a~ammf/~a~a"
          min-focal-length
          (if (< min-focal-length max-focal-length )
              (format "-~a" max-focal-length)
              "")

          (exact->inexact min-aperture)
          (if (< min-aperture max-aperture)
              (format "-~a" (exact->inexact max-aperture))
              "")))
(define *the-auth-frob*
  (car ((sxpath '(frob *text*))
        (parameterize ((*verbose* #t))
          (flickr.auth.getFrob)))))

(printf "Here dat frob, boss: ~s~%" *the-auth-frob*)

(define
  *the-token*
  (let again ()
    (with-handlers
        ([exn:xmlrpc:fault?
          (lambda (e)
            (define *login-url* (get-login-url *the-auth-frob* "write"))
            (printf "Handling ~s~%" e)
            (printf "Your web browser should open; tell it that it's OK to let this app mess with flickr!~%")
            (flush-output)
            (sleep 2)
            (send-url *login-url* #f)
            (sleep 10)
            (again))
          ])
      (car ((sxpath '(auth token *text*)) (flickr.auth.getToken
                                           'frob *the-auth-frob*))))))
(printf "Here dat token, boss: ~s~%" *the-token*)

(define *dry-run* (make-parameter #f))
(command-line
 "update-nikkor-tags"
 (current-command-line-arguments)
 (once-each

  (("-d" "--dry-run")
   "Just report what tags we'd add, instead of actually adding them"
   (*dry-run* #t))
  )
)

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

                    (let ((tag (apply lens-data->string parsed-exact-numbers)))
                      ;; I don't know why, but flickr.photos.addTags
                      ;; raises a -1 _when it succeeds_.  So I
                      ;; have to ignore that here.  (Actually it's
                      ;; ssax that's raising -1.  How annoying)
                      (with-handlers
                          ([integer?
                            (lambda (e)
                              (fprintf (current-error-port)
                                       "Ignoring integer ~s~%" e)
                              (flush-output (current-error-port))
                              )])
                        (when (not (*dry-run*))
                          (flickr.photos.addTags
                           'auth_token *the-token*
                           'photo_id id
                           'tags tag)))
                      (printf " => ~s" tag)))
                  )))
            (printf "~%")
            ))))

    (loop
     (add1 i)
     (! (cdr photo-stream)))))
)
