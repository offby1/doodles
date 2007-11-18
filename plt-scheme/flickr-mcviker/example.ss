#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mred -M errortrace --no-init-file --mute-banner --version --require "$0"
|#
(module example mzscheme
(require (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1))
         (lib "mred.ss" "mred")
         (lib "class.ss")
         (lib "match.ss")
         (lib "file.ss")
         (lib "external.ss" "browser")
         (lib "etc.ss")
         "auth.ss"
         "yow.ss")

;; Make a frame by instantiating the frame% class
(define frame (instantiate frame% ()
                           (label "Flickr Thingy")
                           (width 1000)))
(send frame create-status-line)

(let* ((mb (instantiate menu-bar% (frame)))
       (menu (instantiate menu% ("&File" mb))))
  (instantiate menu-item%
               ("&Open..."
                menu
                (lambda (item event)
                  (let ((files (get-file-list
                                "Pick some files to mess with"
                                frame
                                (this-expression-source-directory)
                                #f
                                "*.csv"
                                '()
                                '(("CSV" "*.csv")))))

                    (send frame set-status-text "Authenticating ...")
                    (maybe-authenticate!
                     (lambda ()
                       (message-box "OK!" "Do the web-browser thing" frame)))
                    (send frame set-status-text "Authenticating ... done!")

                    (for-each (lambda (file)
                                (snorgle-file file (lambda (message)
                                                     (send frame set-status-text message))))
                              files)
                    (send frame set-status-text "Done.")

;;;                     (parameterize ((non-text-tags (list* 'photos (non-text-tags)))
;;;                                    (sign-all? #t))
;;;                       (match (flickr.photos.search #:user_id "me" #:auth_token (get-preference 'flickr:token))
;;;                              [(('photos _ ('photo (('farm farm)
;;;                                                    ('id id)
;;;                                                    ('isfamily _)
;;;                                                    ('isfriend _)
;;;                                                    ('ispublic _)
;;;                                                    ('owner owner)
;;;                                                    ('secret secret)
;;;                                                    ('server server)
;;;                                                    ('title _))) . rest))
;;;                               (send frame set-status-text "Pointing your web browser at some picture or other ...")
;;;                               (message-box "Pretend ..."
;;;                                (format "... that I'm opening http://farm~a.static.flickr.com/~a/~a_~a_t.jpg"
;;;                                        farm server id secret))
;;;                               (send frame set-status-text "")]))
                    ))))

  (instantiate
   menu-item%
   ("&Quit"
    menu
    (lambda (item event)
      (exit 0)))))

(send frame show #t)

)
