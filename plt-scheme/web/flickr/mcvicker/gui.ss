#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mred -M errortrace --no-init-file --mute-banner --version --require "$0"
|#
(module gui mzscheme
(require (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1))
         (lib "class.ss")
         (lib "etc.ss")
         (lib "external.ss" "browser")
         (lib "file.ss")
         (lib "match.ss")
         (lib "mred.ss" "mred")
         (lib "pretty.ss")
         "auth.ss"
         "get-all.ss"
         "read-csvs.ss")

(define my-frame%
  (class frame%
    (augment on-close)
    (define (on-close)
      (exit 0))
    (super-new)))

(define frame (instantiate my-frame% ()
                           (label "Flickr Thingy")
                           (width 1000)))
(send frame create-status-line)

(let* ((mb (instantiate menu-bar% (frame)))
       (menu (instantiate menu% ("&File" mb))))
  (instantiate menu-item%
               ("&Open..."
                menu
                (lambda (item event)
                  (with-handlers
                      ([exn:flickr?
                        (lambda (e)
                          (send frame set-status-text "")
                          (message-box "Uh oh"
                                       (exn:flickr-message e)
                                       frame))])
                    (let ((files (get-file-list
                                  "Pick some files to mess with"
                                  frame
                                  (this-expression-source-directory)
                                  #f
                                  "*.csv"
                                  '()
                                  '(("CSV" "*.csv")))))

                      (when files
                        (send frame set-status-text "Authenticating ...")
                        (maybe-authenticate!
                         (lambda ()
                           (message-box "..." "Do whatever your web browser tells you, then click the OK button" frame)))
                        (send frame set-status-text "Authenticating ... done!")

                        (for-each (lambda (file)
                                    (snorgle-file file (lambda (message)
                                                         (send frame set-status-text message))))
                                  files)

                        (parameterize ((non-text-tags (list* 'photos (non-text-tags)))
                                       (sign-all? #t))
                          (send frame set-status-text "Searching ...")
                          (let* ((photos (get-all-photos
                                          (lambda (this-page total-pages)
                                            (send
                                             frame
                                             set-status-text
                                             (format "Examining page ~a of ~a ..."
                                                     this-page
                                                     total-pages))
                                            (yield))
                                          #:user_id "10665268@N04"
                                          #:auth_token (get-preference 'flickr:token)))
                                 (op (open-output-string)))
                            (send frame set-status-text "")
                            (pretty-print photos op)
                            (message-box
                             "See what I found"
                             (get-output-string op))))))))))

  (instantiate
   menu-item%
   ("&Quit"
    menu
    (lambda (item event)
      (exit 0)))))

(send frame show #t)

)
