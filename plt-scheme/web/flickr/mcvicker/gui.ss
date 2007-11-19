#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mred -M errortrace --no-init-file --mute-banner --version --require "$0"
|#
(module gui mzscheme
(require (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1))
         (lib "class.ss")
         (lib "etc.ss")
         (lib "file.ss")
         (lib "match.ss")
         (lib "mred.ss" "mred")
         (only (lib "1.ss" "srfi") filter)
         "auth.ss"
         "get-all.ss"
         "read-csvs.ss")

(define frame
  (new
   (class frame% (augment on-close) (define (on-close) (printf "Later!~%")
                                      (exit 0)) (super-new))
   (label "Flickr Thingy")))

(send frame create-status-line)

(define hpane (new horizontal-pane% (parent frame)))

(define csv-panel (new panel% (parent hpane) (style '(border))))

(define open-button
  (new button% (parent csv-panel) (label "Read dem CSV files")
       (callback (lambda (item event)
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

                       (for-each
                        (lambda (file)
                          (snorgle-file
                           file
                           (lambda (message)
                             (send frame set-status-text message))))
                        files)))))))

(define downloaded-panel (new panel% (parent hpane) (style '(border))))

(define *photos-by-title* (make-hash-table 'equal))

(define download-button
  (new button% (parent downloaded-panel) (label "Snarf photo data from flickr")
       (callback
        (lambda (item event)
          (send frame set-status-text "Authenticating ...")
          (maybe-authenticate!
           (lambda ()
             (message-box
              "..."
              "Do whatever your web browser tells you, then click the OK button"
              frame)))

          (send frame set-status-text "Waiting for the first page from flickr ...")

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

                          #:per_page "25" ;; smaller numbers make
                          ;; the GUI somewhat more responsive,
                          ;; since it freezes while downloading
                          ;; the page

                          #:auth_token (get-preference 'flickr:token))))

            (for-each
             (lambda (photo)
               (match
                photo
                [('photo
                  (('farm _)
                   ('id _)
                   ('isfamily _)
                   ('isfriend _)
                   ('ispublic _)
                   ('owner _)
                   ('secret _)
                   ('server _)
                   ('title title)))
                 (hash-table-put! *photos-by-title* title photo)]))
             photos)
            (send frame set-status-text (format "Downloaded ~a photos" (length photos))))))))

(define joined-panel (new panel% (parent hpane) (style '(border))))

(define join-button
  (new button% (parent joined-panel) (label "glue the two bits together")
       (callback
        (lambda (item event)
          (let ((joined
                 (filter
                  (lambda (x) x)
                  (hash-table-map
                   *photos-by-title*
                   (lambda (title photo)
                     (let* ((sans-leading-j
                             (regexp-replace #rx"^[jJ]" title ""))
                            (as-number (read (open-input-string sans-leading-j))))
                       (and (integer? as-number)
                            (hash-table-get *data-by-number* as-number #f))))))))

            (message-box "Joined" (format "~a" joined) #f))))))

(let* ((mb (instantiate menu-bar% (frame)))
       (menu (instantiate menu% ("&File" mb))))
  (instantiate
   menu-item%
   ("&Quit"
    menu
    (lambda (item event)
      (exit 0)))))

(send frame show #t)

)
