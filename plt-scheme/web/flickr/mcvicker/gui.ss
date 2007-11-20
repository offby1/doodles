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
         (only (lib "list.ss") sort)
         (lib "match.ss")
         (lib "mred.ss" "mred")
         (only (lib "1.ss" "srfi") filter)
         "auth.ss"
         "append-only-canvas.ss"
         "get-all.ss"
         "progress-bar.ss"
         "read-csvs.ss")

(define frame (new frame% (label "Flickr Thingy")))

(send frame create-status-line)

(define hpane (new horizontal-pane% (parent frame)))

(define csv-panel (new vertical-panel% (parent hpane) (style '(border))))

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

                       (when files
                         (for-each
                          (lambda (file)
                            (snorgle-file
                             file
                             (lambda (message)
                               (send frame set-status-text message))))
                          files)

                         (send csv-message set-label (format
                                                      "~a photo records loaded"
                                                      (hash-table-count *data-by-number*)))
                         (send frame set-status-text "Done reading CSV files."))))))))
(define csv-message
  (new message%
       (label "You haven't yet loaded any photo records from CSV files.")
       (parent csv-panel)))

(define *photos-by-title* (make-hash-table 'equal))

(define downloaded-panel (new vertical-panel% (parent hpane) (style '(border))))

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

          (let* ((download-thread #f)
                 (progress-bar
                  (new pb%
                       (label "Progress!")
                       (parent frame)
                       (work-to-do 1)   ;we'll set this to the correct
                                        ;value once we know what it is.
                       (cancel-callback
                        (lambda (button event) (kill-thread download-thread)))))
                 (photos '()))
            (set! download-thread
                  (thread
                   (lambda ()
                     (set!
                      photos
                      (get-all-photos
                       (lambda (this-page total-pages)
                         (when (equal? "1" this-page)
                           (send progress-bar set-work-to-do! (string->number total-pages))
                           (send frame set-status-text "Downloading from flickr ..."))
                         (send progress-bar advance!)
                         (yield))
                       #:user_id (if #f
                                     "10665268@N04" ;ed
                                     "20825469@N00" ;me
                                     )

                       #:per_page "25" ;; smaller numbers make
                       ;; the GUI somewhat more responsive,
                       ;; since it freezes while downloading
                       ;; the page

                       #:auth_token (get-preference 'flickr:token)))
                     (send progress-bar show #f))))

            (send progress-bar show #t)
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
            (send frame set-status-text "Finished downloading from flickr.")
            (send download-message set-label (format "Downloaded information about ~a photos" (length photos))))))))

(define download-message
  (new message%
       (label "You haven't yet downloaded any photo info from flickr.")
       (parent downloaded-panel)))

(define joined-panel (new vertical-panel% (parent hpane) (style '(border))))

(define review-window
  (new frame%
       (label "Joined records")
       (parent frame)
       (width 200)
       (height 200)))

;; e.g. "j123" => 123
;; but
;; "123" => #f
;; and
;; "jklmn" => #f
(define (title->number-or-false string)
  (match
   (regexp-match #px"^[jJ]([0-9]+)$" string)
   [(_ number-string)
    (string->number number-string)]
   [_ #f]))

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
                     (let* ((as-number (title->number-or-false title)))
                       (and (integer? as-number)
                            (hash-table-get *data-by-number* as-number #f))))))))
            (send joined-message set-label (format "~a records matched" (length joined)))
            (when (positive? (length joined))
              (for-each
               (lambda (child-victim)
                 (send review-window delete-child child-victim))
               (send review-window get-children))
              (let ((aoc (new append-only-canvas%
                              (parent review-window))))

                (for-each (lambda (record)
                            (send aoc append (format "~s~%" record)))
                          (sort
                           joined
                           (lambda (r1 r2)
                             (< (string->number (datum-slide-number r1))
                                (string->number (datum-slide-number r2))))))
                (send review-window show #t))))))))

(define joined-message
  (new message%
       (label "You haven't yet linked CSV records with downloaded photo info.")
       (parent joined-panel)))

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
