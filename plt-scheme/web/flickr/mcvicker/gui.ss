#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mred --no-init-file --mute-banner --version --require "$0"
|#
(module gui mzscheme
(require (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1))
         (planet "htmlprag.ss" ("neil" "htmlprag.plt" ))
         (lib "class.ss")
         (lib "etc.ss")
         (lib "file.ss")
         (lib "include.ss")
         (only (lib "list.ss") sort)
         (lib "match.ss")
         (lib "mred.ss" "mred")
         (lib "pretty.ss")
         (only (lib "1.ss" "srfi")
               filter)
         (lib "trace.ss")
         "auth.ss"
         "get-all.ss"
         "keys.ss"
         "progress-bar.ss"
         "read-csvs.ss")

(include "version.ss")

(with-handlers
    ([void void])
  (file-stream-buffer-mode (current-output-port) 'line)
  (file-stream-buffer-mode (current-error-port)  'line))

(define frame (new frame% (label "Flickr Thingy")))

(define (usual-exception-handler e)
  (message-box
   "Uh oh"
   (cond
    ((exn? e)
     (exn-message e))
    ((exn:flickr? e)
     (exn:flickr-message e))
    (else
     (format "Unknown exception ~s" e))))

  (when (exn? e)
    (message-box
     "Oh, and"
     (format
      "~a"
      (let ((op (open-output-string)))
        (pretty-display
         (continuation-mark-set->context (exn-continuation-marks e))
         op)
        (get-output-string op))))))

(send frame create-status-line)

(define hpane (new horizontal-pane% (parent frame)))

(define csv-panel (new vertical-panel% (parent hpane) (style '(border))))

(define open-button
  (new button% (parent csv-panel) (label "Read dem CSV files")
       (callback (lambda (item event)
                   (with-handlers
                       ([void usual-exception-handler])
                     (let ((files (get-file-list
                                   "Pick some files to mess with"
                                   frame
                                   #f
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
          (with-handlers
              ([void usual-exception-handler])

            (send frame set-status-text "Authenticating ...")
            (maybe-authenticate!
             (lambda ()
               (message-box
                "..."
                "Do whatever your web browser tells you, then click the OK button"
                frame)))
            (send frame set-status-text "")

            (let ((progress-bar
                   (new pb%
                        (label "Progress!")
                        (parent frame)
                        (work-to-do 1) ;we'll set this to the correct
                                        ;value once we know what it is.
                        (worker-proc
                         (lambda (pb)
                           (with-handlers
                               ([void usual-exception-handler])
                             (send frame set-status-text "Waiting for the first page from flickr ...")
                             (for-each-page
                              (lambda ( photos this-page-number total-pages)
                                (when (equal? 1 this-page-number)
                                  (send pb set-work-to-do! total-pages)
                                  (send frame set-status-text "Downloading from flickr ..."))
                                (send pb advance!)
                                (for-each
                                 (lambda (photo)
                                   (hash-table-put! *photos-by-title* (photo-title photo) photo))
                                 photos)
                                (send frame set-status-text
                                      (format "Downloaded ~a photos from flickr..."
                                              (hash-table-count *photos-by-title*)))
                                (yield))
                              #:user_id (*user-id*)

                              #:auth_token (get-preference (*pref-name*)*))

                             (send frame set-status-text "Finished downloading from flickr.")
                             (send download-message set-label
                                   (format
                                    "Downloaded information about ~a photos"
                                    (hash-table-count *photos-by-title*)))
                             (send pb show #f)))))))

              (send progress-bar start!)))))))

(define download-message
  (new message%
       (label "You haven't yet downloaded any photo info from flickr.")
       (parent downloaded-panel)))

(define joined-panel (new vertical-panel% (parent hpane) (style '(border))))

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

(define-struct full-info (title csv-record flickr-metadata) #f)

(define join-button
  (new button% (parent joined-panel) (label "glue the two bits together")
       (callback
        (lambda (item event)
          (with-handlers
              ([void usual-exception-handler])
            (let ((joined
                   (filter
                    (lambda (record)
                      (and record
                           (match-let (((date . granularity)
                                        (datum-mount-date (full-info-csv-record record))))
                             (and date granularity))))
                    (hash-table-map
                     *photos-by-title*
                     (lambda (title photo)
                       (let ((as-number (title->number-or-false title)))
                         (and (integer? as-number)
                          (let ((datum (hash-table-get *data-by-number* as-number #f)))
                            (and
                             datum
                             (make-full-info
                              title
                              datum
                              photo))))))))))
              (send joined-message set-label (format "~a records matched" (length joined)))
              (when (positive? (length joined))
                (let* ((sorted (sort
                                joined
                                (lambda (r1 r2)
                                  (< (string->number (datum-slide-number (full-info-csv-record r1)))
                                     (string->number (datum-slide-number (full-info-csv-record r2)))))))
                       (progress-bar
                        (new pb%
                             (label "Doin' It!")
                             (parent frame)
                             (work-to-do (length sorted))
                             (worker-proc
                              (lambda (pb)
                                (with-handlers
                                    ([void usual-exception-handler])
                                  (for-each
                                   (lambda (record)
                                     (parameterize ((sign-all? #t))
                                       (match-let (((date . granularity)
                                                    (datum-mount-date (full-info-csv-record record))))
                                         (let* ((mn (datum-mount-notation  (full-info-csv-record record)))
                                                (s  (datum-subject         (full-info-csv-record record)))
                                                (descr
                                                 `(html
                                                   ,(if (positive? (string-length mn)) (list 'em mn) "")
                                                   ,(if (and (positive? (string-length mn))
                                                             (positive? (string-length  s)))
                                                        ": " "")
                                                   ,(if (positive? (string-length s))  s       "")

                                                   )))

                                           (flickr.photos.setDates
                                            #:auth_token (get-preference (*pref-name*)*)

                                            #:photo_id (photo-id (full-info-flickr-metadata record))
                                            #:date_taken date
                                            #:date_taken_granularity granularity)
                                           (when (not (equal?  descr '(html "" "" "")))
                                             (flickr.photos.setMeta
                                              #:auth_token (get-preference (*pref-name*)*)

                                              #:photo_id  (photo-id (full-info-flickr-metadata record))
                                              #:title (full-info-title record)
                                              #:description (shtml->html descr)))

                                           (send frame set-status-text
                                                 (format
                                                  "~s: ~s: ~s"
                                                  (full-info-title record)
                                                  date
                                                  descr)))

                                         (send pb advance!)
                                         (yield))))

                                   sorted))

                                (send frame set-status-text
                                      (format
                                       "Fiddled ~a photos on flickr!!"
                                       (length sorted)))
                                (send pb show #f))))))
                  (send progress-bar start!)))))))))

(define joined-message
  (new message%
       (label "You haven't yet linked CSV records with downloaded photo info.")
       (parent joined-panel)))

(let* ((mb (instantiate menu-bar% (frame)))
       (file-menu (instantiate menu% ("&File" mb)))
       (help-menu (instantiate menu% ("&Help" mb)))
       (update-frame
        (lambda ()
          (send frame set-label
                (format "Flickr Thingy: ~a"
                        (if (ed?) "Ed" "Someone other than Ed!!")))
          )))

  (update-frame)

  (instantiate
   checkable-menu-item%
   ("&Ed (as opposed to Eric)"
    file-menu
    (lambda (item event)
      (ed? (send item is-checked?))
      (update-frame)))

   (checked (ed?)))

;;;   (instantiate
;;;    checkable-menu-item%
;;;    ("&Fail flickr calls"
;;;     file-menu
;;;     (lambda (item event)
;;;       (*flickr-fail* (send item is-checked?))))

;;;    (checked (*flickr-fail*)))

  (instantiate
   menu-item%
   ("&Quit"
    file-menu
    (lambda (item event)
      (exit 0))))
  (instantiate
   menu-item%
   ("&About"
    help-menu
    (lambda (item event)
      (message-box
       "Exciting, huh?"
       (format "This is flickr-thingy version ~a" *svnversion-string*))))))

(send frame show #t)

)