#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module get-one-batch mzscheme
(require (lib "trace.ss")
         (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
               sxpath)
         "../flickr.ss")

(define *my-NSID*
  (if #t
      "20825469@N00"
      (car ((sxpath '(user @ nsid *text*))
            (flickr.people.findByUsername
             'username "offby1")))))

;; returns a simple list of photos, not an actual page.
(define (get-one-batch page per_page auth_token)
  (fprintf (current-error-port)
           "Getting at most ~a photos from page ~a~%"
           per_page page)

  (let ((from-one-call ((sxpath '(photos (photo)))
                        (apply flickr.photos.search
                               (append (if auth_token
                                           (list 'auth_token auth_token)
                                           '())
                                       (list
                                        'user_id  *my-NSID*
                                        'page      page
                                        'per_page per_page
                                        'sort     "date-taken-desc"))))))

    (fprintf (current-error-port)
             "got ~a~%" (length from-one-call))
    from-one-call))
;;(trace get-one-batch)

(provide get-one-batch)
)
