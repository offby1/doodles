#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module get-one-batch mzscheme
(require (lib "kw.ss")
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
(define/kw (get-one-batch  #:key page per_page)
  (fprintf (current-error-port)
           "Getting at most ~a photos from page ~a~%"
           per_page page)

  (let loop ((privacy-filter-values (list 5 4 3 2 1))
             (accumulated '()))
    (if (null? privacy-filter-values)
        (apply append accumulated)
        (begin
          (fprintf (current-error-port)
                   "Snarfing photos with privacy_filter ~s ... "
                   (car privacy-filter-values))
          (loop
           (cdr privacy-filter-values)
           (cons
            (let ((from-one-call ((sxpath '(photos (photo)))
                                  (flickr.photos.search
                                     'user_id  *my-NSID*
                                     'page      page
                                     'per_page per_page
                                     'privacy_filter (car privacy-filter-values)
                                     'sort     "date-taken-desc"))))
              (fprintf (current-error-port)
                       "got ~a~%" (length from-one-call))
              from-one-call)
            accumulated))))))


(provide get-one-batch)
)
