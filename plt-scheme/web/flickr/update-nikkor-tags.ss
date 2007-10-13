#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui update-nikkor-tags-tests 'verbose))"
|#
(module update-nikkor-tags mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
               sxpath)
         "flickr.ss")

;; for each of my flickr photos
;;   if it was taken with my D200
;;     find the focal length and aperture
;;     find a tag that describes that lens
;;     add that tag to the picture

(define *my-NSID* (car ((sxpath '(user @ nsid *text*))
                        (flickr.people.findByUsername
                         'username "offby1"))))

(let ((all-my-photos
       (flickr.photos.search
        'user_id  *my-NSID*
        'page      1
        )))
  (printf "Golly, here's the first page of my photos: ~a~%"
          all-my-photos))


(define update-nikkor-tags-tests

  (test-suite
   "update-nikkor-tags"
   (test-case
    "yow"
    (check-regexp-match
     #rx"bar"
     "foo"))))

(provide (all-defined))
)
