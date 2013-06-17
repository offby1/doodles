#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme "$0"
|#

#lang mzscheme

;; This little test adds a title and a description to the photo at
;; http://www.flickr.com/photos/offby1/2055192230

(require (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1))
         (lib "etc.ss")
         (lib "file.ss")
         (lib "include.ss")
         (only (lib "list.ss") sort)
         (lib "match.ss")
         (only (lib "1.ss" "srfi")
               filter)
         (lib "trace.ss")
         (planet "html-parser.ss" ("ashinn" "html-parser.plt" 1 1))
         "keys.rkt")

(define (whop-photo title subject slide-mount-notation)
  (flickr.photos.setMeta
   #:auth_token (get-preference (*pref-name*))

   #:photo_id "2055192230"
   #:title title
   #:description (sxml->html
                  `(html
                    (em ,subject) ": " ,slide-mount-notation

                    ))))

(whop-photo "glitz"
            "A lovely pagoda, shimmering under the full moon"
            "E. Pagoda")

(displayln "OK, let's see if that worked ...")
