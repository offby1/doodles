#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --load "$0"
|#
(module descr-test mzscheme
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
         "keys.ss")

(
 (lambda (title subject slide-mount-notation)
   (flickr.photos.setMeta
    #:auth_token (get-preference (*pref-name*))

    #:photo_id "2055192230"
    #:title title
    #:description (sxml->html
                   `(html
                     (em ,subject) ": " ,slide-mount-notation

                     ))))
 "glitz"
 "A lovely pagoda, shimmering under the full moon"
 "E. Pagoda")
)