#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --load "$0"
|#
(require (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1))
         (lib "etc.ss")
         (lib "file.ss")
         (lib "include.ss")
         (only (lib "list.ss") sort)
         (lib "match.ss")
         (only (lib "1.ss" "srfi")
               filter)
         (lib "trace.ss")
         (planet "htmlprag.ss" ("neil" "htmlprag.plt" ))
         "keys.ss")

(
 (lambda (title subject slide-mount-notation)
   (flickr.photos.setMeta
    #:auth_token (get-preference (*pref-name*))

    #:photo_id "2055192230"
    #:title title
    #:description (shtml->html
                   `(html
                     (em "") ": " ""

                     ))))
 "glitz"
 "A lovely pagoda, shimmering under the full moon"
 "E. Pagoda")
