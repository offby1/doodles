#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module profile mzscheme
(require (lib "url.ss" "net")
         (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
         "flickr.ss")
(define *username* "offby1")
(define user-id (flickr.people.findByUsername
                 'username     *username*
                 ))
(let ((url
       (string->url (format "http://www.flickr.com/people/~a" (car ((sxpath '(user @ nsid *text*)) user-id))))
       ))
  (printf "profile of ~a can be found at ~s~%" *username* (url->string url))
  )
)