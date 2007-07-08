#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module profile mzscheme
(require (only (lib "url.ss" "net")
               make-url
               make-path/param
               get-pure-port
               string->url
               url->string)
         (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
               sxpath)
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (only (lib "pretty.ss")
               pretty-display
               pretty-print)
         "flickr.ss")
(define *username* "Thallium")
(define user-id (flickr.people.findByUsername
                 'username     *username*
                 ))
(let ((url
       (string->url (format "http://www.flickr.com/people/~a" (car ((sxpath '(user @ nsid *text*)) user-id))))
       ))
  (printf "profile of ~a can be found at ~s~%" *username* (url->string url))
  (let* ((profile-page  (html->shtml (get-pure-port url)))
         (strongs ((sxpath '(// strong *text*)) profile-page)))
    (pretty-print strongs)))
)