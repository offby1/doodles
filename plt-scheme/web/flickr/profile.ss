#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module profile mzscheme
(require (lib "trace.ss")
         (only (lib "url.ss" "net")
               make-url
               make-path/param
               get-pure-port
               string->url
               url->string)
         (only (lib "13.ss" "srfi") string-join)
         (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
               sxpath)
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (only (lib "pretty.ss")
               pretty-display
               pretty-print)
         (only (planet "bfs.ss" ("offby1" "offby1.plt") ) bfs)
         "flickr.ss")
(define *username* "George V. Reilly")
(define user_id (car ((sxpath '(user @ nsid *text*))
                      (flickr.people.findByUsername
                       'username     *username*
                       ))))
(define (bfs-compare . args)
  (apply string=? args))
;;(trace bfs-compare)
(display
 (string-join
  (bfs user_id
       "20825469@N00"                   ; yours truly
       bfs-compare
       (lambda (user_id)
         (let ((url
                (string->url (format "http://www.flickr.com/people/~a" user_id))
                ))
           (printf "profile of ~a can be found at ~s~%" user_id (url->string url))
           (let* ((profile-page  (html->shtml (get-pure-port url)))
                  (strongs ((sxpath '(// strong *text*)) profile-page)))
             (pretty-print strongs))

           (let* ((contacts  ((sxpath '(// (contact (@ username)))) (flickr.contacts.getPublicList 'user_id user_id)))
                  (usernames ((sxpath '(@ username *text*)) contacts))
                  (nsids     ((sxpath '(@ nsid     *text*)) contacts)))
             (pretty-print  contacts)
             (pretty-print (map cons nsids usernames))
             nsids)
           )))
  " => "))

)