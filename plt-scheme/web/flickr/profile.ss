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
         (only (lib "1.ss" "srfi") take)
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
(define (nsid->username nsid)
  (car ((sxpath '(person username *text*)) (flickr.people.getInfo 'user_id nsid))))
(define *username*
  ;"George V. Reilly"
  "ohnoimdead"
  )
(define user_id (car ((sxpath '(user @ nsid *text*))
                      (flickr.people.findByUsername
                       'username     *username*
                       ))))
(define (bfs-compare . args)
  (apply string=? args))
;;(trace bfs-compare)
(let ((trail
       (bfs user_id
            "20825469@N00"              ; yours truly
            bfs-compare
            (lambda (user_id)
              (let ((url
                     (string->url (format "http://www.flickr.com/people/~a" user_id))
                     ))
                (printf "profile of ~a can be found at ~s~%" (nsid->username user_id) (url->string url))
                (let* ((profile-page  (html->shtml (get-pure-port url)))
                       (strongs ((sxpath '(// strong *text*)) profile-page)))
                  (pretty-print (take strongs (min 2 (length strongs)))))

                (let* ((contacts  ((sxpath '(// (contact (@ username)))) (flickr.contacts.getPublicList 'user_id user_id)))
                       (usernames ((sxpath '(@ username *text*)) contacts)))
                  ((sxpath '(@ nsid     *text*)) contacts))))

            2)))
  (if trail
      (display
       (string-join
        trail
        " => "))
    (printf "Bummer -- no path~%")))
)