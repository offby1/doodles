#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module six-degrees mzscheme
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
         (only (planet "bfs.ss" ("offby1" "offby1.plt") ) bfs bfs-distance)
         "flickr.ss")
(define (nsid->username nsid)
  (car ((sxpath '(person username *text*)) (flickr.people.getInfo 'user_id nsid))))
(define *initial-username*
  ;"George V. Reilly"                    ;easy
  "ohnoimdead"                         ;hard
  )
(define *initial-nsid* (car ((sxpath '(user @ nsid *text*))
                      (flickr.people.findByUsername
                       'username     *initial-username*
                       ))))
(define (bfs-compare . args)
  (apply string=? args))
;;(trace bfs-compare)

(call/ec
 (lambda (return)
   (parameterize-break
    #t
    (with-handlers
        ([exn:break?
          (lambda (x)
            (return))])
      (let ((trail
             (bfs *initial-nsid*
                  "20825469@N00"        ; yours truly
                  bfs-compare
                  (lambda (user_id)
                    (let* ((url (string->url (format "http://www.flickr.com/people/~a" user_id)))
                           (profile-page  (html->shtml (get-pure-port url)))
                           (strongs ((sxpath '(// strong *text*)) profile-page))
                           (contacts  ((sxpath '(// (contact (@ username)))) (flickr.contacts.getPublicList 'user_id user_id)))
                           (usernames ((sxpath '(@ username *text*)) contacts)))
                      (printf "Finding contacts of ~a, distance ~a from ~a~%"
                              user_id
                              (bfs-distance)
                              *initial-username*)
                      (flush-output)
                      ((sxpath '(@ nsid     *text*)) contacts))
                    )

                  3)))
        (if trail
            (printf "~a~%"
                    (string-join
                     (map nsid->username trail)
                     " => "))
          (printf "Bummer -- no path~%")))))))
(newline)
(pretty-print (get-timings))
)