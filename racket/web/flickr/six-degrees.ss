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

(define *initial-username*
                                        ;"George V. Reilly"                    ;easy
  "ohnoimdead"                          ;hard
  )
(define *initial-nsid* (car ((sxpath '(user @ nsid *text*))
                             (flickr.people.findByUsername
                              'username     *initial-username*
                              ))))

(define *cache-file-name* "cache")
(define *cached-contacts* (make-hash-table 'equal))

(when (file-exists? *cache-file-name*)
  (with-input-from-file *cache-file-name*
    (lambda ()
      (let loop ()
        (let ((datum (read)))
          (when (not (eof-object? datum))
            (hash-table-put!
             *cached-contacts*
             (car datum)
             (cdr datum))
            (loop))))))
  (printf "Read ~a contacts from ~a~%"
          (hash-table-count *cached-contacts*)
          *cache-file-name*)
  (flush-output))

(define (get-for-real user_id)
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
    ((sxpath '(@ nsid     *text*)) contacts)))

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
                  string=?

                  ;; an idea: rather than tracking down contacts, we
                  ;; could instead track down favorite photos, and
                  ;; extract the authors from those.
                  (lambda (user_id)
                    (let ((probe (hash-table-get *cached-contacts* user_id #f)))
                      (when probe
                        (fprintf
                         (current-error-port)
                         "Woo hoo! Found ~a in cache~%"
                         user_id)
                        (flush-output (current-error-port)))
                      (when (not probe)
                        (set! probe  (get-for-real user_id))
                        (parameterize-break #f
                                            (let ((op (open-output-file *cache-file-name* 'append)))
                                              (write (cons user_id probe) op)
                                              (newline op)
                                              (close-output-port op)
                                              )))
                      probe))

                  3)))

        (if trail
            (printf "~a~%"
                    (string-join
                     (map (lambda (nsid)
                            (car ((sxpath '(person username *text*)) (flickr.people.getInfo 'user_id nsid))))
                          trail)

                     " => "))
          (printf "Bummer -- no path~%"))
        )))))

(newline)
(pretty-print (get-timings))
)
