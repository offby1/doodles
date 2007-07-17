#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module del mzscheme
(require (planet "delicious.ss" ("untyped" "delicious.plt" ))
         (only (lib "19.ss" "srfi")
               add-duration
               current-time
               date->string
               make-time
               time-duration
               time-utc->date
               )
         (only (lib "1.ss" "srfi")
               any
               filter)
         (lib "pretty.ss"))
(current-username "tucumcari")
(current-password (vector-ref (current-command-line-arguments) 0))
(printf "~a's bookmarks were last updated on ~a~%"
        (current-username)
        (date->string (last-updated) "~Y-~m-~dT~X~z"))


;; find all items with the tag "Imported", and with some other tag whose name contains a colon.
;; delete 'em!


(let* ((12-hours-ago (add-duration (current-time) (make-time time-duration 0 (* 12 3600))))
       (imported-posts (get-posts "Imported"
                                  )))
  (dump-sxml-responses? #t)
  (for-each (lambda (post)
              (let ((url (post-url post)))
                (if (positive? (string-length url))
                    (begin
                      (printf "Deleting ~s..." url)
                      (delete-post! post)
                      (newline)
                      (flush-output))
                  (begin
                    (fprintf (current-error-port)
                             "Gaa, this post has an empty URL; can't delete it~%")
                    (pretty-print post)))
                ))
            imported-posts))
)