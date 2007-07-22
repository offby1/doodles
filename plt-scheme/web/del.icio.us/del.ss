#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module del mzscheme
(require (planet "delicious.ss" ("untyped" "delicious.plt" 1 3))
         (only (lib "19.ss" "srfi")
               date->string)
         (only (lib "1.ss" "srfi")
               any
               filter)
         (lib "pretty.ss")
         (lib "trace.ss"))
(current-username "tucumcari")

;; TODO -- use proper full-blown command line parsing, so when I
;; forget to pass a password argument, I get a decent error, and not a
;; "vector-ref" error.
(current-password (vector-ref (current-command-line-arguments) 0))

(printf "~a's bookmarks were last updated on ~a~%"
        (current-username)
        (date->string (last-updated) "~Y-~m-~dT~X~z"))

;; find all items with the tag "Imported", and delete 'em!

(dump-sxml-responses? #t)
(define yow
  (lambda (post)
    (let ((url (post-url post)))
      (if (positive? (string-length url))
          (begin
            (printf "Pretending to delete ~s..." url)
            ;;(delete-post! post)
            (newline)
            (flush-output))
        (begin
          (fprintf (current-error-port)
                   "Gaa, this post has an empty URL; can't delete it~%")
          (pretty-print post)))
      )))
(trace yow)
(let ((gotten (get-posts "Imported")))
  (display "GOt these:")
  (pretty-print gotten)
  (for-each yow gotten))

)